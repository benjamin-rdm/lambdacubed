module Game.WorldManager
  ( ChunkHandle (..),
    ChunkCoord,
    ChunkMap,
    WorldConfig (..),
    WorldState (..),
    WorldT,
    World,
    mkWorldConfig,
    initialWorldState,
    chunkWorldOrigin,
    chunkCoordOf,
    getBlockAtWorld,
    setBlockAtWorld,
    updatePlayerPosition,
    prioritizeChunks,
    updateChunks,
    runWorld,
    raycastBlockData,
    queryBlock,
    queryBlockMaybe,
    queryBlockWM,
    loadedChunks,
    MonadWorld (..),
    getsWorldM,
  )
where

import App.Config
import Control.Concurrent.STM (STM, atomically)
import Control.Monad (filterM, replicateM)
import Control.Monad.Reader
import Control.Monad.State.Strict
import Data.Map.Strict qualified as M
import Data.Maybe (catMaybes, fromMaybe)
import Data.Set qualified as S
import Data.Vector.Storable qualified as VS
import Game.ChunkWorkers (ChunkWorkers (..), PreparedChunk (..), requestChunk, tryPopPrepared)
import Game.World
import Game.WorldSource
import Linear hiding (nearZero)
import Rendering.Mesh (Mesh (..))
import Utils.Monad

calculateDistance :: V2 Int -> V2 Int -> Float
calculateDistance a b = sqrt $ fromIntegral $ quadrance (a - b)

chunkWorldOrigin :: V2 Int -> V3 Int
chunkWorldOrigin (V2 cx cy) = V3 (cx * sx) (cy * sy) 0
  where
    V3 sx sy _ = chunkSize

chunkCoordOf :: V3 Float -> V2 Int
chunkCoordOf (V3 x y _z) = V2 (floor (x / fromIntegral sx)) (floor (y / fromIntegral sy))
  where
    V3 sx sy _ = chunkSize

data ChunkHandle = ChunkHandle
  { chCoord :: !(V2 Int),
    chOrigin :: !(V3 Int),
    chData :: !TerrainChunk,
    chMeshes :: !(Mesh GpuMesh)
  }

type ChunkCoord = V2 Int

type ChunkMap = M.Map ChunkCoord ChunkHandle

data WorldConfig = WorldConfig
  { cmcRenderDistance :: !Int,
    cmcMaxChunksPerFrame :: !Int,
    cmcWorldSource :: !WorldSource,
    -- This feels more like state, but we only need to read this
    cmcWorkers :: !ChunkWorkers
  }

data WorldState = WorldState
  { cmsLoadedChunks :: !ChunkMap,
    cmsPlayerPos :: !(V3 Float),
    cmsPending :: !(S.Set (V2 Int))
  }

class (Monad m) => MonadWorld m where
  getWorld :: m WorldState

getsWorldM :: (MonadWorld m) => (WorldState -> a) -> m a
getsWorldM f = f <$> getWorld

instance (Monad m) => MonadWorld (WorldT m) where
  getWorld :: (Monad m) => WorldT m WorldState
  getWorld = get

type WorldT m = ReaderT WorldConfig (StateT WorldState m)

type World = WorldT IO

mkWorldConfig :: ChunkWorkers -> WorldConfig
mkWorldConfig cw =
  WorldConfig
    { cmcRenderDistance = renderDistance,
      cmcMaxChunksPerFrame = 3,
      cmcWorldSource = generatedTerrian,
      cmcWorkers = cw
    }

initialWorldState :: WorldState
initialWorldState =
  WorldState
    { cmsLoadedChunks = M.empty,
      cmsPlayerPos = V3 0 0 0,
      cmsPending = S.empty
    }

distanceToPlayer :: (Monad m) => V2 Int -> WorldT m Float
distanceToPlayer coord = do
  playerPos <- gets cmsPlayerPos
  let playerChunk = chunkCoordOf playerPos
  pure $ calculateDistance coord playerChunk

getBlockAtWorld :: (Monad m) => V3 Int -> WorldT m Block
getBlockAtWorld pos = do
  chunks <- loadedChunks
  let cc = chunkCoordOf (fromIntegral <$> pos)
      defaultBlock = Air
  pure $ case M.lookup cc chunks of
    Nothing -> defaultBlock
    Just h -> blockAtV3 (chData h) pos

setBlockAtWorld :: (MonadIO m) => V3 Int -> Block -> WorldT m Bool
setBlockAtWorld pos newBlock = do
  chunks <- loadedChunks
  let cc = chunkCoordOf (fromIntegral <$> pos)
  case M.lookup cc chunks of
    Nothing -> pure False
    Just h -> do
      case setBlockInChunk (chData h) pos newBlock of
        Nothing -> pure False
        Just newChunk -> do
          -- Rebuild meshes for this chunk
          let verts = buildTerrainVertices newChunk
              wverts = buildWaterVertices newChunk
              lverts = buildLeavesVertices newChunk
              gverts = buildGrassOverlayVertices newChunk
          -- Delete old meshes
          deleteGpuMeshes (chMeshes h)
          newGpuMesh <- liftIO $ uploadGpuMeshes (Mesh verts wverts lverts gverts)
          let updatedHandle = h {chData = newChunk, chMeshes = newGpuMesh}
          insertLoadedChunk cc updatedHandle
          pure True

finalizePrepared :: PreparedChunk -> IO ChunkHandle
finalizePrepared PreparedChunk {pcCoord, pcOrigin, pcTerrain, pcMeshes} = do
  gpuMeshes <- uploadGpuMeshes pcMeshes
  return (ChunkHandle pcCoord pcOrigin pcTerrain gpuMeshes)

uploadGpuMeshes :: (MonadIO m) => Mesh (VS.Vector Float) -> m (Mesh GpuMesh)
uploadGpuMeshes (Mesh o w l g) = do
  opaqueMesh <- liftIO $ uploadGpuMesh o
  waterMesh <- liftIO $ uploadGpuMesh w
  leavesMesh <- liftIO $ uploadGpuMesh l
  overlayMesh <- liftIO $ uploadGpuMesh g
  return (Mesh opaqueMesh waterMesh leavesMesh overlayMesh)

deleteGpuMeshes :: (MonadIO m) => Mesh GpuMesh -> WorldT m ()
deleteGpuMeshes (Mesh om wm lm gom) = do
  liftIO $ deleteGpuMesh om
  liftIO $ deleteGpuMesh wm
  liftIO $ deleteGpuMesh lm
  liftIO $ deleteGpuMesh gom

unloadChunkHandle :: (MonadIO m) => ChunkHandle -> WorldT m ()
unloadChunkHandle (ChunkHandle coord _ _ mesh) = do
  deleteGpuMeshes mesh
  modify $ \s -> s {cmsLoadedChunks = M.delete coord (cmsLoadedChunks s)}

updatePlayerPosition :: (Monad m) => V3 Float -> WorldT m ()
updatePlayerPosition pos = modify $ \s -> s {cmsPlayerPos = pos}

getDesiredChunks :: (MonadIO m) => WorldT m [V2 Int]
getDesiredChunks = do
  renderDist <- asks cmcRenderDistance
  playerPos <- gets cmsPlayerPos
  let playerChunk = chunkCoordOf playerPos
      V2 cx cy = playerChunk
  pure
    [ V2 (cx + dx) (cy + dy)
      | dx <- [-renderDist .. renderDist],
        dy <- [-renderDist .. renderDist]
    ]

prioritizeChunks :: (Monad m) => [V2 Int] -> WorldT m [V2 Int]
prioritizeChunks = sortOnM distanceToPlayer

liftSTM :: (MonadIO m) => STM a -> m a
liftSTM x = liftIO (atomically x)

insertLoadedChunk :: (Monad m) => ChunkCoord -> ChunkHandle -> WorldT m ()
insertLoadedChunk v h = modify $ \s -> s {cmsLoadedChunks = M.insert v h (cmsLoadedChunks s)}

deletePendingChunk :: (Monad m) => ChunkCoord -> WorldT m ()
deletePendingChunk v = modify $ \s -> s {cmsPending = S.delete v (cmsPending s)}

isPending :: (Monad m) => ChunkCoord -> WorldT m Bool
isPending v = do
  pending <- gets cmsPending
  return (v `S.member` pending)

updateChunks :: (MonadIO m) => WorldT m ()
updateChunks = do
  desired <- getDesiredChunks
  loaded <- gets (M.keys . cmsLoadedChunks)
  {-
  We store Pending chunks here to avoid adding the same chunk
  to the queue for loading multiple times. So pending is just the queue after
  this update plus the chunks that are currently being processed by workers.
  This has some potential for optimization
  -}
  let toLoad = filter (`notElem` loaded) desired
      toUnload = filter (`notElem` desired) loaded
  prioritizedToLoad <- prioritizeChunks toLoad
  maxPerFrame <- asks cmcMaxChunksPerFrame
  cw <- asks cmcWorkers
  schedCandidates <- filterM (fmap not . isPending) prioritizedToLoad
  let scheduled = take maxPerFrame schedCandidates
  liftSTM $ mapM_ (requestChunk cw) scheduled
  modify $ \s -> s {cmsPending = S.union (cmsPending s) (S.fromList scheduled)}
  prepared <- liftSTM $ replicateM maxPerFrame (tryPopPrepared cw)
  mapM_
    ( \pc -> do
        deletePendingChunk (pcCoord pc)
        h <- liftIO $ finalizePrepared pc
        insertLoadedChunk (pcCoord pc) h
    )
    (catMaybes prepared)
  cs <- loadedChunks
  mapM_ (unloadChunkHandle . (cs M.!)) (take maxPerFrame toUnload)

runWorld :: (MonadIO m) => WorldConfig -> WorldState -> WorldT m a -> m (a, WorldState)
runWorld config initialState action =
  runStateT (runReaderT action config) initialState

raycastBlockData :: ChunkMap -> V3 Float -> V3 Float -> Float -> Maybe (V3 Int, V3 Int)
raycastBlockData cm origin dirInput maxDist
  | nearZero dirInput = Nothing
  | otherwise =
      let dir = normalize dirInput
          stepSize = 0.1 :: Float
          steps :: Int
          steps = max 1 (floor (maxDist / stepSize))
          startCell = floor <$> origin :: V3 Int
          isSolid v = let b = queryBlock cm v in b /= Air && blockOpaque b
          go :: V3 Int -> V3 Float -> Int -> Maybe (V3 Int, V3 Int)
          go _ _ i | i > steps = Nothing
          go prevCell pos i =
            let pos' = pos + stepSize *^ dir
                cell' = floor <$> pos' :: V3 Int
             in if isSolid cell'
                  then Just (cell', entryNormal prevCell cell' dir)
                  else go cell' pos' (i + 1)
       in go startCell origin (0 :: Int)

entryNormal :: V3 Int -> V3 Int -> V3 Float -> V3 Int
entryNormal (V3 ax ay az) (V3 bx by bz) (V3 dx dy dz)
  | bx /= ax && abs dx >= abs dy && abs dx >= abs dz = if bx > ax then V3 (-1) 0 0 else V3 1 0 0
  | by /= ay && abs dy >= abs dz = if by > ay then V3 0 (-1) 0 else V3 0 1 0
  | bz /= az = if bz > az then V3 0 0 (-1) else V3 0 0 1
  | abs dx >= abs dy && abs dx >= abs dz = if dx > 0 then V3 (-1) 0 0 else V3 1 0 0
  | abs dy >= abs dz = if dy > 0 then V3 0 (-1) 0 else V3 0 1 0
  | otherwise = if dz > 0 then V3 0 0 (-1) else V3 0 0 1

queryBlock :: ChunkMap -> V3 Int -> Block
queryBlock cm pos = fromMaybe Air (queryBlockMaybe cm pos)

queryBlockMaybe :: ChunkMap -> V3 Int -> Maybe Block
queryBlockMaybe cm pos =
  let cc = chunkCoordOf (fromIntegral <$> pos)
   in fmap (\h -> blockAtV3 (chData h) pos) (M.lookup cc cm)

nearZero :: V3 Float -> Bool
nearZero v = quadrance v < 1e-12

queryBlockWM :: (Monad m) => V3 Int -> WorldT m Block
queryBlockWM v = do
  chunks <- loadedChunks
  return (queryBlock chunks v)

loadedChunks :: (Monad m) => WorldT m ChunkMap
loadedChunks = gets cmsLoadedChunks