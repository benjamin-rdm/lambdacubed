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
    MonadWorld (..),
    runWorld,
  )
where

import App.Config
import Control.Concurrent.STM (STM, atomically)
import Control.Monad (filterM, replicateM, forM_)
import Control.Monad.Reader
import Control.Monad.State.Strict
import Data.Map.Strict qualified as M
import Data.Maybe (catMaybes)
import Data.Set qualified as S
import Game.ChunkWorkers (ChunkWorkers (..), PreparedChunk (..), requestChunk, tryPopPrepared)
import Game.World
import Game.World.Mesh
import Rendering.Render (MonadRender(..))
import Game.WorldSource
import Linear hiding (nearZero)
import Rendering.Mesh (Mesh (..))
import Utils.Monad
import Rendering.Buffer (deleteBuffer)

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
    cmcWorkers :: !ChunkWorkers
  }

data WorldState = WorldState
  { cmsLoadedChunks :: !ChunkMap,
    cmsPlayerPos :: !(V3 Float),
    cmsPending :: !(S.Set (V2 Int))
  }

class (Monad m) => MonadWorld m where
  loadedChunks :: m ChunkMap
  blockAtMaybe :: V3 Int -> m (Maybe Block)
  blockAt :: V3 Int -> m Block
  setBlockAtWorld :: V3 Int -> Block -> m Bool
  updatePlayerPosition :: V3 Float -> m ()
  updateChunks :: m ()
  raycastBlock :: V3 Float -> V3 Float -> Float -> m (Maybe (V3 Int, V3 Int))

type WorldT m = ReaderT WorldConfig (StateT WorldState m)

type World = WorldT IO

mkWorldConfig :: ChunkWorkers -> WorldConfig
mkWorldConfig cw =
  WorldConfig
    { cmcRenderDistance = renderDistance,
      cmcMaxChunksPerFrame = 1,
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

instance (MonadRender m) => MonadRender (WorldT m) where
  askRender = lift . lift $ askRender

{-
TODO
This MonadRender constraint is BAD. MonadWorld should just be about
game data, not need to rebuild the mesh on the GPU and require UI-specific code.
Maybe return the chunk to update here and handle at caller
-}
instance (MonadIO m, MonadRender m) => MonadWorld (WorldT m) where
  loadedChunks :: WorldT m ChunkMap
  loadedChunks = gets cmsLoadedChunks

  blockAtMaybe :: V3 Int -> WorldT m (Maybe Block)
  blockAtMaybe pos = do
    cm <- gets cmsLoadedChunks
    let cc = chunkCoordOf (fromIntegral <$> pos)
    pure (fmap (\h -> blockAtV3 (chData h) pos) (M.lookup cc cm))

  blockAt :: V3 Int -> WorldT m Block
  blockAt pos = do
    cm <- gets cmsLoadedChunks
    let cc = chunkCoordOf (fromIntegral <$> pos)
    pure $ case M.lookup cc cm of
      Nothing -> Air
      Just h -> blockAtV3 (chData h) pos

  setBlockAtWorld :: V3 Int -> Block -> WorldT m Bool
  setBlockAtWorld pos newBlock = do
    cm <- loadedChunks
    let cc = chunkCoordOf (fromIntegral <$> pos)
    case M.lookup cc cm of
      Nothing -> pure False
      Just h ->
        case setBlockInChunk (chData h) pos newBlock of
          Nothing -> pure False
          Just newChunk -> do
            texOf <- atlasTexOf
            overlayOf <- atlasOverlayOf
            let verts = buildTerrainVertices texOf newChunk
                wverts = buildWaterVertices texOf newChunk
                lverts = buildLeavesVertices texOf newChunk
                gverts = buildOverlayVertices overlayOf newChunk
            deleteGpuMeshes (chMeshes h)
            newGpuMesh <- liftIO $ uploadGpuMeshes (Mesh verts wverts lverts gverts)
            let updatedHandle = h {chData = newChunk, chMeshes = newGpuMesh}
            insertLoadedChunk cc updatedHandle
            pure True

  updatePlayerPosition :: V3 Float -> WorldT m ()
  updatePlayerPosition pos = modify $ \s -> s {cmsPlayerPos = pos}

  updateChunks :: WorldT m ()
  updateChunks = do
    desired <- getDesiredChunks
    loaded <- gets (M.keys . cmsLoadedChunks)
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
    
    forM_ (catMaybes prepared) $ \pc -> do
      deletePendingChunk (pcCoord pc)
      h <- liftIO $ finalizePrepared pc
      insertLoadedChunk (pcCoord pc) h
      
    cs <- gets cmsLoadedChunks
    mapM_ (unloadChunkHandle . (cs M.!)) (take maxPerFrame toUnload)

  raycastBlock :: V3 Float -> V3 Float -> Float -> WorldT m (Maybe (V3 Int, V3 Int))
  raycastBlock origin dirInput maxDist
    | nearZero dirInput = pure Nothing
    | otherwise = do
        let dir = normalize dirInput
            stepSize = 0.1 :: Float
            steps = max 1 (floor (maxDist / stepSize))
            startCell = floor <$> origin :: V3 Int
            go _ _ i | i > steps = pure Nothing
            go prevCell pos i = do
              let pos' = pos + stepSize *^ dir
                  cell' = floor <$> pos' :: V3 Int
              b <- blockAt cell'
              if b /= Air && blockOpaque b
                then pure (Just (cell', entryNormal prevCell cell' dir))
                else go cell' pos' (i + 1)
        go startCell origin (0 :: Int)

finalizePrepared :: PreparedChunk -> IO ChunkHandle
finalizePrepared PreparedChunk {pcCoord, pcOrigin, pcTerrain, pcMeshes} = do
  gpuMeshes <- uploadGpuMeshes pcMeshes
  return (ChunkHandle pcCoord pcOrigin pcTerrain gpuMeshes)

uploadGpuMeshes :: (MonadIO m) => Mesh TerrainVertices -> m (Mesh GpuMesh)
uploadGpuMeshes (Mesh o w l g) = liftIO $ do
  opaqueMesh <- uploadGpuMesh o
  waterMesh <- uploadGpuMesh w
  leavesMesh <- uploadGpuMesh l
  overlayMesh <- uploadGpuMesh g
  return (Mesh opaqueMesh waterMesh leavesMesh overlayMesh)

deleteGpuMeshes :: (MonadIO m) => Mesh GpuMesh -> WorldT m ()
deleteGpuMeshes (Mesh om wm lm gom) = liftIO $ do
  deleteBuffer om
  deleteBuffer wm
  deleteBuffer lm
  deleteBuffer gom

unloadChunkHandle :: (MonadIO m) => ChunkHandle -> WorldT m ()
unloadChunkHandle (ChunkHandle coord _ _ mesh) = do
  deleteGpuMeshes mesh
  modify $ \s -> s {cmsLoadedChunks = M.delete coord (cmsLoadedChunks s)}

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

runWorld :: (MonadIO m) => WorldConfig -> WorldState -> WorldT m a -> m (a, WorldState)
runWorld config initialState action =
  runStateT (runReaderT action config) initialState

entryNormal :: V3 Int -> V3 Int -> V3 Float -> V3 Int
entryNormal (V3 ax ay az) (V3 bx by bz) (V3 dx dy dz)
  | bx /= ax && abs dx >= abs dy && abs dx >= abs dz = if bx > ax then V3 (-1) 0 0 else V3 1 0 0
  | by /= ay && abs dy >= abs dz = if by > ay then V3 0 (-1) 0 else V3 0 1 0
  | bz /= az = if bz > az then V3 0 0 (-1) else V3 0 0 1
  | abs dx >= abs dy && abs dx >= abs dz = if dx > 0 then V3 (-1) 0 0 else V3 1 0 0
  | abs dy >= abs dz = if dy > 0 then V3 0 (-1) 0 else V3 0 1 0
  | otherwise = if dz > 0 then V3 0 0 (-1) else V3 0 0 1

nearZero :: V3 Float -> Bool
nearZero v = quadrance v < 1e-12
