module Game.WorldManager
  ( ChunkHandle (..),
    ChunkCoord,
    ChunkMap,
    ChunkManagerConfig (..),
    ChunkManagerState (..),
    ChunkManager,
    defaultChunkManagerConfig,
    initialChunkManagerState,
    chunkWorldOrigin,
    chunkCoordOf,
    getBlockAtWorld,
    setBlockAtWorld,
    loadChunk,
    unloadChunkHandle,
    updatePlayerPosition,
    getDesiredChunks,
    prioritizeChunks,
    updateChunks,
    getLoadedChunks,
    runChunkManager,
    execChunkManager,
    evalChunkManager,
    raycastBlockData,
    buildChunkAtIO,
    blockQueryFromChunkMap
  )
where

import Control.Monad (unless)
import Control.Monad.Reader
import Control.Monad.State
import Data.List (sortOn)
import Data.Map.Strict qualified as M
import Data.Vector.Storable ()
import Game.World
import Game.WorldSource
import Linear hiding (nearZero)
import qualified App.Config as C

calculateDistance :: V2 Int -> V2 Int -> Float
calculateDistance a b = sqrt $ fromIntegral $ quadrance (a - b)

chunkWorldOrigin :: V2 Int -> V3 Int
chunkWorldOrigin (V2 cx cy) = V3 (cx * sx) (cy * sy) 0
  where
    V3 sx sy _ = worldChunkSize

chunkCoordOf :: V3 Float -> V2 Int
chunkCoordOf (V3 x y _z) = V2 (floor (x / fromIntegral sx)) (floor (y / fromIntegral sy))
  where
    V3 sx sy _ = worldChunkSize

data ChunkHandle = ChunkHandle
  { chCoord :: !(V2 Int),
    chOrigin :: !(V3 Int),
    chData :: !TerrainChunk,
    chOpaque :: !ChunkMesh,
    chWater :: !ChunkMesh
  }

type ChunkCoord = V2 Int

type ChunkMap = M.Map ChunkCoord ChunkHandle

data ChunkManagerConfig = ChunkManagerConfig
  { cmcRenderDistance :: !Int,
    cmcMaxChunksPerFrame :: !Int,
    cmcWorldSource :: !WorldSource
  }

data ChunkManagerState = ChunkManagerState
  { cmsLoadedChunks :: !ChunkMap,
    cmsPlayerPos :: !(V3 Float)
  }

type ChunkManager = ReaderT ChunkManagerConfig (StateT ChunkManagerState IO)

defaultChunkManagerConfig :: ChunkManagerConfig
defaultChunkManagerConfig =
  ChunkManagerConfig
    { cmcRenderDistance = C.renderDistance,
      cmcMaxChunksPerFrame = 1,
      -- 
      -- Reduce the amount of chunks loaded by frame reduce CPU-limited frame times during world generation
      cmcWorldSource = generatedTerrian
    }

initialChunkManagerState :: ChunkManagerState
initialChunkManagerState =
  ChunkManagerState
    { cmsLoadedChunks = M.empty,
      cmsPlayerPos = V3 0 0 0
    }

distanceToPlayer :: V2 Int -> ChunkManager Float
distanceToPlayer coord = do
  playerPos <- gets cmsPlayerPos
  let playerChunk = chunkCoordOf playerPos
  pure $ calculateDistance coord playerChunk

getBlockAtWorld :: V3 Int -> ChunkManager Block
getBlockAtWorld pos = do
  chunks <- gets cmsLoadedChunks
  let cc = chunkCoordOf (fromIntegral <$> pos)
      defaultBlock = Air
  pure $ case M.lookup cc chunks of
    Nothing -> defaultBlock
    Just h -> blockAtV3 (chData h) pos

setBlockAtWorld :: V3 Int -> Block -> ChunkManager Bool
setBlockAtWorld pos newBlock = do
  chunks <- gets cmsLoadedChunks
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
          liftIO $ deleteChunkMesh (chOpaque h)
          liftIO $ deleteChunkMesh (chWater h)
          newOpaque <- liftIO $ uploadChunk verts
          newWater <- liftIO $ uploadChunk wverts
          let updatedHandle = h {chData = newChunk, chOpaque = newOpaque, chWater = newWater}
              updatedChunks = M.insert cc updatedHandle chunks
          modify $ \s -> s {cmsLoadedChunks = updatedChunks}
          pure True

buildChunkAt :: V2 Int -> ChunkManager ChunkHandle
buildChunkAt coord = do
  ws <- asks cmcWorldSource
  terrainChunk <- liftIO $ ws coord
  let origin = chunkWorldOrigin coord
      verts = buildTerrainVertices terrainChunk
      wverts = buildWaterVertices terrainChunk
  opaqueMesh <- liftIO $ uploadChunk verts
  waterMesh <- liftIO $ uploadChunk wverts
  pure $ ChunkHandle coord origin terrainChunk opaqueMesh waterMesh

buildChunkAtIO :: WorldSource -> V2 Int -> IO ChunkHandle
buildChunkAtIO ws coord = do
  let origin = chunkWorldOrigin coord
  terrainChunk <- ws coord
  let verts = buildTerrainVertices terrainChunk
      wverts = buildWaterVertices terrainChunk
  opaqueMesh <- uploadChunk verts
  waterMesh <- uploadChunk wverts
  pure $ ChunkHandle coord origin terrainChunk opaqueMesh waterMesh

loadChunk :: V2 Int -> ChunkManager ()
loadChunk coord = do
  chunks <- gets cmsLoadedChunks
  unless (M.member coord chunks) $ do
    handle <- buildChunkAt coord
    modify $ \s -> s {cmsLoadedChunks = M.insert coord handle (cmsLoadedChunks s)}

unloadChunkHandle :: ChunkHandle -> ChunkManager ()
unloadChunkHandle (ChunkHandle coord _ _ om wm) = do
  liftIO $ deleteChunkMesh om
  liftIO $ deleteChunkMesh wm
  modify $ \s -> s {cmsLoadedChunks = M.delete coord (cmsLoadedChunks s)}

updatePlayerPosition :: V3 Float -> ChunkManager ()
updatePlayerPosition pos = modify $ \s -> s {cmsPlayerPos = pos}

getDesiredChunks :: ChunkManager [V2 Int]
getDesiredChunks = do
  renderDistance <- asks cmcRenderDistance
  playerPos <- gets cmsPlayerPos
  let playerChunk = chunkCoordOf playerPos
      V2 cx cy = playerChunk
  pure [V2 (cx + dx) (cy + dy) | dx <- [-renderDistance .. renderDistance], dy <- [-renderDistance .. renderDistance]]

prioritizeChunks :: [V2 Int] -> ChunkManager [V2 Int]
prioritizeChunks coords = do
  distances <- mapM (\coord -> (,) coord <$> distanceToPlayer coord) coords
  pure $ map fst $ sortOn snd distances

updateChunks :: ChunkManager ()
updateChunks = do
  desired <- getDesiredChunks
  loaded <- gets (M.keys . cmsLoadedChunks)
  let toLoad = filter (`notElem` loaded) desired
      toUnload = filter (`notElem` desired) loaded
  prioritizedToLoad <- prioritizeChunks toLoad
  maxPerFrame <- asks cmcMaxChunksPerFrame
  mapM_ loadChunk (take maxPerFrame prioritizedToLoad)
  loadedChunks <- gets cmsLoadedChunks
  mapM_ (unloadChunkHandle . (loadedChunks M.!)) (take maxPerFrame toUnload)

getLoadedChunks :: ChunkManager ChunkMap
getLoadedChunks = gets cmsLoadedChunks

runChunkManager :: ChunkManagerConfig -> ChunkManagerState -> ChunkManager a -> IO (a, ChunkManagerState)
runChunkManager config initialState action =
  runStateT (runReaderT action config) initialState

execChunkManager :: ChunkManagerConfig -> ChunkManagerState -> ChunkManager a -> IO ChunkManagerState
execChunkManager config initialState action =
  execStateT (runReaderT action config) initialState

evalChunkManager :: ChunkManagerConfig -> ChunkManagerState -> ChunkManager a -> IO a
evalChunkManager config initialState action =
  evalStateT (runReaderT action config) initialState

raycastBlockData :: ChunkMap -> V3 Float -> V3 Float -> Float -> Maybe (V3 Int, V3 Int)
raycastBlockData cm origin dirInput maxDist
  | nearZero dirInput = Nothing
  | otherwise =
      let dir = normalize dirInput
          stepSize = 0.1 :: Float
          steps :: Int
          steps = max 1 (floor (maxDist / stepSize))
          startCell = floor <$> origin :: V3 Int
          isSolid v = let b = blockQueryFromChunkMap cm v in b /= Air && blockOpaque b
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

blockQueryFromChunkMap :: ChunkMap -> V3 Int -> Block
blockQueryFromChunkMap cm pos =
  let cc = chunkCoordOf (fromIntegral <$> pos)
   in case M.lookup cc cm of
        Nothing -> Air
        Just h -> blockAtV3 (chData h) pos

nearZero :: V3 Float -> Bool
nearZero v = quadrance v < 1e-12
