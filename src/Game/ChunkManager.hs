module Game.ChunkManager
  ( ChunkHandle (..),
    ChunkCoord,
    ChunkMap,
    defaultChunkSize,
    ChunkManagerConfig (..),
    ChunkManagerState (..),
    ChunkManager,
    defaultChunkManagerConfig,
    initialChunkManagerState,
    getChunkSize,
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
    RaycastConfig (..),
    RaycastState (..),
    RaycastResult (..),
    buildChunkAtIO,
    blockQueryFromChunkMap,
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

defaultChunkSize :: V3 Int
defaultChunkSize = V3 64 64 64

calculateDistance :: V3 Int -> V3 Int -> Float
calculateDistance a b = sqrt $ fromIntegral $ quadrance (a - b)

chunkWorldOriginFromCoord :: V3 Int -> V3 Int -> V3 Int
chunkWorldOriginFromCoord chunkSize (V3 cx cy cz) = V3 (cx * sx) (cy * sy) (cz * sz)
  where
    V3 sx sy sz = chunkSize

chunkCoordFromWorldPos :: V3 Int -> V3 Float -> V3 Int
chunkCoordFromWorldPos chunkSize (V3 x y z) =
  V3
    (floor (x / fromIntegral sx))
    (floor (y / fromIntegral sy))
    (floor (z / fromIntegral sz))
  where
    V3 sx sy sz = chunkSize

data ChunkHandle = ChunkHandle
  { chCoord :: !(V3 Int),
    chOrigin :: !(V3 Int),
    chData :: !TerrainChunk,
    chOpaque :: !ChunkMesh,
    chWater :: !ChunkMesh
  }

type ChunkCoord = V3 Int

type ChunkMap = M.Map ChunkCoord ChunkHandle

data ChunkManagerConfig = ChunkManagerConfig
  { cmcRenderDistance :: !Int,
    cmcMaxChunksPerFrame :: !Int,
    cmcChunkSize :: !(V3 Int),
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
      -- Reduce the amount of chunks loaded by frame reduce CPU-limited frame times during world generation
      cmcChunkSize = defaultChunkSize,
      cmcWorldSource = generatedTerrian
    }

initialChunkManagerState :: ChunkManagerState
initialChunkManagerState =
  ChunkManagerState
    { cmsLoadedChunks = M.empty,
      cmsPlayerPos = V3 0 0 0
    }

getChunkSize :: ChunkManager (V3 Int)
getChunkSize = asks cmcChunkSize

chunkWorldOrigin :: V3 Int -> V3 Int -> V3 Int
chunkWorldOrigin = chunkWorldOriginFromCoord

chunkCoordOf :: V3 Int -> V3 Float -> V3 Int
chunkCoordOf = chunkCoordFromWorldPos

distanceToPlayer :: V3 Int -> ChunkManager Float
distanceToPlayer coord = do
  playerPos <- gets cmsPlayerPos
  chunkSize <- getChunkSize
  let playerChunk = chunkCoordOf chunkSize playerPos
  pure $ calculateDistance coord playerChunk

getBlockAtWorld :: V3 Int -> ChunkManager Block
getBlockAtWorld pos = do
  chunkSize <- getChunkSize
  chunks <- gets cmsLoadedChunks
  let cc = chunkCoordOf chunkSize (fromIntegral <$> pos)
      defaultBlock = Air
  pure $ case M.lookup cc chunks of
    Nothing -> defaultBlock
    Just h -> blockAtV3 (chData h) pos

setBlockAtWorld :: V3 Int -> Block -> ChunkManager Bool
setBlockAtWorld pos newBlock = do
  chunkSize <- getChunkSize
  chunks <- gets cmsLoadedChunks
  let cc = chunkCoordOf chunkSize (fromIntegral <$> pos)
  case M.lookup cc chunks of
    Nothing -> pure False
    Just h -> do
      case setBlockInChunk (chData h) pos newBlock of
        Nothing -> pure False
        Just newChunk -> do
          -- Immediately rebuild meshes for this chunk
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

buildChunkAt :: V3 Int -> ChunkManager ChunkHandle
buildChunkAt coord = do
  chunkSize <- getChunkSize
  ws <- asks cmcWorldSource
  terrainChunk <- liftIO $ ws coord chunkSize
  let origin = chunkWorldOrigin chunkSize coord
      verts = buildTerrainVertices terrainChunk
      wverts = buildWaterVertices terrainChunk
  opaqueMesh <- liftIO $ uploadChunk verts
  waterMesh <- liftIO $ uploadChunk wverts
  pure $ ChunkHandle coord origin terrainChunk opaqueMesh waterMesh

buildChunkAtIO :: WorldSource -> V3 Int -> IO ChunkHandle
buildChunkAtIO ws coord = do
  let chunkSize = defaultChunkSize
      origin = chunkWorldOrigin chunkSize coord
  terrainChunk <- ws coord chunkSize
  let verts = buildTerrainVertices terrainChunk
      wverts = buildWaterVertices terrainChunk
  opaqueMesh <- uploadChunk verts
  waterMesh <- uploadChunk wverts
  pure $ ChunkHandle coord origin terrainChunk opaqueMesh waterMesh

loadChunk :: V3 Int -> ChunkManager ()
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

getDesiredChunks :: ChunkManager [V3 Int]
getDesiredChunks = do
  renderDistance <- asks cmcRenderDistance
  playerPos <- gets cmsPlayerPos
  chunkSize <- getChunkSize
  let playerChunk = chunkCoordOf chunkSize playerPos
      V3 cx cy cz = playerChunk
  return [V3 (cx + dx) (cy + dy) cz | dx <- [-renderDistance .. renderDistance], dy <- [-renderDistance .. renderDistance]]

prioritizeChunks :: [V3 Int] -> ChunkManager [V3 Int]
prioritizeChunks coords = do
  distances <- mapM (\coord -> (,) coord <$> distanceToPlayer coord) coords
  return $ map fst $ sortOn snd distances

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

stepI :: Float -> Int
stepI a
  | a > 0 = 1
  | a < 0 = -1
  | otherwise = 0

data RaycastConfig = RaycastConfig
  { rcMaxDistance :: !Float,
    rcMaxSteps :: !Int
  }
  deriving (Eq, Show)

defaultRaycastConfig :: RaycastConfig
defaultRaycastConfig =
  RaycastConfig
    { rcMaxDistance = 100.0,
      rcMaxSteps = 200
    }

data RaycastState = RaycastState
  { rsPosition :: !(V3 Int),
    rsTMax     :: !(V3 Float),
    rsStep     :: !(V3 Int),
    rsDelta    :: !(V3 Float)
  }
  deriving (Eq, Show)

data RaycastResult = RaycastResult
  { rrHitPosition :: !(V3 Int),
    rrFaceNormal :: !(V3 Int),
    rrDistance :: !Float
  }
  deriving (Eq, Show)

raycastBlockData :: ChunkMap -> V3 Float -> V3 Float -> Float -> Maybe (V3 Int, V3 Int)
raycastBlockData cm origin dir maxDist =
  let config = defaultRaycastConfig {rcMaxDistance = maxDist}
   in fmap
        (\result -> (rrHitPosition result, rrFaceNormal result))
        (raycastBlockDataWithConfig cm origin dir config)

raycastBlockDataWithConfig :: ChunkMap -> V3 Float -> V3 Float -> RaycastConfig -> Maybe RaycastResult
raycastBlockDataWithConfig cm origin dirInput config
  | nearZero dirInput = Nothing
  | otherwise =
      let dir = normalize dirInput
          raycastState = initializeRaycastState origin dir
          isSolid v = let b = blockQueryFromChunkMap cm v in b /= Air && blockOpaque b
       in traceRay raycastState isSolid config

initializeRaycastState :: V3 Float -> V3 Float -> RaycastState
initializeRaycastState origin dir =
  let V3 ox oy oz = origin
      V3 dx dy dz = dir
      stepV@(V3 sx sy sz) = stepI <$> dir
      voxel0@(V3 vx vy vz) = floor <$> origin :: V3 Int
      nextBoundary v s = if s > 0 then fromIntegral (v + 1) else fromIntegral v
      t a o v s = if a == 0 then (1/0) else (nextBoundary v s - o) / a
      d a = if a == 0 then (1/0) else abs (1 / a)
      tMaxV = V3 (t dx ox vx sx) (t dy oy vy sy) (t dz oz vz sz)
      deltaV = V3 (d dx) (d dy) (d dz)
   in RaycastState
        { rsPosition = voxel0,
          rsTMax     = tMaxV,
          rsStep     = stepV,
          rsDelta    = deltaV
        }

traceRay :: RaycastState -> (V3 Int -> Bool) -> RaycastConfig -> Maybe RaycastResult
traceRay s0 isSolid cfg = go (rcMaxSteps cfg) s0
  where
    go 0 _ = Nothing
    go n s
      | isSolid (rsPosition s) = Just RaycastResult { rrHitPosition = rsPosition s
                                                    , rrFaceNormal  = V3 0 0 0
                                                    , rrDistance    = 0 }
      | otherwise =
          let V3 tx ty tz = rsTMax s
              V3 sx sy sz = rsStep s
              V3 dx dy dz = rsDelta s
           in if tx < ty && tx < tz then
                let newPos  = rsPosition s + V3 sx 0 0
                    newTMax = V3 (tx + dx) ty tz
                 in advance newPos newTMax (V3 (-sx) 0 0) (tx + dx) n s
              else if ty < tz then
                let newPos  = rsPosition s + V3 0 sy 0
                    newTMax = V3 tx (ty + dy) tz
                 in advance newPos newTMax (V3 0 (-sy) 0) (ty + dy) n s
              else
                let newPos  = rsPosition s + V3 0 0 sz
                    newTMax = V3 tx ty (tz + dz)
                 in advance newPos newTMax (V3 0 0 (-sz)) (tz + dz) n s

    advance newPos newTMax normal dist n s =
      let s' = s { rsPosition = newPos, rsTMax = newTMax }
       in if dist > rcMaxDistance cfg
            then Nothing
            else if isSolid newPos
              then Just RaycastResult { rrHitPosition = newPos
                                      , rrFaceNormal  = normal
                                      , rrDistance    = dist }
              else go (n - 1) s'

blockQueryFromChunkMap :: ChunkMap -> V3 Int -> Block
blockQueryFromChunkMap cm pos =
  let cc = chunkCoordOf defaultChunkSize (fromIntegral <$> pos)
   in case M.lookup cc cm of
        Nothing -> Air
        Just h -> blockAtV3 (chData h) pos

nearZero :: V3 Float -> Bool
nearZero v = quadrance v < 1e-12
