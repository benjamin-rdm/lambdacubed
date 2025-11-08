module Game.ChunkWorkers
  ( PreparedChunk (..),
    ChunkWorkers (..),
    startChunkWorkers,
    requestChunk,
    tryPopPrepared,
  )
where

import App.Config
import Control.Concurrent.Async (async, link)
import Control.Concurrent.STM
import Control.DeepSeq (force)
import Game.Direction (Direction)
import Game.World
import Game.World.Mesh
import Game.WorldSource
import Linear
import Rendering.Mesh (Mesh (..))

-- The returned chunks by a worker. We COULD split this work up into loading terrain and
-- building meshes, but this seems to be easier.
data PreparedChunk = PreparedChunk
  { pcCoord :: !(V2 Int),
    pcOrigin :: !(V3 Int),
    pcTerrain :: !TerrainChunk,
    pcMeshes :: !(Mesh TerrainVertices)
  }

data ChunkWorkers = ChunkWorkers
  { cwQ :: !(TBQueue (V2 Int)),
    cwResQ :: !(TBQueue PreparedChunk)
  }

requestChunk :: ChunkWorkers -> V2 Int -> STM ()
requestChunk ChunkWorkers {cwQ} = writeTBQueue cwQ

tryPopPrepared :: ChunkWorkers -> STM (Maybe PreparedChunk)
tryPopPrepared ChunkWorkers {cwResQ} = tryReadTBQueue cwResQ

-- TODO: Investigate possible issues of having this value misconfigured.
resultQueueSize :: Int
resultQueueSize = 128

incomingQueueSize :: Int
incomingQueueSize = 128

startChunkWorkers :: Int -> WorldSource -> (Block -> Direction -> Float) -> (Block -> Direction -> Maybe Float) -> IO ChunkWorkers
startChunkWorkers n ws texOf overlayOf = do
  q <- newTBQueueIO (fromIntegral incomingQueueSize)
  resQ <- newTBQueueIO (fromIntegral resultQueueSize)
  let worker = workerLoop q resQ ws texOf overlayOf
  mapM_ (const (async worker >>= link)) [1 .. max 1 n]
  return (ChunkWorkers q resQ)

workerLoop :: TBQueue (V2 Int) -> TBQueue PreparedChunk -> WorldSource -> (Block -> Direction -> Float) -> (Block -> Direction -> Maybe Float) -> IO ()
workerLoop coordQ resQ ws texOf overlayOf = do
  coord <- atomically $ readTBQueue coordQ
  terrain <- ws coord
  let origin = V3 (cx * sx) (cy * sy) 0
        where
          V2 cx cy = coord
          V3 sx sy _ = chunkSize

  let opaque = buildTerrainVertices texOf terrain
      water = buildWaterVertices texOf terrain
      leaves = buildLeavesVertices texOf terrain
      grass = buildOverlayVertices overlayOf terrain
      prepared = PreparedChunk coord origin terrain (Mesh opaque water leaves grass)
  -- This forces the evalation of the meshes on the worker thread
  let Mesh o w l g = pcMeshes prepared
      forceLengths = verticesLength o + verticesLength w + verticesLength l + verticesLength g
  !_ <- pure (force forceLengths)
  atomically $ writeTBQueue resQ prepared
  workerLoop coordQ resQ ws texOf overlayOf
