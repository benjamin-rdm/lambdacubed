module Game.WorldSource
  ( WorldSource,
    generatedTerrian,
  )
where

import Game.World
import Linear

-- Chunk size should probably be fixed at compile-time
type WorldSource = V3 Int -> V3 Int -> IO TerrainChunk

generatedTerrian :: WorldSource
generatedTerrian coord chunkSize = do
  let V3 cx cy cz = coord
      V3 sx sy sz = chunkSize
      origin = V3 (cx * sx) (cy * sy) (cz * sz)
  pure (genTerrainChunk origin chunkSize)
