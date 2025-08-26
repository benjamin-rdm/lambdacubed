module Game.WorldSource
  ( WorldSource,
    worldChunkSize,
    generatedTerrian,
  )
where

import Game.World
import Game.WorldConfig (worldChunkSize)
import Linear

type WorldSource = V2 Int -> IO TerrainChunk

generatedTerrian :: WorldSource
generatedTerrian coord = pure (genTerrainChunkAtCoord coord)
