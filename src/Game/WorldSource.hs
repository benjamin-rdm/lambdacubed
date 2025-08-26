module Game.WorldSource
  ( WorldSource,
    worldChunkSize,
    generatedTerrian,
  )
where

import Game.World
import Linear
import Game.WorldConfig (worldChunkSize)

type WorldSource = V2 Int -> IO TerrainChunk

generatedTerrian :: WorldSource
generatedTerrian coord = pure (genTerrainChunkAtCoord coord)
