module Game.WorldSource
  ( WorldSource,
    generatedTerrian
  )
where

import Game.World
import Linear

type WorldSource = V2 Int -> IO TerrainChunk

generatedTerrian :: WorldSource
generatedTerrian coord = pure (genTerrainChunkAtCoord coord)
