module Game.WorldConfig
  ( worldChunkSize
  ) where

import Linear (V3(..))

worldChunkSize :: V3 Int
worldChunkSize = V3 64 64 128