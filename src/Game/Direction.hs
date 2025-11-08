module Game.Direction
  ( Direction (..),
    invertDirection,
    dirOffset
  )
where

import Linear (V3 (..))

data Direction = NegX | PosX | NegY | PosY | NegZ | PosZ deriving (Eq, Ord, Show)

invertDirection :: Direction -> Direction
invertDirection td = case td of
  NegX -> PosX
  NegY -> PosY
  NegZ -> PosZ
  PosX -> NegX
  PosY -> NegY
  PosZ -> NegZ

dirOffset :: Direction -> V3 Int
dirOffset NegX = V3 (-1) 0 0
dirOffset PosX = V3 1 0 0
dirOffset NegY = V3 0 (-1) 0
dirOffset PosY = V3 0 1 0
dirOffset NegZ = V3 0 0 (-1)
dirOffset PosZ = V3 0 0 1