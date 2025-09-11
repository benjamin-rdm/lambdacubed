module Game.Direction where

data Direction = NegX | PosX | NegY | PosY | NegZ | PosZ deriving (Eq, Ord, Show)

invertDirection :: Direction -> Direction
invertDirection td = case td of
  NegX -> PosX
  NegY -> PosY
  NegZ -> PosZ
  PosX -> NegX
  PosY -> NegY
  PosZ -> NegZ
