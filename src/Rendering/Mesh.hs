module Rendering.Mesh
  ( Mesh (..),
  )
where

data Mesh a = Mesh
  { mOpaque :: !a,
    mWater :: !a,
    mLeaves :: !a,
    mGrassOverlay :: !a
  }
  deriving (Eq, Show)
