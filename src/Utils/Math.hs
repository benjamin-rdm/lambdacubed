module Utils.Math
  ( toGLMatrix
  , toRad
  ) where

import Data.Foldable

import Linear
import qualified Graphics.Rendering.OpenGL.GL as GL

toRad :: Float -> Float
toRad = (*(pi/180))

m44ToList :: M44 Float -> [Float]
m44ToList = concatMap toList

toGLMatrix :: M44 Float -> IO (GL.GLmatrix Float)
toGLMatrix = GL.newMatrix GL.RowMajor . m44ToList