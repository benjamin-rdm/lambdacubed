module Rendering.Shader.Outline
  ( loadOutlineProgram,
  )
where

import Data.ByteString.Char8 qualified as BS8
import Graphics.Rendering.OpenGL.GL qualified as GL
import Rendering.Shader.AST
import Rendering.Shader.Typed
import Rendering.Shader.Utils

toSrc :: ShaderSource -> BS8.ByteString
toSrc = BS8.pack . ppr

outlineVertexAST :: ShaderSource
outlineVertexAST = runVertexT $ do
  aPos <- inV3 "aPos"
  uView <- uniformMat4 "uView"
  uProj <- uniformMat4 "uProj"
  assignGLPosition ((use uProj .*. use uView) .*. vec4 (use aPos, 1.0 :: Double))

outlineFragmentAST :: ShaderSource
outlineFragmentAST = runFragmentT $ do
  frag <- outV4 "FragColor"
  assignN frag (vec4 (0.0 :: Double, 0.0 :: Double, 0.0 :: Double, 1.0 :: Double))

loadOutlineProgram :: IO GL.Program
loadOutlineProgram = do
  let vsrc = toSrc outlineVertexAST
      fsrc = toSrc outlineFragmentAST
  loadProgramFromSources vsrc fsrc
