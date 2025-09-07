module Rendering.Shader.Sky
  ( loadSkyProgram,
  )
where

import Graphics.Rendering.OpenGL.GL qualified as GL
import Rendering.Shader.AST
import Rendering.Shader.Typed
import Rendering.Shader.Utils

skyVertexAST :: ShaderSource
skyVertexAST = runVertexT $ do
  aPos <- inV2 "aPos"
  vY <- outF "vY"
  assignN vY (y (use aPos))
  assignGLPosition (vec4 (use aPos, vec2 (0.0 :: Double, 1.0 :: Double)))

skyFragmentAST :: ShaderSource
skyFragmentAST = runFragmentT $ do
  vY <- inF "vY"
  frag <- outV4 "FragColor"
  uTop <- uniformV3 "uTopColor"
  uHor <- uniformV3 "uHorizonColor"
  t <- localF "t" (Just (clamp01 (addF (mulF (use vY) (0.5 :: Double)) (0.5 :: Double))))
  c <- localV3 "c" (Just (mixV3 (use uHor) (use uTop) (use t)))
  assignN frag (vec4 (use c, 1.0 :: Double))

loadSkyProgram :: IO GL.Program
loadSkyProgram = do
  let vsrc = toSrc skyVertexAST
      fsrc = toSrc skyFragmentAST
  loadProgramFromSources vsrc fsrc
