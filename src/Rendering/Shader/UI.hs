module Rendering.Shader.UI
  ( loadUIProgram,
  )
where

import Data.ByteString.Char8 qualified as BS8
import Graphics.Rendering.OpenGL.GL qualified as GL
import Rendering.Shader.AST
import Rendering.Shader.Typed
import Rendering.Shader.Utils

toSrc :: ShaderSource -> BS8.ByteString
toSrc = BS8.pack . ppr

uiVertexAST :: ShaderSource
uiVertexAST = runVertexT $ do
  aPos <- inV2 "aPos"
  aUV <- inV2 "aUV"
  uAspect <- uniformFloat "uAspect"
  vUV <- outV2 "vUV"
  assignN vUV (use aUV)
  let invA = divF (1.0 :: Double) (use uAspect)
      pos2 = use aPos .*. vec2 (invA, 1.0 :: Double)
  assignGLPosition (vec4 (pos2, 0.0 :: Double, 1.0 :: Double))

uiFragmentAST :: ShaderSource
uiFragmentAST = runFragmentT $ do
  vUV <- inV2 "vUV"
  frag <- outV4 "FragColor"
  uUiTex <- uniformSampler2D "uUiTex"
  c <- localV4 "c" (Just (texture2D uUiTex (use vUV)))
  ifT (leF (a (use c)) (0.0 :: Double)) $ do
    discardT
  assignN frag (use c)

loadUIProgram :: IO GL.Program
loadUIProgram = do
  let vsrc = toSrc uiVertexAST
      fsrc = toSrc uiFragmentAST
  loadProgramFromSources vsrc fsrc
