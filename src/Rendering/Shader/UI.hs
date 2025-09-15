{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Rendering.Shader.UI
  ( UiUs,
    loadUIProgram,
    bindUiStatics,
  )
where

import Graphics.Rendering.OpenGL.GL qualified as GL
import Rendering.Shader.AST
import Rendering.Shader.Typed
import Rendering.Shader.Utils

type UiUs = AppendPairs '[ '("uAspect", 'FloatT)] '[ '("uUiTex", 'Sampler2D)]

uiVertexT :: ShaderT '[ '("uAspect", 'FloatT)] ()
uiVertexT = do
  aPos <- inV2 "aPos"
  aUV <- inV2 "aUV"
  uAspect <- uniformFloat @"uAspect"
  vUV <- outV2 "vUV"
  assignN vUV (use aUV)
  let invA = divF (1.0 :: Double) (use uAspect)
      pos2 = use aPos .*. vec2 (invA, 1.0 :: Double)
  assignGLPosition (vec4 (pos2, 0.0 :: Double, 1.0 :: Double))

uiFragmentT :: ShaderT '[ '("uUiTex", 'Sampler2D)] ()
uiFragmentT = do
  vUV <- inV2 "vUV"
  frag <- outV4 "FragColor"
  uUiTex <- uniformSampler2D @"uUiTex"
  c <- localV4 "c" (Just (texture2D uUiTex (use vUV)))
  ifT (leF (a (use c)) (0.0 :: Double)) $ do
    discardT
  assignN frag (use c)

loadUIProgram :: IO (ProgramU UiUs)
loadUIProgram = do
  let vsrc = toSrc (runVertexT uiVertexT)
      fsrc = toSrc (runFragmentT uiFragmentT)
  ProgramU <$> loadProgramFromSources vsrc fsrc

bindUiStatics :: ProgramU UiUs -> Int -> IO ()
bindUiStatics p i = setSampler2D @"uUiTex" p (GL.TextureUnit (fromIntegral i))
