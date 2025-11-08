{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Rendering.Shader.UI
  ( UiVertexIs,
    UiUs,
    loadUIProgram
  )
where

import Graphics.Rendering.OpenGL.GL qualified as GL
import Rendering.Shader.AST
import Rendering.Shader.Typed

type UiVertexIs = '[ '("aPos", 'V2), '("aUV", 'V2) ]

type UiVertexUs = '[ '("uAspect", 'FloatT)]

type UiVertexOs = '[ '("vUV", 'V2) ]

type UiFragmentIs = '[ '("vUV", 'V2) ]

type UiFragmentUs = '[ '("uUiTex", 'Sampler2D)]

type UiFragmentOs = '[ '("FragColor", 'V4) ]

type UiUs = AppendPairs UiVertexUs UiFragmentUs

uiVertexT :: ShaderT UiVertexIs UiVertexOs UiVertexUs ()
uiVertexT = do
  aPos <- inV2 @"aPos"
  aUV <- inV2 @"aUV"
  uAspect <- uniformFloat @"uAspect"
  vUV <- outV2 @"vUV"
  assign vUV (use aUV)
  let invA = divF (1.0 :: Double) (use uAspect)
      pos2 = use aPos .*. vec2 (invA, 1.0 :: Double)
  assignGLPosition (vec4 (pos2, 0.0 :: Double, 1.0 :: Double))

uiFragmentT :: ShaderT UiFragmentIs UiFragmentOs UiFragmentUs ()
uiFragmentT = do
  vUV <- inV2 @"vUV"
  frag <- outV4 @"FragColor"
  uUiTex <- uniformSampler2D @"uUiTex"
  c <- localV4 "c" (Just (texture2D uUiTex (use vUV)))
  ifT (leF (a (use c)) (0.0 :: Double)) $ do
    discardT
  assign frag (use c)

loadUIProgram :: Int -> IO (ProgramU UiVertexIs UiUs)
loadUIProgram unit = do
  pu <- loadProgram uiVertexT uiFragmentT
  withProgram pu $ setSampler2D @"uUiTex" pu (GL.TextureUnit (fromIntegral unit))
  pure pu
