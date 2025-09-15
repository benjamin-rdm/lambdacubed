{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeApplications #-}

module Rendering.Shader.Outline
  ( OutlineUs,
    loadOutlineProgram
  )
where

import Rendering.Shader.AST
import Rendering.Shader.Typed
import Rendering.Shader.Utils

type OutlineUs = AppendPairs '[ '("uView", 'Mat4), '("uProj", 'Mat4) ] '[]

outlineVertexT :: ShaderT '[ '("uView", 'Mat4), '("uProj", 'Mat4)] ()
outlineVertexT = do
  aPos <- inV3 "aPos"
  uView <- uniformMat4 @"uView"
  uProj <- uniformMat4 @"uProj"
  assignGLPosition ((use uProj .*. use uView) .*. vec4 (use aPos, 1.0 :: Double))

outlineFragmentT :: ShaderT '[] ()
outlineFragmentT = do
  frag <- outV4 "FragColor"
  assignN frag (vec4 (0.0 :: Double, 0.0 :: Double, 0.0 :: Double, 1.0 :: Double))

loadOutlineProgram :: IO (ProgramU OutlineUs)
loadOutlineProgram = do
  let vsrc = toSrc (runVertexT outlineVertexT)
      fsrc = toSrc (runFragmentT outlineFragmentT)
  ProgramU <$> loadProgramFromSources vsrc fsrc
