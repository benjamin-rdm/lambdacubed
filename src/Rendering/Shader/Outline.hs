{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeApplications #-}

module Rendering.Shader.Outline
  ( OutlineVertexIs,
    OutlineUs,
    loadOutlineProgram
  )
where

import Rendering.Shader.AST
import Rendering.Shader.Typed

type OutlineVertexIs = '[ '("aPos", 'V3) ]

type OutlineVertexUs = '[ '("uView", 'Mat4), '("uProj", 'Mat4) ]

type OutlineVertexOs = '[]

type OutlineFragmentIs = '[]

type OutlineFragmentUs = '[]

type OutlineFragmentOs = '[ '("FragColor", 'V4) ]

type OutlineUs = AppendPairs OutlineVertexUs OutlineFragmentUs

outlineVertexT :: ShaderT OutlineVertexIs OutlineVertexOs OutlineVertexUs ()
outlineVertexT = do
  aPos <- inV3 @"aPos"
  uView <- uniformMat4 @"uView"
  uProj <- uniformMat4 @"uProj"
  assignGLPosition ((use uProj .*. use uView) .*. vec4 (use aPos, 1.0 :: Double))

outlineFragmentT :: ShaderT OutlineFragmentIs OutlineFragmentOs OutlineFragmentUs ()
outlineFragmentT = do
  frag <- outV4 @"FragColor"
  assign frag (vec4 (0.0 :: Double, 0.0 :: Double, 0.0 :: Double, 1.0 :: Double))

loadOutlineProgram :: IO (ProgramU OutlineVertexIs OutlineUs)
loadOutlineProgram = do
  loadProgram outlineVertexT outlineFragmentT
