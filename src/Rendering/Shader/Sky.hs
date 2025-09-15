{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeApplications #-}
module Rendering.Shader.Sky
  ( SkyUs,
    SkyStatic,
    loadSkyProgram,
    bindSkyStatics
  )
where

import qualified Linear as L (V3 (..))
import Rendering.Shader.AST
import Rendering.Shader.Typed
import Rendering.Shader.Utils

type SkyUs = AppendPairs '[] '[ '("uTopColor", 'V3), '("uHorizonColor", 'V3) ]
type SkyStatic = SkyUs

skyVertexT :: ShaderT '[] ()
skyVertexT = do
  aPos <- inV2 "aPos"
  vY <- outF "vY"
  assignN vY (y (use aPos))
  assignGLPosition (vec4 (use aPos, vec2 (0.0 :: Double, 1.0 :: Double)))
  

skyFragmentT :: ShaderT '[ '("uTopColor", 'V3), '("uHorizonColor", 'V3) ] ()
skyFragmentT = do
  vY <- inF "vY"
  frag <- outV4 "FragColor"
  uTop <- uniformV3 @"uTopColor"
  uHor <- uniformV3 @"uHorizonColor"
  t <- localF "t" (Just (clamp01 (addF (mulF (use vY) (0.5 :: Double)) (0.5 :: Double))))
  c <- localV3 "c" (Just (mixV3 (use uHor) (use uTop) (use t)))
  assignN frag (vec4 (use c, 1.0 :: Double))
  

loadSkyProgram :: IO (ProgramU SkyUs)
loadSkyProgram = do
  let vsrc = toSrc (runVertexT skyVertexT)
      fsrc = toSrc (runFragmentT skyFragmentT)
  ProgramU <$> loadProgramFromSources vsrc fsrc
 
bindSkyStatics :: ProgramU SkyUs -> L.V3 Float -> L.V3 Float -> IO ()
bindSkyStatics pu topColor horizonColor = do
  setV3 @"uTopColor" pu topColor
  setV3 @"uHorizonColor" pu horizonColor
