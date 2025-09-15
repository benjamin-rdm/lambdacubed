{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Rendering.Shader.Terrain (loadTerrainProgramWith, TerrainVertexT, TerrainFragmentT, TerrainUs, TerrainStatic, bindTerrainStatics) where

import Data.Set qualified as S
import Game.Block.Atlas (SpecialIndices (..))
import Graphics.Rendering.OpenGL.GL qualified as GL
import Rendering.Shader.AST
import Rendering.Shader.Typed
import Rendering.Shader.Utils

type TerrainVertexT = '[ '("uView", 'Mat4), '("uProj", 'Mat4)]

type TerrainFragmentT =
  '[ '("uAtlas", 'Sampler2DArray),
     '("uGrassColormap", 'Sampler2D),
     '("uFoliageColormap", 'Sampler2D),
     '("uAlphaCutoff", 'FloatT),
     '("uFogColor", 'V3),
     '("uFogStart", 'FloatT),
     '("uFogEnd", 'FloatT),
     '("uTime", 'FloatT)
   ]

type TerrainUs = AppendPairs TerrainVertexT TerrainFragmentT

type TerrainStatic = '[ '("uAtlas", 'Sampler2DArray), '("uGrassColormap", 'Sampler2D), '("uFoliageColormap", 'Sampler2D), '("uAlphaCutoff", 'FloatT)]

bindTerrainStatics :: ProgramU TerrainUs -> Int -> Int -> Int -> Float -> IO ()
bindTerrainStatics pu atlasUnit grassUnit foliageUnit alphaCut = do
  setSampler2DArray @"uAtlas" pu (GL.TextureUnit (fromIntegral atlasUnit))
  setSampler2D @"uGrassColormap" pu (GL.TextureUnit (fromIntegral grassUnit))
  setSampler2D @"uFoliageColormap" pu (GL.TextureUnit (fromIntegral foliageUnit))
  setFloat @"uAlphaCutoff" pu alphaCut

terrainVertexT :: ShaderT TerrainVertexT ()
terrainVertexT = do
  aPos <- inV3 "aPos"
  aTC <- inV3 "aTC"
  aClimate <- inV2 "aClimate"
  vTC <- outV3 "vTC"
  vClimate <- outV2 "vClimate"
  vFogDist <- outF "vFogDist"
  uView <- uniformMat4 @"uView"
  uProj <- uniformMat4 @"uProj"
  assignN vTC (use aTC)
  assignN vClimate (use aClimate)
  let posVS = use uView .*. vec4 (use aPos, 1.0 :: Double)
  assignN vFogDist (length3 (xyz posVS))
  assignGLPosition (use uProj .*. posVS)

terrainFragmentT :: SpecialIndices -> ShaderT TerrainFragmentT ()
terrainFragmentT spec = do
  vTC <- inV3 "vTC"
  vClimate <- inV2 "vClimate"
  vFogDist <- inF "vFogDist"
  frag <- outV4 "FragColor"
  uAtlas <- uniformSampler2DArray @"uAtlas"
  uGrass <- uniformSampler2D @"uGrassColormap"
  uFoliage <- uniformSampler2D @"uFoliageColormap"
  uAlphaCutoff <- uniformFloat @"uAlphaCutoff"
  uFogColor <- uniformV3 @"uFogColor"
  uFogStart <- uniformFloat @"uFogStart"
  uFogEnd <- uniformFloat @"uFogEnd"
  uTime <- uniformFloat @"uTime"

  texCoord <- localV3 "texCoord" (Just (use vTC))

  let waterBase = fromIntegral (siWaterBase spec) :: Double
      waterFrames = fromIntegral (siWaterFrames spec) :: Double
      newZ = addF waterBase (floorF (modF (mulF (use uTime) (4.0 :: Double)) waterFrames))
  ifT (z (use texCoord) .==. waterBase) $ do
    assignN texCoord (vec3 (xy (use texCoord), newZ))

  base <- localV4 "base" (Just (texture2DArray uAtlas (use texCoord)))

  t <- localF "t" (Just (clamp01 (x (use vClimate))))
  h <- localF "h" (Just (clamp01 (y (use vClimate))))
  clim <- localV2 "clim" (Just (vec2 (subF (1.0 :: Double) (use t), subF (1.0 :: Double) (mulF (use h) (use t)))))

  let applyTint tint = do
        grassTint <- localV3 "tint" (Just (rgb (texture2D tint (use clim))))
        assignN base (vec4 (rgb (use base) .*. use grassTint, a (use base)))

  let applyOverlay = do
        overlay <- localV4 "overlay" (Just (texture2DArray uAtlas (use texCoord)))
        grassIntensity <- localF "grassIntensity" (Just (x (rgb (use overlay))))
        ifT (ltF (use grassIntensity) (0.1 :: Double)) discardT
        grassTint <- localV3 "grassTint" (Just (rgb (texture2D uGrass (use clim))))
        assignN base (vec4 (rgb (use base) .*. use grassTint, 1.0 :: Double))

  -- TODO: Generate one if with "or" for different indices?
  -- Performance should not be impacted, but maybe the code would be a bit nicer.
  let grassIdxs = S.map (fromIntegral :: Int -> Double) (siTintGrassIndices spec)
  mapM_ (\idx -> ifT (z (use texCoord) .==. idx) (applyTint uGrass)) grassIdxs

  let foliageIdxs = S.map (fromIntegral :: Int -> Double) (siTintFoliageIndices spec)
  mapM_ (\idx -> ifT (z (use texCoord) .==. idx) (applyTint uFoliage)) foliageIdxs

  let overlayIdxs = S.map (fromIntegral :: Int -> Double) (siOverlayIndices spec)
  mapM_ (\idx -> ifT (z (use texCoord) .==. idx) applyOverlay) overlayIdxs

  ifT (ltF (a (use base)) (use uAlphaCutoff)) discardT

  fog <- localF "fog" (Just (clamp01 (divF (subF (use vFogDist) (use uFogStart)) (subF (use uFogEnd) (use uFogStart)))))
  rgbCol <- localV3 "rgb" (Just (mixV3 (rgb (use base)) (use uFogColor) (use fog)))
  assignN frag (vec4 (use rgbCol, a (use base)))

loadTerrainProgramWith :: SpecialIndices -> IO (ProgramU TerrainUs)
loadTerrainProgramWith spec = do
  let vsrc = toSrc (runVertexT terrainVertexT)
      fsrc = toSrc (runFragmentT (terrainFragmentT spec))
  ProgramU <$> loadProgramFromSources vsrc fsrc
