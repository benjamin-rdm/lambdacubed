module Rendering.Shader.Terrain
  ( loadTerrainProgram,
  )
where

import Data.ByteString.Char8 qualified as BS8
import Graphics.Rendering.OpenGL.GL qualified as GL
import Rendering.Shader.AST
import Rendering.Shader.Typed
import Rendering.Shader.Utils

toSrc :: ShaderSource -> BS8.ByteString
toSrc = BS8.pack . ppr

terrainVertexAST :: ShaderSource
terrainVertexAST = runVertexT $ do
  aPos <- inV3 "aPos"
  aTC <- inV3 "aTC"
  aClimate <- inV2 "aClimate"
  vTC <- outV3 "vTC"
  vClimate <- outV2 "vClimate"
  vFogDist <- outF "vFogDist"
  uView <- uniformMat4 "uView"
  uProj <- uniformMat4 "uProj"
  assignN vTC (use aTC)
  assignN vClimate (use aClimate)
  let posVS = use uView .*. vec4 (use aPos, 1.0 :: Double)
  assignN vFogDist (length3 (xyz posVS))
  assignGLPosition (use uProj .*. posVS)

terrainFragmentAST :: ShaderSource
terrainFragmentAST = runFragmentT $ do
  vTC <- inV3 "vTC"
  vClimate <- inV2 "vClimate"
  vFogDist <- inF "vFogDist"
  frag <- outV4 "FragColor"
  uAtlas <- uniformSampler2DArray "uAtlas"
  uGrass <- uniformSampler2D "uGrassColormap"
  uFoliage <- uniformSampler2D "uFoliageColormap"
  uAlphaCutoff <- uniformFloat "uAlphaCutoff"
  uFogColor <- uniformV3 "uFogColor"
  uFogStart <- uniformFloat "uFogStart"
  uFogEnd <- uniformFloat "uFogEnd"
  uTime <- uniformFloat "uTime"

  texCoord <- localV3 "texCoord" (Just (use vTC))

  -- These magic numbers in the indices will be resolved, when
  -- we automatically generate the shader from minecraft block data files

  -- water animation if z == 5.0
  let newZ = addF (5.0 :: Double) (floorF (modF (mulF (use uTime) (4.0 :: Double)) (16.0 :: Double)))
  ifT (z (use texCoord) .==. (5.0 :: Double)) $ do
    assignN texCoord (vec3 (xy (use texCoord), newZ))

  base <- localV4 "base" (Just (texture2DArray uAtlas (use texCoord)))

  t <- localF "t" (Just (clamp01 (x (use vClimate))))
  h <- localF "h" (Just (clamp01 (y (use vClimate))))
  clim <- localV2 "clim" (Just (vec2 (subF (1.0 :: Double) (use t), subF (1.0 :: Double) (mulF (use h) (use t)))))

  -- grass tint if z == 0.0
  ifT (z (use texCoord) .==. (0.0 :: Double)) $ do
    grassTint <- localV3 "grassTint" (Just (rgb (texture2D uGrass (use clim))))
    assignN base (vec4 (rgb (use base) .*. use grassTint, a (use base)))

  -- foliage tint if z == 23.0
  ifT (z (use texCoord) .==. (23.0 :: Double)) $ do
    foliageTint <- localV3 "foliageTint" (Just (rgb (texture2D uFoliage (use clim))))
    assignN base (vec4 (rgb (use base) .*. use foliageTint, a (use base)))

  -- grass overlay if == 4.0
  ifT (z (use texCoord) .==. (4.0 :: Double)) $ do
    overlay <- localV4 "overlay" (Just (texture2DArray uAtlas (use texCoord)))
    grassIntensity <- localF "grassIntensity" (Just (x (rgb (use overlay))))
    ifT (ltF (use grassIntensity) (0.1 :: Double)) discardT
    grassTint <- localV3 "grassTint" (Just (rgb (texture2D uGrass (use clim))))
    assignN base (vec4 (rgb (use base) .*. use grassTint, 1.0 :: Double))

  ifT (ltF (a (use base)) (use uAlphaCutoff)) discardT

  fog <- localF "fog" (Just (clamp01 (divF (subF (use vFogDist) (use uFogStart)) (subF (use uFogEnd) (use uFogStart)))))
  rgbCol <- localV3 "rgb" (Just (mixV3 (rgb (use base)) (use uFogColor) (use fog)))
  assignN frag (vec4 (use rgbCol, a (use base)))

loadTerrainProgram :: IO GL.Program
loadTerrainProgram = do
  let vsrc = toSrc terrainVertexAST
      fsrc = toSrc terrainFragmentAST
  loadProgramFromSources vsrc fsrc
