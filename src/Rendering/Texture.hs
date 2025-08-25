module Rendering.Texture
  ( createBlockTextureArray
  , loadTextureFromPng
  , setupTextureMode
  , extractSpriteFrames
  ) where

import Codec.Picture

import qualified Data.Vector.Storable as V
import qualified Graphics.Rendering.OpenGL.GL as GL
import Graphics.Rendering.OpenGL (($=))

-- Helper constants
defaultBlockTexturePaths :: [FilePath]
defaultBlockTexturePaths = [
  "resource_pack/assets/minecraft/textures/block/grass_block_top_green.png",
  "resource_pack/assets/minecraft/textures/block/gravel.png",
  "resource_pack/assets/minecraft/textures/block/stone.png",
  "resource_pack/assets/minecraft/textures/block/dirt.png",
  "resource_pack/assets/minecraft/textures/block/grass_block_side.png",
  "resource_pack/assets/minecraft/textures/block/water_still_blue.png",
  "resource_pack/assets/minecraft/textures/block/oak_log_top.png",
  "resource_pack/assets/minecraft/textures/block/oak_log.png",
  "resource_pack/assets/minecraft/textures/block/oak_leaves.png"
  ]

-- Helper functions for common patterns
loadImageFromPath :: FilePath -> IO (Either String DynamicImage)
loadImageFromPath = readPng

convertImageToRGBA :: DynamicImage -> Image PixelRGBA8
convertImageToRGBA = convertRGBA8

loadImageSafe :: FilePath -> IO (Image PixelRGBA8)
loadImageSafe path = do
  result <- loadImageFromPath path
  case result of
    Right dynImg -> pure $ convertImageToRGBA dynImg
    Left err -> fail $ "Failed to load texture " ++ path ++ ": " ++ err

setupTexture2D :: GL.TextureTarget2D -> Image PixelRGBA8 -> IO ()
setupTexture2D target img = do
  let (Image width height dat) = img
  V.unsafeWith dat $ \ptr ->
    GL.texImage2D target GL.NoProxy 0 GL.RGBA8
      (GL.TextureSize2D (fromIntegral width) (fromIntegral height)) 0
      (GL.PixelData GL.RGBA GL.UnsignedByte ptr)

setupTexture3D :: GL.TextureTarget3D -> [Image PixelRGBA8] -> IO ()
setupTexture3D target imgs = do
  case imgs of
    [] -> fail "No textures to load"
    (firstImg:_) -> do
      let width = imageWidth firstImg
          height = imageHeight firstImg
          dat = V.concat $ map (\(Image _ _ d) -> d) imgs
      V.unsafeWith dat $ \ptr ->
        GL.texImage3D target GL.NoProxy 0 GL.RGBA8
          (GL.TextureSize3D (fromIntegral width) (fromIntegral height) (fromIntegral $ length imgs)) 0
          (GL.PixelData GL.RGBA GL.UnsignedByte ptr)

configureTextureParameters :: (GL.ParameterizedTextureTarget a) => a -> IO ()
configureTextureParameters target = do
  GL.textureWrapMode target GL.S $= (GL.Repeated, GL.Repeat)
  GL.textureWrapMode target GL.T $= (GL.Repeated, GL.Repeat)
  GL.textureFilter target $= ((GL.Nearest, Nothing), GL.Nearest)

createTextureObject :: IO GL.TextureObject
createTextureObject = GL.genObjectName

bindTextureToTarget :: GL.TextureTarget3D -> GL.TextureObject -> IO ()
bindTextureToTarget target tex = GL.textureBinding target $= Just tex

blockTextureSources :: [FilePath]
blockTextureSources = defaultBlockTexturePaths

extractSpriteFrames :: Image PixelRGBA8 -> [Image PixelRGBA8]
extractSpriteFrames img@(Image width height pixels) = 
  let frameHeight = 16
  in if width == 16 && height > 16 && height `mod` 16 == 0
     then let numFrames = min 16 (height `div` frameHeight)  -- Limit to 16 frames max
          in [ Image width frameHeight (V.slice (frame * width * frameHeight * 4) (width * frameHeight * 4) pixels)
             | frame <- [0..numFrames-1] ]
     else [img]

loadTexturesFromPng :: [FilePath] -> IO ()
loadTexturesFromPng filenames = do
  results <- mapM (\filename -> do
    img <- loadImageSafe filename
    let frames = extractSpriteFrames img
    pure frames
    ) filenames

  let allTextures = concat results
  case allTextures of
    [] -> fail "No textures loaded"
    _ -> setupTexture3D GL.Texture2DArray allTextures 

loadTextureFromPng :: FilePath -> IO ()
loadTextureFromPng filename = do
  image <- loadImageSafe filename
  setupTexture2D GL.Texture2D image 

setupTextureMode :: (GL.ParameterizedTextureTarget a) => a -> IO ()
setupTextureMode = configureTextureParameters

createBlockTextureArray :: IO GL.TextureObject
createBlockTextureArray = do
  tex <- createTextureObject
  GL.activeTexture $= GL.TextureUnit 0
  GL.texture GL.Texture2DArray $= GL.Enabled
  bindTextureToTarget GL.Texture2DArray tex
  configureTextureParameters GL.Texture2DArray
  loadTexturesFromPng blockTextureSources
  GL.generateMipmap GL.Texture2DArray $= GL.Enabled
  pure tex
