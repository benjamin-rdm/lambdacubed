module Rendering.Texture
  ( loadTextureFromPng,
    setupTextureMode,
    loadTextureAtUnit,
    extractSpriteFrames,
    Image,
    PixelRGBA8,
    loadImage,
    createTextureArrayFromImages,
    withTextureUnit,
    withTexture2D,
    withTexture2DArray,
    bindTexture2DArrayAtUnit
  )
where

import Codec.Picture
import Data.Vector.Storable qualified as V
import Graphics.Rendering.OpenGL (($=))
import Graphics.Rendering.OpenGL.GL qualified as GL

convertImageToRGBA :: DynamicImage -> Image PixelRGBA8
convertImageToRGBA = convertRGBA8

loadImage :: FilePath -> IO (Image PixelRGBA8)
loadImage path = do
  result <- readPng path
  case result of
    Right dynImg -> pure $ convertImageToRGBA dynImg
    Left err -> fail $ "Failed to load texture " ++ path ++ ": " ++ err

setupTexture2D :: GL.TextureTarget2D -> Image PixelRGBA8 -> IO ()
setupTexture2D target img = do
  let (Image width height dat) = img
  V.unsafeWith dat $ \ptr ->
    GL.texImage2D
      target
      GL.NoProxy
      0
      GL.RGBA8
      (GL.TextureSize2D (fromIntegral width) (fromIntegral height))
      0
      (GL.PixelData GL.RGBA GL.UnsignedByte ptr)

setupTexture3D :: GL.TextureTarget3D -> [Image PixelRGBA8] -> IO ()
setupTexture3D target imgs = do
  case imgs of
    [] -> fail "No textures to load"
    (firstImg : _) -> do
      let width = imageWidth firstImg
          height = imageHeight firstImg
          dat = V.concat $ map (\(Image _ _ d) -> d) imgs
      V.unsafeWith dat $ \ptr ->
        GL.texImage3D
          target
          GL.NoProxy
          0
          GL.RGBA8
          (GL.TextureSize3D (fromIntegral width) (fromIntegral height) (fromIntegral $ length imgs))
          0
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

extractSpriteFrames :: Image PixelRGBA8 -> [Image PixelRGBA8]
extractSpriteFrames img@(Image width height pixels) =
  let frameHeight = 16
   in if width == 16 && height > 16 && height `mod` 16 == 0
        then
          let numFrames = min 16 (height `div` frameHeight)
           in [ Image width frameHeight (V.slice (frame * width * frameHeight * 4) (width * frameHeight * 4) pixels)
                | frame <- [0 .. numFrames - 1]
              ]
        else [img]

loadTextureFromPng :: FilePath -> IO ()
loadTextureFromPng filename = do
  image <- loadImage filename
  setupTexture2D GL.Texture2D image

setupTextureMode :: (GL.ParameterizedTextureTarget a) => a -> IO ()
setupTextureMode = configureTextureParameters

createTextureArrayFromImages :: [Image PixelRGBA8] -> IO GL.TextureObject
createTextureArrayFromImages imgs = do
  tex <- createTextureObject
  GL.activeTexture $= GL.TextureUnit 0
  GL.texture GL.Texture2DArray $= GL.Enabled
  bindTextureToTarget GL.Texture2DArray tex
  configureTextureParameters GL.Texture2DArray
  setupTexture3D GL.Texture2DArray imgs
  GL.generateMipmap GL.Texture2DArray $= GL.Enabled
  pure tex

loadTextureAtUnit :: Int -> FilePath -> IO GL.TextureObject
loadTextureAtUnit unit path = do
  tex <- createTextureObject
  withTexture2D unit tex $ do
    setupTextureMode GL.Texture2D
    loadTextureFromPng path
    GL.generateMipmap GL.Texture2D $= GL.Enabled
  GL.activeTexture $= GL.TextureUnit (fromIntegral unit)
  GL.texture GL.Texture2D $= GL.Enabled
  GL.textureBinding GL.Texture2D $= Just tex
  pure tex

withTextureUnit :: Int -> IO a -> IO a
withTextureUnit unit act = do
  prev <- GL.get GL.activeTexture
  GL.activeTexture $= GL.TextureUnit (fromIntegral unit)
  r <- act
  GL.activeTexture $= prev
  pure r

withTexture2D :: Int -> GL.TextureObject -> IO a -> IO a
withTexture2D unit tex act = withTextureUnit unit $ do
  prev <- GL.get (GL.textureBinding GL.Texture2D)
  GL.texture GL.Texture2D $= GL.Enabled
  GL.textureBinding GL.Texture2D $= Just tex
  r <- act
  GL.textureBinding GL.Texture2D $= prev
  pure r

withTexture2DArray :: Int -> GL.TextureObject -> IO a -> IO a
withTexture2DArray unit tex act = withTextureUnit unit $ do
  prev <- GL.get (GL.textureBinding GL.Texture2DArray)
  GL.texture GL.Texture2DArray $= GL.Enabled
  GL.textureBinding GL.Texture2DArray $= Just tex
  r <- act
  GL.textureBinding GL.Texture2DArray $= prev
  pure r

bindTexture2DArrayAtUnit :: Int -> GL.TextureObject -> IO ()
bindTexture2DArrayAtUnit unit tex = do
  GL.activeTexture $= GL.TextureUnit (fromIntegral unit)
  GL.texture GL.Texture2DArray $= GL.Enabled
  GL.textureBinding GL.Texture2DArray $= Just tex
