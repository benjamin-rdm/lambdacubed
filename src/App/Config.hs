module App.Config
  ( windowWidth,
    windowHeight,
    fogStart,
    fogEnd,
    fogColor,
    renderDistance,
    interactionDistance,
    chunkSize
  )
where

import Graphics.Rendering.OpenGL.GL qualified as GL
import Linear (V3(..))

windowWidth :: Int
windowWidth = 1280

windowHeight :: Int
windowHeight = 720

fogStart :: Float
fogStart = 192

fogEnd :: Float
fogEnd = 256

fogColor :: GL.Color3 Float
fogColor = GL.Color3 0.5 0.65 0.85

renderDistance :: Int
renderDistance = 6

interactionDistance :: Float
interactionDistance = 10.0

chunkWidth :: Int
chunkWidth = 32

worldHeight :: Int
worldHeight = 128

chunkSize :: V3 Int
chunkSize = V3 chunkWidth chunkWidth worldHeight