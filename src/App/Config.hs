module App.Config
  ( windowWidth
  , windowHeight
  , fogStart
  , fogEnd
  , fogColor
  , renderDistance
  , interactionDistance
  ) where

import qualified Graphics.Rendering.OpenGL.GL as GL

windowWidth :: Int
windowWidth = 1280

windowHeight :: Int
windowHeight = 720

fogStart :: Float
fogStart = 192

fogEnd :: Float
fogEnd = 320

fogColor :: GL.Color3 Float
fogColor = GL.Color3 0.5 0.65 0.85

renderDistance :: Int
renderDistance = 4

interactionDistance :: Float
interactionDistance = 10.0
