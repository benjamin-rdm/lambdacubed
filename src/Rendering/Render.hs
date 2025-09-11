module Rendering.Render
  ( MonadRender(..)
  , AtlasIndex(..)
  , RenderState(..)
  )
where

import Game.Direction (Direction)
import Game.World (Block)
import qualified Graphics.Rendering.OpenGL.GL as GL

class Monad m => MonadRender m where
  askRender :: m RenderState

  atlasTexOf :: m (Block -> Direction -> Float)
  atlasTexOf = do
    RenderState{rsAtlasIndex=AtlasIndex{aiLayerOf}} <- askRender
    pure (\b d -> realToFrac (aiLayerOf b d))

  atlasOverlayOf :: m (Block -> Direction -> Maybe Float)
  atlasOverlayOf = do
    RenderState{rsAtlasIndex=AtlasIndex{aiOverlayOf}} <- askRender
    pure (\b d -> realToFrac <$> aiOverlayOf b d)

data AtlasIndex = AtlasIndex
  { aiLayerOf :: Block -> Direction -> Int
  , aiOverlayOf :: Block -> Direction -> Maybe Int
  }

data RenderState = RenderState
  { rsTerrainProg :: !GL.Program
  , rsUView :: !GL.UniformLocation
  , rsUProj :: !GL.UniformLocation
  , rsUFogColor :: !GL.UniformLocation
  , rsUFogStart :: !GL.UniformLocation
  , rsUFogEnd :: !GL.UniformLocation
  , rsUTime :: !GL.UniformLocation
  , rsSkyProg :: !GL.Program
  , rsSkyVAO :: !GL.VertexArrayObject
  , rsSkyVBO :: !GL.BufferObject
  , rsOutlineProg :: !GL.Program
  , rsOutlineVAO :: !GL.VertexArrayObject
  , rsOutlineVBO :: !GL.BufferObject
  , rsUOutlineView :: !GL.UniformLocation
  , rsUOutlineProj :: !GL.UniformLocation
  , rsUIProg :: !GL.Program
  , rsUITex :: !GL.TextureObject
  , rsUIVAO :: !GL.VertexArrayObject
  , rsUIVBO :: !GL.BufferObject
  , rsUUiTex :: !GL.UniformLocation
  , rsUUiAspect :: !GL.UniformLocation
  , rsUAlphaCutoff :: !GL.UniformLocation
  , rsAtlasTex :: !GL.TextureObject
  , rsAtlasIndex :: !AtlasIndex
  }

