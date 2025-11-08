{-# LANGUAGE DataKinds #-}

module Rendering.Render
  ( MonadRender (..),
    AtlasIndex (..),
    RenderState (..),
  )
where

import Game.Direction (Direction)
import Game.World (Block)
import Graphics.Rendering.OpenGL.GL qualified as GL
import Rendering.Buffer (Buffer)
import Rendering.Shader.Outline (OutlineUs, OutlineVertexIs)
import Rendering.Shader.Sky (SkyUs, SkyVertexIs)
import Rendering.Shader.Terrain (TerrainUs, TerrainVertexIs)
import Rendering.Shader.Typed (ProgramU)
import Rendering.Shader.UI (UiUs, UiVertexIs)

class (Monad m) => MonadRender m where
  askRender :: m RenderState

  atlasTexOf :: m (Block -> Direction -> Float)
  atlasTexOf = do
    RenderState {rsAtlasIndex = AtlasIndex {aiLayerOf}} <- askRender
    pure (\b d -> realToFrac (aiLayerOf b d))

  atlasOverlayOf :: m (Block -> Direction -> Maybe Float)
  atlasOverlayOf = do
    RenderState {rsAtlasIndex = AtlasIndex {aiOverlayOf}} <- askRender
    pure (\b d -> realToFrac <$> aiOverlayOf b d)

data AtlasIndex = AtlasIndex
  { aiLayerOf :: Block -> Direction -> Int,
    aiOverlayOf :: Block -> Direction -> Maybe Int
  }

data RenderState = RenderState
  { rsTerrainP :: !(ProgramU TerrainVertexIs TerrainUs),
    rsSkyP :: !(ProgramU SkyVertexIs SkyUs),
    rsSkyBuf :: !(Buffer SkyVertexIs),
    rsOutlineP :: !(ProgramU OutlineVertexIs OutlineUs),
    rsOutlineBuf :: !(Buffer OutlineVertexIs),
    rsUIP :: !(ProgramU UiVertexIs UiUs),
    rsUITex :: !GL.TextureObject,
    rsUIBuf :: !(Buffer UiVertexIs),
    rsAtlasTex :: !GL.TextureObject,
    rsAtlasIndex :: !AtlasIndex
  }
