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
import Rendering.Shader.Outline (OutlineUs)
import Rendering.Shader.Sky (SkyUs)
import Rendering.Shader.Terrain (TerrainUs)
import Rendering.Shader.Typed (ProgramU)
import Rendering.Shader.UI (UiUs)

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
  { rsTerrainP :: !(ProgramU TerrainUs),
    rsSkyP :: !(ProgramU SkyUs),
    rsSkyBuf :: !Buffer,
    rsOutlineP :: !(ProgramU OutlineUs),
    rsOutlineBuf :: !Buffer,
    rsUIP :: !(ProgramU UiUs),
    rsUITex :: !GL.TextureObject,
    rsUIBuf :: !Buffer,
    rsAtlasTex :: !GL.TextureObject,
    rsAtlasIndex :: !AtlasIndex
  }
