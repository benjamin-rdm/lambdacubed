{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}

module Game.World.Mesh
  ( TextureLayerOf,
    GpuMesh,
    TerrainVertex (..),
    TerrainVertices,
    verticesLength,
    uploadGpuMesh,
    buildTerrainVertices,
    buildWaterVertices,
    buildLeavesVertices,
    buildOverlayVertices,
  )
where

import App.Config (chunkSize)
import Data.Vector qualified as V
import Data.Vector.Storable qualified as VS
import Data.Maybe (fromMaybe)
import Foreign.Storable (Storable (..))
import GHC.Ptr (Ptr)
import Game.Direction (Direction (..), dirOffset, invertDirection)
import Game.World (Block (..), TerrainChunk (..), blockAtV3, blockOpaque)
import Graphics.Rendering.OpenGL.GL qualified as GL
import Linear (V2 (..), V3 (..))
import Rendering.Buffer
  ( Buffer (..),
    Vertex (..),
    bufferSubDataForVertices,
    createDynamicBufferFor,
  )
import Rendering.Shader.Terrain (TerrainVertexIs)

type TextureLayerOf = Block -> Direction -> Float

type GpuMesh = Buffer TerrainVertexIs

data TerrainVertex = TerrainVertex
  { tvPosition :: !(V3 Float),
    tvUV :: !(V2 Float),
    tvLayer :: !Float,
    tvTemperature :: !Float,
    tvHumidity :: !Float
  }

type TerrainVertices = V.Vector TerrainVertex

verticesLength :: TerrainVertices -> Int
verticesLength = V.length

instance Vertex TerrainVertex where
  toFloats :: TerrainVertex -> VS.Vector Float
  toFloats TerrainVertex {tvPosition = V3 px py pz, tvUV = V2 u v, tvLayer, tvTemperature, tvHumidity} =
    VS.fromListN 8 [px, py, pz, u, v, tvLayer, tvTemperature, tvHumidity]

instance Storable TerrainVertex where
  sizeOf :: TerrainVertex -> Int
  sizeOf _ = 8 * sizeOf (undefined :: Float)
  alignment :: TerrainVertex -> Int
  alignment _ = alignment (undefined :: Float)
  peek :: Ptr TerrainVertex -> IO TerrainVertex
  peek ptr = do
    let peekF off = peekByteOff ptr off :: IO Float
    px <- peekF 0
    py <- peekF 4
    pz <- peekF 8
    u <- peekF 12
    v <- peekF 16
    layer <- peekF 20
    temp <- peekF 24
    hum <- peekF 28
    pure (TerrainVertex (V3 px py pz) (V2 u v) layer temp hum)
  poke :: Ptr TerrainVertex -> TerrainVertex -> IO ()
  poke ptr TerrainVertex {tvPosition = V3 px py pz, tvUV = V2 u v, tvLayer, tvTemperature, tvHumidity} = do
    let pokeF = pokeByteOff ptr
    pokeF 0 px
    pokeF 4 py
    pokeF 8 pz
    pokeF 12 u
    pokeF 16 v
    pokeF 20 tvLayer
    pokeF 24 tvTemperature
    pokeF 28 tvHumidity

defaultClimate :: (Fractional a) => (a, a)
defaultClimate = (0.8, 0.4)

uploadGpuMesh :: TerrainVertices -> IO GpuMesh
uploadGpuMesh verts = do
  let vertexCount = V.length verts
  buf <- createDynamicBufferFor @TerrainVertexIs vertexCount GL.StaticDraw
  count <- bufferSubDataForVertices @TerrainVertexIs buf verts
  pure buf {bufCount = count}

leavesClimate :: (Fractional a) => (a, a)
leavesClimate = (0.8, 0.4)

terrainVertex :: V3 Float -> (Float, Float) -> Float -> (Float, Float) -> TerrainVertex
terrainVertex pos (u, v) layer (t, h) =
  TerrainVertex pos (V2 u v) layer t h

quadVertices :: V3 Float -> V3 Float -> V3 Float -> V3 Float -> Float -> (Float, Float) -> [TerrainVertex]
quadVertices v1 v2 v3 v4 layer climate =
  [ terrainVertex v1 (0, 0) layer climate,
    terrainVertex v2 (1, 0) layer climate,
    terrainVertex v3 (1, 1) layer climate,
    terrainVertex v1 (0, 0) layer climate,
    terrainVertex v3 (1, 1) layer climate,
    terrainVertex v4 (0, 1) layer climate
  ]

faceCorners :: (Real a) => V3 a -> Direction -> (V3 Float, V3 Float, V3 Float, V3 Float)
faceCorners (V3 x y z) dir =
  let f a b c = realToFrac <$> V3 a b c
      p000 = f x y z
      p100 = f (x + 1) y z
      p110 = f (x + 1) (y + 1) z
      p010 = f x (y + 1) z
      p001 = f x y (z + 1)
      p101 = f (x + 1) y (z + 1)
      p111 = f (x + 1) (y + 1) (z + 1)
      p011 = f x (y + 1) (z + 1)
   in case dir of
        NegZ -> (p100, p000, p010, p110)
        PosZ -> (p001, p101, p111, p011)
        NegX -> (p001, p011, p010, p000)
        PosX -> (p111, p101, p100, p110)
        NegY -> (p101, p001, p000, p100)
        PosY -> (p011, p111, p110, p010)

allDirections :: [Direction]
allDirections = [NegX, PosX, NegY, PosY, NegZ, PosZ]

buildDirectionalVertices ::
  [Direction] ->
  TerrainChunk ->
  (Block -> Bool) ->
  (V3 Int -> Block -> Direction -> Maybe [TerrainVertex]) ->
  TerrainVertices
buildDirectionalVertices dirs chunk blockQualifies faceGenerator = do
  let (V3 sx sy sz) = chunkSize
      chunkOrigin = tOrigin chunk
      positions = [chunkOrigin + V3 lx ly lz | lz <- [0 .. sz - 1], ly <- [0 .. sy - 1], lx <- [0 .. sx - 1]]

  V.fromList $ concatMap generateFacesForPos positions
  where
    generateFacesForPos worldPos = do
      let block = blockAtV3 chunk worldPos
      if not (blockQualifies block)
        then []
        else
          concatMap
            (fromMaybe [] . faceGenerator worldPos block)
            dirs

buildTerrainVertices :: TextureLayerOf -> TerrainChunk -> TerrainVertices
buildTerrainVertices texOf chunk =
  buildDirectionalVertices allDirections chunk blockQualifies faceGenerator
  where
    faceVisible worldPos dir = not (blockOpaque (blockAtV3 chunk (worldPos + dirOffset dir)))
    blockQualifies = blockOpaque
    faceGenerator worldPos block dir
      | not (faceVisible worldPos dir) = Nothing
      | otherwise =
          let (v1, v2, v3, v4) = faceCorners worldPos dir
              layer = case (block, dir) of
                (Grass, NegX) -> texOf Grass NegZ
                (Grass, PosX) -> texOf Grass NegZ
                (Grass, NegY) -> texOf Grass NegZ
                (Grass, PosY) -> texOf Grass NegZ
                _ -> texOf block dir
           in Just (quadVertices v1 v2 v3 v4 layer defaultClimate)

waterLevelOffset :: Float
waterLevelOffset = 0.1

buildWaterVertices :: TextureLayerOf -> TerrainChunk -> TerrainVertices
buildWaterVertices texOf chunk =
  buildDirectionalVertices [PosZ] chunk blockQualifies faceGenerator
  where
    blockQualifies block = block == Water
    faceGenerator worldPos block dir
      | dir /= PosZ = Nothing
      | block /= Water = Nothing
      | blockAtV3 chunk (worldPos + dirOffset PosZ) /= Air = Nothing
      | otherwise =
          let offsetWorldPos = (realToFrac <$> worldPos) - V3 0 0 waterLevelOffset
              (v1, v2, v3, v4) = faceCorners offsetWorldPos PosZ
              (v5, v6, v7, v8) = faceCorners offsetWorldPos (invertDirection PosZ)
              layer = texOf Water PosZ
              top = quadVertices v1 v2 v3 v4 layer defaultClimate
              bottom = quadVertices v5 v6 v7 v8 layer defaultClimate
           in Just (top ++ bottom)

buildLeavesVertices :: TextureLayerOf -> TerrainChunk -> TerrainVertices
buildLeavesVertices texOf chunk =
  buildDirectionalVertices allDirections chunk blockQualifies faceGenerator
  where
    blockQualifies block = block == OakLeaves
    faceGenerator worldPos block dir
      | block /= OakLeaves = Nothing
      | otherwise =
          let (v1, v2, v3, v4) = faceCorners worldPos dir
              (v5, v6, v7, v8) = faceCorners worldPos (invertDirection dir)
              layer = texOf OakLeaves dir
              front = quadVertices v1 v2 v3 v4 layer leavesClimate
              back = quadVertices v5 v6 v7 v8 layer leavesClimate
           in Just (front ++ back)

buildOverlayVertices :: (Block -> Direction -> Maybe Float) -> TerrainChunk -> TerrainVertices
buildOverlayVertices overlayOf chunk =
  buildDirectionalVertices allDirections chunk blockQualifies faceGenerator
  where
    faceVisible worldPos dir = not (blockOpaque (blockAtV3 chunk (worldPos + dirOffset dir)))
    blockQualifies _ = True
    faceGenerator worldPos block dir =
      case overlayOf block dir of
        Just layer
          | faceVisible worldPos dir ->
              let (v1, v2, v3, v4) = faceCorners worldPos dir
               in Just (quadVertices v1 v2 v3 v4 layer defaultClimate)
        _ -> Nothing
