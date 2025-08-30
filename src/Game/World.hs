{-# LANGUAGE DeriveGeneric #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}

module Game.World
  ( Block (..),
    WorldGenConfig (..),
    defaultWorldGenConfig,
    blockOpaque,
    genTerrainChunk,
    genTerrainChunkAtCoord,
    buildTerrainVertices,
    buildWaterVertices,
    buildLeavesVertices,
    buildGrassOverlayVertices,
    TerrainChunk (..),
    ChunkMesh (..),
    uploadChunk,
    deleteChunkMesh,
    blockAtV3,
    setBlockInChunk,
  )
where

import Control.Monad.Reader
import Control.Monad.State (State, execState, modify)
import Data.Foldable
import Data.IntMap.Strict qualified as IM
import Data.Maybe
import Data.Vector.Storable qualified as VS
import Data.Word (Word8)
import Foreign.Marshal.Array (withArray)
import Foreign.Ptr (Ptr, castPtr, nullPtr, plusPtr)
import Foreign.Storable (Storable (..), sizeOf)
import GHC.Generics (Generic)
import Game.WorldConfig (worldChunkSize)
import Graphics.Rendering.OpenGL (($=))
import Graphics.Rendering.OpenGL.GL qualified as GL
import Linear
import Utils.Monad
import Utils.PerlinNoise (perlin2)

defaultClimate :: Fractional a => (a,a)
defaultClimate = (0.8, 0.4)

-- TODO: Make non-monadic
sample01ExplicitM :: Float -> Float -> Float -> (Float, Float) -> WorldGen Float
sample01ExplicitM baseFreq scaleX scaleY (x, y) = do
  let nx = x * baseFreq * scaleX
      ny = y * baseFreq * scaleY
  pure $ clamp01 (0.5 * (perlin2 nx ny + 1.0))

sample01AtFreqM :: Float -> (Float, Float) -> WorldGen Float
sample01AtFreqM f (x, y) = do
  sx <- scaleXM
  sy <- scaleYM
  sample01ExplicitM f sx sy (x, y)

clamp :: (Ord a) => a -> a -> a -> a
clamp val minVal maxVal = max minVal (min maxVal val)

clamp01 :: Float -> Float
clamp01 x = clamp x 0.0 1.0

isWithinChunkBounds :: V3 Int -> V3 Int -> Bool
isWithinChunkBounds (V3 lx ly lz) (V3 sx sy sz) =
  lx >= 0 && ly >= 0 && lz >= 0 && lx < sx && ly < sy && lz < sz

positionToIndex :: V3 Int -> V3 Int -> Int -> Maybe Int
positionToIndex pos chunkSize xySize =
  let V3 lx ly lz = pos
      V3 sx sy sz = chunkSize
   in if isWithinChunkBounds (V3 lx ly lz) (V3 sx sy sz)
        then Just (lx + ly * sx + lz * xySize)
        else Nothing

setBlockInChunk :: TerrainChunk -> V3 Int -> Block -> Maybe TerrainChunk
setBlockInChunk (TerrainChunk org@(V3 ox oy oz) (V3 sx sy sz) blocks) (V3 x y z) newB =
  let lx = x - ox; ly = y - oy; lz = z - oz
   in if lx < 0 || ly < 0 || lz < 0 || lx >= sx || ly >= sy || lz >= sz
        then Nothing
        else
          let i = lx + ly * sx + lz * sx * sy
              blocks' = blocks VS.// [(i, newB)]
           in Just (TerrainChunk org (V3 sx sy sz) blocks')

data WorldGenConfig = WorldGenConfig
  { wgNoiseFreq :: !Float,
    wgScaleX :: !Float,
    wgScaleY :: !Float,
    wgBaseHeight :: !Int,
    wgHeightAmplitude :: !Int,
    wgWaterLevel :: !Int,
    wgTreeDensity :: !Float
  }
  deriving (Eq, Show)

noiseFreqM :: WorldGen Float
noiseFreqM = asks wgNoiseFreq

scaleXM :: WorldGen Float
scaleXM = asks wgScaleX

scaleYM :: WorldGen Float
scaleYM = asks wgScaleY

baseHeightM :: WorldGen Int
baseHeightM = asks wgBaseHeight

heightAmplitudeM :: WorldGen Int
heightAmplitudeM = asks wgHeightAmplitude

waterLevelM :: WorldGen Int
waterLevelM = asks wgWaterLevel

treeDensityM :: WorldGen Float
treeDensityM = asks wgTreeDensity

defaultWorldGenConfig :: WorldGenConfig
defaultWorldGenConfig =
  WorldGenConfig
    { wgNoiseFreq = 0.01,
      wgScaleX = 0.8,
      wgScaleY = 1.3,
      wgBaseHeight = 20,
      wgHeightAmplitude = 50,
      wgWaterLevel = 35,
      wgTreeDensity = 0.5
    }

data TerrainChunk = TerrainChunk
  { tOrigin :: !(V3 Int),
    tSize :: !(V3 Int),
    tBlocks :: !(VS.Vector Block)
  }

type WorldGen = Reader WorldGenConfig

type WorldPos = V3 Int

treeHeightDefault :: Int
treeHeightDefault = 7

leafRadiusDefault :: Int
leafRadiusDefault = 3

genTerrainChunk :: V3 Int -> TerrainChunk
genTerrainChunk origin = runReader (genTerrainChunkM origin) defaultWorldGenConfig

genTerrainChunkAtCoord :: V2 Int -> TerrainChunk
genTerrainChunkAtCoord (V2 cx cy) =
  let V3 sx sy _ = worldChunkSize
      origin = V3 (cx * sx) (cy * sy) 0
   in genTerrainChunk origin

genTerrainChunkM :: V3 Int -> WorldGen TerrainChunk
genTerrainChunkM org@(V3 ox oy _oz) = do
  let V3 sx sy sz = worldChunkSize
      !totalSize = sx * sy * sz
      !xySize = sx * sy

  -- Precompute heights once per (x,y) to avoid recomputing for each z
  heightsList <-
    mapM
      ( \i ->
          let (!y, !x) = i `divMod` sx
           in heightAtWorldM (ox + x) (oy + y)
      )
      [0 .. xySize - 1]
  let !heights = VS.fromList heightsList

  waterLevelZ <- waterLevelM

  let classify :: Int -> Int -> Int -> Block
      classify z h wL =
        case compare z (h - 1) of
          GT -> if z <= wL then Water else Air
          EQ -> Grass
          LT -> if z >= h - 4 then Dirt else Stone

      blockAtIndex !i =
        let (!z, !r1) = i `divMod` xySize
            (!y, !x) = r1 `divMod` sx
            !h = heights VS.! (y * sx + x)
         in classify z h waterLevelZ

      !baseBlocks = VS.generate totalSize blockAtIndex
      !baseChunk = TerrainChunk org worldChunkSize baseBlocks
  generateTreesM baseChunk

generateTreesM :: TerrainChunk -> WorldGen TerrainChunk
generateTreesM chunk@(TerrainChunk org siz blocks) = do
  positions <- findTreePositionsM chunk
  let im = execState (mapM_ (treeBuilder chunk) positions) IM.empty
      newBlocks = blocks VS.// IM.toList im
  pure $ TerrainChunk org siz newBlocks

type ChunkBuilder = State (IM.IntMap Block)

treeBuilder :: TerrainChunk -> WorldPos -> ChunkBuilder ()
treeBuilder chunk base@(V3 _ _ tz) = do
  let trunk = buildTrunk treeHeightDefault base
      leaves = buildLeafSphere leafRadiusDefault (tz + treeHeightDefault) base
  mapM_ (addBlock chunk OakLog) trunk
  mapM_ (addBlock chunk OakLeaves) leaves

buildTrunk :: Int -> WorldPos -> [WorldPos]
buildTrunk h (V3 x y z0) = [V3 x y (z0 + dz) | dz <- [0 .. h - 1]]

canopyOffsets :: Int -> [WorldPos]
canopyOffsets r =
  [ V3 dx dy dz
    | dx <- [-r .. r],
      dy <- [-r .. r],
      dz <- [-r .. r],
      let d2 = dx * dx + dy * dy + dz * dz,
      d2 <= r * r,
      not (dx == 0 && dy == 0 && dz <= 0)
  ]

-- This is not quite the same shape as Minecraft, the logic could be adjusted here
buildLeafSphere :: Int -> Int -> WorldPos -> [WorldPos]
buildLeafSphere r topZ (V3 x y _) = [V3 (x + dx) (y + dy) (topZ + dz) | V3 dx dy dz <- canopyOffsets r]

treeSpacingStep :: Int
treeSpacingStep = 5

alignedRange :: Int -> Int -> Int -> [Int]
alignedRange from to step =
  let r = (from `mod` step + step) `mod` step
      first = if r == 0 then from else from + (step - r)
   in [first, first + step .. to]

chunkBoundsWithMargin :: TerrainChunk -> Int -> (Int, Int, Int, Int)
chunkBoundsWithMargin (TerrainChunk (V3 ox oy _oz) (V3 sx sy _sz) _) margin =
  let fromX = ox - margin
      toX = ox + sx - 1 + margin
      fromY = oy - margin
      toY = oy + sy - 1 + margin
   in (fromX, toX, fromY, toY)

findTreePositionsM :: TerrainChunk -> WorldGen [V3 Int]
findTreePositionsM chunk@(TerrainChunk (V3 _ox _oy oz) (V3 _sx _sy sz) _) = do
  let margin = leafRadiusDefault + 1
      (fromX, toX, fromY, toY) = chunkBoundsWithMargin chunk margin
      xs = alignedRange fromX toX treeSpacingStep
      ys = alignedRange fromY toY treeSpacingStep
  candidates <-
    mapM
      ( \(wx, wy) -> do
          h <- heightAtWorldM wx wy
          ok <- shouldPlaceTreeM wx wy
          wl <- waterLevelM
          let okZ = h > 1 && h < sz - 12 && h > wl + 1
          pure $ if ok && okZ then Just (V3 wx wy (oz + h)) else Nothing
      )
      [(x, y) | x <- xs, y <- ys]
  pure (catMaybes candidates)

shouldPlaceTreeM :: Int -> Int -> WorldGen Bool
shouldPlaceTreeM !x !y = do
  d <- treeDensityM
  let density = clamp d 0.0 1.0
      wx = fromIntegral x
      wy = fromIntegral y
  f <- noiseFreqM
  forest <- sample01AtFreqM (max 1e-3 (f * 0.5)) (wx, wy)
  key <- sample01ExplicitM 0.015 1 1 (wx * 5, wy * 5)
  let p = density * forest
  pure (key > (1.0 - p))

addBlock :: TerrainChunk -> Block -> V3 Int -> ChunkBuilder ()
addBlock (TerrainChunk origin chunkSize _) b posWorld =
  whenJust (localIndex origin chunkSize posWorld) $ \idx -> modify (IM.insertWith combine idx b)
  where
    combine new old = case (old, new) of
      -- We do not want leaves of one tree to overwrite logs of another tree
      (OakLog, _) -> old
      (_, OakLog) -> new
      _ -> new

localIndex :: V3 Int -> V3 Int -> V3 Int -> Maybe Int
localIndex origin chunkSize posWorld =
  let V3 sx sy _ = chunkSize
      xySize = sx * sy
   in positionToIndex (posWorld - origin) chunkSize xySize

heightAtWorldM :: Int -> Int -> WorldGen Int
heightAtWorldM wx wy = do
  let V3 _ _ sz = worldChunkSize
      pos = (fromIntegral wx, fromIntegral wy)
  f <- noiseFreqM
  h1 <- sample01AtFreqM f pos
  h2 <- sample01AtFreqM (f * 2.1) pos
  base <- baseHeightM
  amp <- heightAmplitudeM
  let h = 0.7 * h1 + 0.3 * h2
  pure $ clamp (base + floor (fromIntegral amp * h)) 0 sz

blockAt :: TerrainChunk -> Int -> Int -> Int -> Block
blockAt (TerrainChunk origin chunkSize bm) !x !y !z =
  let V3 ox oy oz = origin
      V3 sx sy _ = chunkSize
      !xySize = sx * sy
      pos = V3 (x - ox) (y - oy) (z - oz)
      V3 lx ly lz = pos
   in if not (isWithinChunkBounds pos chunkSize)
        then Air
        else bm VS.! (lx + ly * sx + lz * xySize)

blockAtV3 :: TerrainChunk -> V3 Int -> Block
blockAtV3 chunk (V3 x y z) = blockAt chunk x y z

dirOffset :: TextureDirection -> V3 Int
dirOffset NegX = V3 (-1) 0 0
dirOffset PosX = V3 1 0 0
dirOffset NegY = V3 0 (-1) 0
dirOffset PosY = V3 0 1 0
dirOffset NegZ = V3 0 0 (-1)
dirOffset PosZ = V3 0 0 1

data TextureDirection = NegX | PosX | NegY | PosY | NegZ | PosZ deriving (Eq, Show)

invertTextureDirection :: TextureDirection -> TextureDirection
invertTextureDirection td = case td of
  NegX -> PosX
  NegY -> PosY
  NegZ -> PosZ
  PosX -> NegX
  PosY -> NegY
  PosZ -> NegZ

data Block
  = Air
  | Dirt
  | Grass
  | Stone
  | Gravel
  | Water
  | OakLog
  | OakLeaves
  deriving (Eq, Show, Enum, Bounded, Generic)

instance Storable Block where
  sizeOf :: Block -> Int
  sizeOf _ = 1
  alignment :: Block -> Int
  alignment _ = 1
  peek :: Ptr Block -> IO Block
  peek ptr = toEnum . fromIntegral <$> peek (castPtr ptr :: Ptr Word8)
  poke :: Ptr Block -> Block -> IO ()
  poke ptr val = poke (castPtr ptr :: Ptr Word8) (fromIntegral (fromEnum val) :: Word8)

blockOpaque :: Block -> Bool
blockOpaque Air = False
blockOpaque Water = False
blockOpaque OakLeaves = False
blockOpaque _ = True

textureLayerOf :: Block -> TextureDirection -> Float
textureLayerOf Grass PosZ = 0
textureLayerOf Grass NegZ = 3
textureLayerOf Grass _ = 4
textureLayerOf Dirt _ = 3
textureLayerOf Stone _ = 2
textureLayerOf Gravel _ = 1
textureLayerOf Air _ = 3
textureLayerOf Water _ = 5
textureLayerOf OakLog PosZ = 21
textureLayerOf OakLog NegZ = 21
textureLayerOf OakLog _ = 22
textureLayerOf OakLeaves _ = 23

face :: V3 Float -> V3 Float -> V3 Float -> V3 Float -> Float -> (Float, Float) -> [Float]
face v1 v2 v3 v4 layer (t, h) =
  let tri a (u1, v1') b (u2, v2') c (u3, v3') =
        toList a
          ++ [u1, v1', layer, t, h]
          ++ toList b
          ++ [u2, v2', layer, t, h]
          ++ toList c
          ++ [u3, v3', layer, t, h]
   in tri v1 (0, 0) v2 (1, 0) v3 (1, 1)
        ++ tri v1 (0, 0) v3 (1, 1) v4 (0, 1)

faceCorners :: (Real a) => V3 a -> TextureDirection -> (V3 Float, V3 Float, V3 Float, V3 Float)
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

buildTerrainVertices :: TerrainChunk -> [Float]
buildTerrainVertices chunk@(TerrainChunk chunkOrigin (V3 sx sy sz) _) =
  concat
    [ let worldPosI = chunkOrigin + V3 lx ly lz
          block = blockAtV3 chunk worldPosI
       in if not (blockOpaque block)
            then []
            else
              concat
                [ let (v1, v2, v3, v4) = faceCorners worldPosI dir
                      layer = case (block, dir) of
                        (Grass, NegX) -> 3
                        (Grass, PosX) -> 3
                        (Grass, NegY) -> 3
                        (Grass, PosY) -> 3
                        _ -> textureLayerOf block dir
                   in face v1 v2 v3 v4 layer defaultClimate
                  | dir <- [NegX, PosX, NegY, PosY, NegZ, PosZ],
                    let neighborPos = worldPosI + dirOffset dir,
                    let neighbor = blockAtV3 chunk neighborPos,
                    not (blockOpaque neighbor)
                ]
      | lz <- [0 .. sz - 1],
        ly <- [0 .. sy - 1],
        lx <- [0 .. sx - 1]
    ]

-- Render water slightly below the surface of the block
waterLevelOffset :: Float
waterLevelOffset = 0.1

buildWaterVertices :: TerrainChunk -> [Float]
buildWaterVertices chunk@(TerrainChunk chunkOrigin (V3 sx sy sz) _) =
  concat
    [ let worldPosI = chunkOrigin + V3 lx ly lz
       in case blockAtV3 chunk worldPosI of
            Water ->
              concat
                [ let offsetWorldPos = (fromIntegral <$> worldPosI) - V3 0 0 waterLevelOffset
                      (v1, v2, v3, v4) = faceCorners offsetWorldPos dir
                      (v5, v6, v7, v8) = faceCorners offsetWorldPos (invertTextureDirection dir)
                      -- Render water from below the surface
                      layer = textureLayerOf Water dir
                   in face v1 v2 v3 v4 layer defaultClimate ++ face v5 v6 v7 v8 layer defaultClimate
                  | dir <- [PosZ], -- Do not render side vertices for water
                    let neighborPos = worldPosI + dirOffset dir,
                    blockAtV3 chunk neighborPos == Air
                ]
            _ -> []
      | lz <- [0 .. sz - 1],
        ly <- [0 .. sy - 1],
        lx <- [0 .. sx - 1]
    ]

buildLeavesVertices :: TerrainChunk -> [Float]
buildLeavesVertices chunk@(TerrainChunk chunkOrigin (V3 sx sy sz) _) =
  concat
    [ let worldPosI = chunkOrigin + V3 lx ly lz
       in case blockAtV3 chunk worldPosI of
            OakLeaves ->
              -- Render all leaf faces regardless of neighbors
              -- This creates a more dense apperance of the leaves.
              -- Also render them from both sides
              concat
                [ let (v1, v2, v3, v4) = faceCorners worldPosI dir
                      (v5, v6, v7, v8) = faceCorners worldPosI (invertTextureDirection dir)
                      layer = textureLayerOf OakLeaves dir
                   in face v1 v2 v3 v4 layer (0.8, 0.4) ++ face v5 v6 v7 v8 layer (0.8, 0.4)
                  | dir <- [NegX, PosX, NegY, PosY, NegZ, PosZ]
                ]
            _ -> []
      | lz <- [0 .. sz - 1],
        ly <- [0 .. sy - 1],
        lx <- [0 .. sx - 1]
    ]

buildGrassOverlayVertices :: TerrainChunk -> [Float]
buildGrassOverlayVertices chunk@(TerrainChunk chunkOrigin (V3 sx sy sz) _) =
  concat
    [ let worldPosI = chunkOrigin + V3 lx ly lz
          block = blockAtV3 chunk worldPosI
       in case block of
            Grass ->
              concat
                [ let (v1, v2, v3, v4) = faceCorners worldPosI dir
                   in face v1 v2 v3 v4 4 defaultClimate
                  | dir <- [NegX, PosX, NegY, PosY],
                    let neighborPos = worldPosI + dirOffset dir,
                    let neighbor = blockAtV3 chunk neighborPos,
                    not (blockOpaque neighbor)
                ]
            _ -> []
      | lz <- [0 .. sz - 1],
        ly <- [0 .. sy - 1],
        lx <- [0 .. sx - 1]
    ]

data ChunkMesh = ChunkMesh
  { cmVAO :: !GL.VertexArrayObject,
    cmVBO :: !GL.BufferObject,
    cmCount :: !Int
  }

uploadChunk :: [Float] -> IO ChunkMesh
uploadChunk verts = do
  vao <- GL.genObjectName
  vbo <- GL.genObjectName
  GL.bindVertexArrayObject $= Just vao
  GL.bindBuffer GL.ArrayBuffer $= Just vbo
  withArray verts $ \ptr -> do
    let bytes = fromIntegral (length verts * sizeOf (undefined :: Float))
    GL.bufferData GL.ArrayBuffer $= (bytes, ptr, GL.StaticDraw)

  GL.vertexAttribPointer (GL.AttribLocation 0)
    $= (GL.ToFloat, GL.VertexArrayDescriptor 3 GL.Float (8 * 4) (plusPtr nullPtr 0))
  GL.vertexAttribArray (GL.AttribLocation 0) $= GL.Enabled

  GL.vertexAttribPointer (GL.AttribLocation 1)
    $= (GL.ToFloat, GL.VertexArrayDescriptor 3 GL.Float (8 * 4) (plusPtr nullPtr (3 * 4)))
  GL.vertexAttribArray (GL.AttribLocation 1) $= GL.Enabled
  
  GL.vertexAttribPointer (GL.AttribLocation 2)
    $= (GL.ToFloat, GL.VertexArrayDescriptor 2 GL.Float (8 * 4) (plusPtr nullPtr (6 * 4)))
  GL.vertexAttribArray (GL.AttribLocation 2) $= GL.Enabled
  pure $ ChunkMesh vao vbo (length verts `div` 8)

deleteChunkMesh :: ChunkMesh -> IO ()
deleteChunkMesh (ChunkMesh vao vbo _) = do
  GL.deleteObjectName vbo
  GL.deleteObjectName vao
