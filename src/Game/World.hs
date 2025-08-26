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
    TerrainChunk (..),
    ChunkMesh (..),
    uploadChunk,
    deleteChunkMesh,
    blockAtV3,
    setBlockInChunk,
  )
where

import Control.Monad.Reader
import Data.Foldable
import Data.Vector.Storable qualified as VS
import Data.Word (Word8)
import Foreign.Marshal.Array (withArray)
import Foreign.Ptr (Ptr, castPtr, nullPtr, plusPtr)
import Foreign.Storable (Storable (..), sizeOf)
import GHC.Generics (Generic)
import Graphics.Rendering.OpenGL (($=))
import Graphics.Rendering.OpenGL.GL qualified as GL
import Linear
import Utils.PerlinNoise (perlin2)
import Game.WorldConfig (worldChunkSize)

calculateNoise :: Float -> Float -> Float -> Float -> Float -> Float
calculateNoise x y baseFreq scaleX scaleY =
  let nx = fromIntegral (floor x) * baseFreq * scaleX
      ny = fromIntegral (floor y) * baseFreq * scaleY
      n = perlin2 nx ny
      n01 = 2.0 * (n + 1.0)
   in n01

clamp :: (Ord a) => a -> a -> a -> a
clamp val minVal maxVal = max minVal (min maxVal val)

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
    wgWaterLevel :: !Int,
    wgTreeDensity :: !Float
  }
  deriving (Eq, Show)

defaultWorldGenConfig :: WorldGenConfig
defaultWorldGenConfig =
  WorldGenConfig
    { wgNoiseFreq = 0.01,
      wgScaleX = 0.8,
      wgScaleY = 1.3,
      wgWaterLevel = 30,
      wgTreeDensity = 0.0 -- Tree generation does not work currently.
      -- I also still need to consider how to render
      -- leaves and other semi-transparent blocks
    }

data TerrainChunk = TerrainChunk
  { tOrigin :: !(V3 Int),
    tSize :: !(V3 Int),
    tBlocks :: !(VS.Vector Block)
  }

type WorldGen = Reader WorldGenConfig

-- Generate a chunk at a given world-space origin using the fixed worldChunkSize
genTerrainChunk :: V3 Int -> TerrainChunk
genTerrainChunk origin = runReader (genTerrainChunkM origin) defaultWorldGenConfig

-- Generate a TerrainChunk given a 2D chunk coordinate using the fixed worldChunkSize
genTerrainChunkAtCoord :: V2 Int -> TerrainChunk
genTerrainChunkAtCoord (V2 cx cy) =
  let V3 sx sy _ = worldChunkSize
      origin = V3 (cx * sx) (cy * sy) 0
   in genTerrainChunk origin

genTerrainChunkM :: V3 Int -> WorldGen TerrainChunk
genTerrainChunkM org@(V3 ox oy _oz) = do
  config <- ask
  let V3 sx sy sz = worldChunkSize
  let !totalSize = sx * sy * sz
      !xySize = sx * sy

      height !x !y =
        let !baseFreq = wgNoiseFreq config
            !scaleX = wgScaleX config
            !scaleY = wgScaleY config
            !n01 = calculateNoise (fromIntegral (ox + x)) (fromIntegral (oy + y)) baseFreq scaleX scaleY
         in clamp (20 + floor (10.0 * n01)) 0 sz

      getBlock !i =
        let !waterLevelZ = wgWaterLevel config
            (!z, !r1) = i `divMod` xySize
            (!y, !x) = r1 `divMod` sx
            !h = height x y
         in if z > h - 1
              then if z <= waterLevelZ then Water else Air
              else
                if z == h - 1
                  then Grass
                  else
                    if z >= h - 4
                      then Dirt
                      else Stone

      !baseBlocks = VS.generate totalSize getBlock
      !baseChunk = TerrainChunk org worldChunkSize baseBlocks
  generateTreesM baseChunk

generateTreesM :: TerrainChunk -> WorldGen TerrainChunk
generateTreesM chunk@(TerrainChunk org siz blocks) = do
  config <- ask
  let !treePositions = findTreePositionsM chunk config
      !newBlocks = foldl' (placeTree chunk) blocks treePositions
  return $ TerrainChunk org siz newBlocks

findTreePositionsM :: TerrainChunk -> WorldGenConfig -> [V3 Int]
findTreePositionsM chunk@(TerrainChunk (V3 ox oy oz) (V3 sx sy sz) _) config =
  [ V3 (ox + x) (oy + y) (oz + surfaceZ + 1)
    | x <- [10, 20, 30, 40, 50], -- This is just for testing and should be reworked to use a noise-based function
      y <- [10, 20, 30, 40, 50],
      x < sx - 3 && y < sy - 3,
      let surfaceZ = findSurfaceAt chunk x y,
      surfaceZ > 1 && surfaceZ < sz - 10,
      shouldPlaceTreeM (ox + x) (oy + y) config
  ]

findSurfaceAt :: TerrainChunk -> Int -> Int -> Int
findSurfaceAt chunk@(TerrainChunk (V3 ox oy oz) (V3 _ _ sz) _) !x !y =
  let searchDown !z
        | z < 0 = 0
        | otherwise =
            let !block = blockAt chunk (ox + x) (oy + y) (oz + z)
             in case block of
                  Grass -> z + 1
                  Dirt -> z + 1
                  _ -> searchDown (z - 1)
   in searchDown (sz - 1)

shouldPlaceTreeM :: Int -> Int -> WorldGenConfig -> Bool
shouldPlaceTreeM !x !y config =
  let !density = wgTreeDensity config
      !seed = fromIntegral (x * 374761393 + y * 668265263)
      !noise = perlin2 (seed * 0.08) 0
   in noise > (1.0 - density)

placeTree :: TerrainChunk -> VS.Vector Block -> V3 Int -> VS.Vector Block
placeTree chunk@(TerrainChunk _ chunkSize _) blocks (V3 tx ty tz) =
  let !treeHeight = 7
      !leafRadius = 3
      !xySize = let V3 sx sy _ = chunkSize in sx * sy

      !trunkBlocks = [V3 tx ty (tz + h) | h <- [0 .. treeHeight - 1]]
      !leafBlocks =
        [ V3 (tx + dx) (ty + dy) (tz + treeHeight + dz)
          | dx <- [-leafRadius .. leafRadius],
            dy <- [-leafRadius .. leafRadius],
            let !distSq = dx * dx + dy * dy,
            distSq <= leafRadius * leafRadius,
            dz <- [-1 .. 1],
            not (dx == 0 && dy == 0 && dz <= 0)
        ]

      !trunkUpdates =
        [ (idx, OakLog)
          | pos <- trunkBlocks,
            Just idx <- [positionToIndex pos chunkSize xySize]
        ]

      !leafUpdates =
        [ (idx, OakLeaves)
          | pos <- leafBlocks,
            blockAtV3 chunk pos == Air,
            Just idx <- [positionToIndex pos chunkSize xySize]
        ]

      !allUpdates = trunkUpdates ++ leafUpdates
   in blocks VS.// allUpdates

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

face :: V3 Float -> V3 Float -> V3 Float -> V3 Float -> Float -> [Float]
face v1 v2 v3 v4 layer =
  let tri a (u1, v1') b (u2, v2') c (u3, v3') =
        toList a
          ++ [u1, v1', layer]
          ++ toList b
          ++ [u2, v2', layer]
          ++ toList c
          ++ [u3, v3', layer]
   in tri v1 (0, 0) v2 (1, 0) v3 (1, 1)
        ++ tri v1 (0, 0) v3 (1, 1) v4 (0, 1)

faceCorners :: V3 Int -> TextureDirection -> (V3 Float, V3 Float, V3 Float, V3 Float)
faceCorners (V3 x y z) dir =
  let f a b c = fromIntegral <$> V3 a b c
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
                      layer = textureLayerOf block dir
                   in face v1 v2 v3 v4 layer
                  | dir <- [NegX, PosX, NegY, PosY, NegZ, PosZ],
                    let neighborPos = worldPosI + dirOffset dir,
                    let neighbor = blockAtV3 chunk neighborPos,
                    not (blockOpaque neighbor)
                ]
      | lz <- [0 .. sz - 1],
        ly <- [0 .. sy - 1],
        lx <- [0 .. sx - 1]
    ]

buildWaterVertices :: TerrainChunk -> [Float]
buildWaterVertices chunk@(TerrainChunk chunkOrigin (V3 sx sy sz) _) =
  concat
    [ let worldPosI = chunkOrigin + V3 lx ly lz
       in case blockAtV3 chunk worldPosI of
            Water ->
              concat
                [ let (v1, v2, v3, v4) = faceCorners worldPosI dir
                      layer = textureLayerOf Water dir
                   in face v1 v2 v3 v4 layer
                  | dir <- [NegX, PosX, NegY, PosY, NegZ, PosZ],
                    let neighborPos = worldPosI + dirOffset dir,
                    blockAtV3 chunk neighborPos == Air
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
    $= (GL.ToFloat, GL.VertexArrayDescriptor 3 GL.Float (6 * 4) (plusPtr nullPtr 0))
  GL.vertexAttribArray (GL.AttribLocation 0) $= GL.Enabled

  GL.vertexAttribPointer (GL.AttribLocation 1)
    $= (GL.ToFloat, GL.VertexArrayDescriptor 3 GL.Float (6 * 4) (plusPtr nullPtr (3 * 4)))
  GL.vertexAttribArray (GL.AttribLocation 1) $= GL.Enabled
  pure $ ChunkMesh vao vbo (length verts `div` 6)

deleteChunkMesh :: ChunkMesh -> IO ()
deleteChunkMesh (ChunkMesh vao vbo _) = do
  GL.deleteObjectName vbo
  GL.deleteObjectName vao
