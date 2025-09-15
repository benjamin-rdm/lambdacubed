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
    buildOverlayVertices,
    TerrainChunk (..),
    GpuMesh,
    uploadGpuMesh,
    deleteGpuMesh,
    blockAtV3,
    setBlockInChunk,
  )
where

import App.Config (chunkSize)
import Control.Monad (foldM, foldM_)
import Control.Monad.Reader
import Control.Monad.ST (ST)
import Control.Monad.State (State, execState, modify)
import Data.IntMap.Strict qualified as IM
import Data.Maybe
import Data.Vector.Storable qualified as VS
import Data.Vector.Storable.Mutable qualified as VSM
import Data.Word (Word8)
import Foreign.Ptr (Ptr, castPtr, nullPtr, plusPtr)
import Foreign.Storable (Storable (..), sizeOf)
import Game.Direction
import Graphics.Rendering.OpenGL (($=))
import Graphics.Rendering.OpenGL.GL qualified as GL
import Linear
import Rendering.Buffer (Buffer (..), deleteBuffer)
import Utils.Monad
import Utils.PerlinNoise (perlin2)

defaultClimate :: (Fractional a) => (a, a)
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

positionToIndex :: V3 Int -> Int -> Maybe Int
positionToIndex pos xySize =
  let V3 lx ly lz = pos
      V3 sx sy sz = chunkSize
   in if isWithinChunkBounds (V3 lx ly lz) (V3 sx sy sz)
        then Just (lx + ly * sx + lz * xySize)
        else Nothing

setBlockInChunk :: TerrainChunk -> V3 Int -> Block -> Maybe TerrainChunk
setBlockInChunk (TerrainChunk org@(V3 ox oy oz) blocks) (V3 x y z) newB =
  let (V3 sx sy sz) = chunkSize
      lx = x - ox
      ly = y - oy
      lz = z - oz
   in if lx < 0 || ly < 0 || lz < 0 || lx >= sx || ly >= sy || lz >= sz
        then Nothing
        else
          let i = lx + ly * sx + lz * sx * sy
              blocks' = blocks VS.// [(i, newB)]
           in Just (TerrainChunk org blocks')

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
    tBlocks :: !(VS.Vector Block)
  }

type WorldGen = Reader WorldGenConfig

type WorldPos = V3 Int

treeHeightDefault :: Int
treeHeightDefault = 5

leafRadiusDefault :: Int
leafRadiusDefault = 2

genTerrainChunk :: V3 Int -> TerrainChunk
genTerrainChunk origin = runReader (genTerrainChunkM origin) defaultWorldGenConfig

genTerrainChunkAtCoord :: V2 Int -> TerrainChunk
genTerrainChunkAtCoord (V2 cx cy) =
  let V3 sx sy _ = chunkSize
      origin = V3 (cx * sx) (cy * sy) 0
   in genTerrainChunk origin

genTerrainChunkM :: V3 Int -> WorldGen TerrainChunk
genTerrainChunkM org@(V3 ox oy _oz) = do
  let V3 sx sy sz = chunkSize
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
      !baseChunk = TerrainChunk org baseBlocks
  generateTreesM baseChunk

generateTreesM :: TerrainChunk -> WorldGen TerrainChunk
generateTreesM chunk@(TerrainChunk org blocks) = do
  positions <- findTreePositionsM chunk
  let im = execState (mapM_ (treeBuilder chunk) positions) IM.empty
      newBlocks = blocks VS.// IM.toList im
  pure $ TerrainChunk org newBlocks

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
chunkBoundsWithMargin (TerrainChunk (V3 ox oy _oz) _) margin =
  let (V3 sx sy _) = chunkSize
      fromX = ox - margin
      toX = ox + sx - 1 + margin
      fromY = oy - margin
      toY = oy + sy - 1 + margin
   in (fromX, toX, fromY, toY)

findTreePositionsM :: TerrainChunk -> WorldGen [V3 Int]
findTreePositionsM chunk@(TerrainChunk (V3 _ox _oy oz) _) = do
  let (V3 _ _ sz) = chunkSize
      margin = leafRadiusDefault + 1
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
addBlock (TerrainChunk origin _) b posWorld =
  whenJust (localIndex origin posWorld) $ \idx -> modify (IM.insertWith combine idx b)
  where
    combine new old = case (old, new) of
      -- We do not want leaves of one tree to overwrite logs of another tree
      (OakLog, _) -> old
      (_, OakLog) -> new
      _ -> new

localIndex :: V3 Int -> V3 Int -> Maybe Int
localIndex origin posWorld =
  let V3 sx sy _ = chunkSize
      xySize = sx * sy
   in positionToIndex (posWorld - origin) xySize

heightAtWorldM :: Int -> Int -> WorldGen Int
heightAtWorldM wx wy = do
  let V3 _ _ sz = chunkSize
      pos = (fromIntegral wx, fromIntegral wy)
  f <- noiseFreqM
  h1 <- sample01AtFreqM f pos
  h2 <- sample01AtFreqM (f * 2.1) pos
  base <- baseHeightM
  amp <- heightAmplitudeM
  let h = 0.7 * h1 + 0.3 * h2
  pure $ clamp (base + floor (fromIntegral amp * h)) 0 sz

blockAt :: TerrainChunk -> Int -> Int -> Int -> Block
blockAt (TerrainChunk origin bm) !x !y !z =
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

dirOffset :: Direction -> V3 Int
dirOffset NegX = V3 (-1) 0 0
dirOffset PosX = V3 1 0 0
dirOffset NegY = V3 0 (-1) 0
dirOffset PosY = V3 0 1 0
dirOffset NegZ = V3 0 0 (-1)
dirOffset PosZ = V3 0 0 1

data Block
  = Air
  | Dirt
  | Grass
  | Stone
  | Gravel
  | Water
  | OakLog
  | OakLeaves
  deriving (Eq, Ord, Show, Enum, Bounded)

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

-- Write two triangles (6 vertices) for a quad directly into a mutable vector.
-- Each vertex layout: px, py, pz, u, v, layer, temperature, humidity
writeFace :: VSM.MVector s Float -> Int -> V3 Float -> V3 Float -> V3 Float -> V3 Float -> Float -> (Float, Float) -> ST s Int
writeFace mv off v1 v2 v3 v4 layer (t, h) = do
  let writeVertex i (V3 px py pz) (u, v) = do
        VSM.unsafeWrite mv (i + 0) px
        VSM.unsafeWrite mv (i + 1) py
        VSM.unsafeWrite mv (i + 2) pz
        VSM.unsafeWrite mv (i + 3) u
        VSM.unsafeWrite mv (i + 4) v
        VSM.unsafeWrite mv (i + 5) layer
        VSM.unsafeWrite mv (i + 6) t
        VSM.unsafeWrite mv (i + 7) h
      vertiexSize = 8
      writeTriangle i a ua b ub c uc = do
        writeVertex i a ua
        writeVertex (i + vertiexSize) b ub
        writeVertex (i + 2 * vertiexSize) c uc
        pure (i + 3 * vertiexSize)
  i1 <- writeTriangle off v1 (0, 0) v2 (1, 0) v3 (1, 1)
  writeTriangle i1 v1 (0, 0) v3 (1, 1) v4 (0, 1)

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

type TextureLayerOf = Block -> Direction -> Float

buildTerrainVertices :: TextureLayerOf -> TerrainChunk -> VS.Vector Float
buildTerrainVertices texOf chunk@(TerrainChunk chunkOrigin _) = VS.create $ do
  let (V3 sx sy sz) = chunkSize
      dirs = [NegX, PosX, NegY, PosY, NegZ, PosZ]
      positions = [chunkOrigin + V3 lx ly lz | lz <- [0 .. sz - 1], ly <- [0 .. sy - 1], lx <- [0 .. sx - 1]]
      faceVisible worldPosI dir = not (blockOpaque (blockAtV3 chunk (worldPosI + dirOffset dir)))
      countFaces =
        sum
          [ length [() | dir <- dirs, faceVisible worldPosI dir]
            | worldPosI <- positions,
              let block = blockAtV3 chunk worldPosI,
              blockOpaque block
          ]
      floatsPerFace = 6 * 8
      totalFloats = countFaces * floatsPerFace
  mv <- VSM.new totalFloats
  let stepDir worldPosI block off dir =
        if faceVisible worldPosI dir
          then
            let (v1, v2, v3, v4) = faceCorners worldPosI dir
                layer = case (block, dir) of
                  (Grass, NegX) -> texOf Grass NegZ
                  (Grass, PosX) -> texOf Grass NegZ
                  (Grass, NegY) -> texOf Grass NegZ
                  (Grass, PosY) -> texOf Grass NegZ
                  _ -> texOf block dir
             in writeFace mv off v1 v2 v3 v4 layer defaultClimate
          else pure off
      stepPos off p =
        let block = blockAtV3 chunk p
         in if blockOpaque block
              then foldM (stepDir p block) off dirs
              else pure off
  foldM_ stepPos 0 positions
  pure mv

-- Render water slightly below the surface of the block
waterLevelOffset :: Float
waterLevelOffset = 0.1

-- TODO: Investigate if a fold instead of mapM_ would help performance
-- because we do not need to keep an STRef around
buildWaterVertices :: TextureLayerOf -> TerrainChunk -> VS.Vector Float
buildWaterVertices texOf chunk@(TerrainChunk chunkOrigin _) = VS.create $ do
  let (V3 sx sy sz) = chunkSize
      positions = [chunkOrigin + V3 lx ly lz | lz <- [0 .. sz - 1], ly <- [0 .. sy - 1], lx <- [0 .. sx - 1]]
      qualifies worldPosI =
        blockAtV3 chunk worldPosI == Water
          && blockAtV3 chunk (worldPosI + dirOffset PosZ) == Air
      countFaces = 2 * sum [1 | worldPosI <- positions, qualifies worldPosI]
      floatsPerFace = 6 * 8
      totalFloats = countFaces * floatsPerFace
  mv <- VSM.new totalFloats
  let dir = PosZ
      step off worldPosI =
        if qualifies worldPosI
          then do
            let offsetWorldPos = (fromIntegral <$> worldPosI) - V3 0 0 waterLevelOffset
                (v1, v2, v3, v4) = faceCorners offsetWorldPos dir
                (v5, v6, v7, v8) = faceCorners offsetWorldPos (invertDirection dir)
                layer = texOf Water dir
            off' <- writeFace mv off v1 v2 v3 v4 layer defaultClimate
            writeFace mv off' v5 v6 v7 v8 layer defaultClimate
          else pure off
  foldM_ step 0 positions
  pure mv

buildLeavesVertices :: TextureLayerOf -> TerrainChunk -> VS.Vector Float
buildLeavesVertices texOf chunk@(TerrainChunk chunkOrigin _) = VS.create $ do
  let (V3 sx sy sz) = chunkSize
      dirs = [NegX, PosX, NegY, PosY, NegZ, PosZ]
      positions = [chunkOrigin + V3 lx ly lz | lz <- [0 .. sz - 1], ly <- [0 .. sy - 1], lx <- [0 .. sx - 1]]
      isLeaves worldPosI = blockAtV3 chunk worldPosI == OakLeaves
      leavesBlocks = length [() | worldPosI <- positions, isLeaves worldPosI]
      countFaces = leavesBlocks * 12
      floatsPerFace = 6 * 8
      totalFloats = countFaces * floatsPerFace
  mv <- VSM.new totalFloats
  let writePair worldPosI off dir = do
        let (v1, v2, v3, v4) = faceCorners worldPosI dir
            (v5, v6, v7, v8) = faceCorners worldPosI (invertDirection dir)
            layer = texOf OakLeaves dir
        off' <- writeFace mv off v1 v2 v3 v4 layer (0.8, 0.4)
        writeFace mv off' v5 v6 v7 v8 layer (0.8, 0.4)
      stepPos off p =
        if isLeaves p
          then foldM (writePair p) off dirs
          else pure off
  foldM_ stepPos 0 positions
  pure mv

buildOverlayVertices :: (Block -> Direction -> Maybe Float) -> TerrainChunk -> VS.Vector Float
buildOverlayVertices overlayOf chunk@(TerrainChunk chunkOrigin _) = VS.create $ do
  let (V3 sx sy sz) = chunkSize
      dirs = [NegX, PosX, NegY, PosY, NegZ, PosZ]
      positions = [chunkOrigin + V3 lx ly lz | lz <- [0 .. sz - 1], ly <- [0 .. sy - 1], lx <- [0 .. sx - 1]]
      faceVisible worldPosI dir = not (blockOpaque (blockAtV3 chunk (worldPosI + dirOffset dir)))
      countFaces =
        sum
          [ length [() | dir <- dirs, faceVisible worldPosI dir, isJust (overlayOf block dir)]
            | worldPosI <- positions,
              let block = blockAtV3 chunk worldPosI
          ]
      floatsPerFace = 6 * 8
      totalFloats = countFaces * floatsPerFace
  mv <- VSM.new totalFloats
  let stepDir worldPosI block off dir =
        case overlayOf block dir of
          Just layer
            | faceVisible worldPosI dir ->
                let (v1, v2, v3, v4) = faceCorners worldPosI dir
                 in writeFace mv off v1 v2 v3 v4 layer defaultClimate
          _ -> pure off
      stepPos off p =
        let block = blockAtV3 chunk p
         in foldM (stepDir p block) off dirs
  foldM_ stepPos 0 positions
  pure mv

type GpuMesh = Buffer

uploadGpuMesh :: VS.Vector Float -> IO GpuMesh
uploadGpuMesh verts = do
  vao <- GL.genObjectName
  vbo <- GL.genObjectName
  GL.bindVertexArrayObject $= Just vao
  GL.bindBuffer GL.ArrayBuffer $= Just vbo
  VS.unsafeWith verts $ \ptr -> do
    let bytes = fromIntegral (VS.length verts * sizeOf (undefined :: Float))
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

  pure $ Buffer vao vbo (VS.length verts `div` 8)

deleteGpuMesh :: GpuMesh -> IO ()
deleteGpuMesh = deleteBuffer
