{-# OPTIONS_GHC -Wno-type-defaults #-}

module Game.World
  ( Block (..),
    WorldGenConfig (..),
    defaultWorldGenConfig,
    blockOpaque,
    genTerrainChunk,
    genTerrainChunkAtCoord,
    TerrainChunk (..),
    blockAtV3,
    setBlockInChunk,
  )
where

import App.Config (chunkSize)
import Control.Monad.Reader
import Control.Monad.State (State, execState, modify)
import Data.IntMap.Strict qualified as IM
import Data.Maybe
import Data.Vector.Storable qualified as VS
import Data.Word (Word8)
import Foreign.Ptr (Ptr, castPtr)
import Foreign.Storable (Storable (..), sizeOf)
import Linear
import Utils.Monad
import Utils.PerlinNoise (perlin2)

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