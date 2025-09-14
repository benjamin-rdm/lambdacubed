{-# LANGUAGE OverloadedStrings #-}

module Game.Block.Atlas
  ( SpecialIndices (..),
    Atlas (..),
    buildBlockAtlas,
  )
where

import Control.Monad
import Data.Foldable
import Data.List (partition)
import Data.Map.Strict qualified as M
import Data.Maybe (fromMaybe, isJust, mapMaybe)
import Data.Set qualified as S
import Data.Text (Text)
import Data.Text qualified as T
import Game.Block.Properties
import Game.Direction
import Game.World (Block (..))
import Graphics.Rendering.OpenGL.GL qualified as GL
import Rendering.Texture

-- This is passed to the shader code generator for it to generate the correct handling of special textures
data SpecialIndices = SpecialIndices
  { siWaterBase :: !Int, -- Base texture for the water sprite
    siWaterFrames :: !Int, -- Number of frames for the water sprite
    siTintGrassIndices :: !(S.Set Int), -- Texture indices that should get the grass tint applied
    siTintFoliageIndices :: !(S.Set Int), -- Same for foliage
    siOverlayIndices :: !(S.Set Int) -- Textures used as overlays
  }

data Atlas = Atlas
  { atTexture :: !GL.TextureObject,
    atLayerOf :: Block -> Direction -> Int, -- Base layer index for a block face
    atOverlayOf :: Block -> Direction -> Maybe Int, -- Overlay for block face
    atSpecial :: !SpecialIndices
  }

-- Identifier of a texture within the resource pack
-- Examples:
-- -> "minecraft:block/oak_leaves"
-- -> "block/grass_block_top"
-- -> "block/grass_block_side_overlay"
-- Don't quite understand why we sometimes have minecraft namespace and sometimes not.
type ResourceId = Text

type BlockFace = (Block, Direction)

data BlockMeta = BlockMeta
  { bmModelPath :: !(Maybe FilePath),
    bmExtraTextures :: ![ResourceId]
  }

blockRegistry :: M.Map Block BlockMeta
blockRegistry =
  M.fromList
    [ (Dirt, BlockMeta (Just (atModelPath "dirt.json")) []),
      (Grass, BlockMeta (Just (atModelPath "grass_block.json")) []),
      (Stone, BlockMeta (Just (atModelPath "stone.json")) []),
      (Gravel, BlockMeta (Just (atModelPath "gravel.json")) []),
      (OakLog, BlockMeta (Just (atModelPath "oak_log.json")) []),
      (OakLeaves, BlockMeta (Just (atModelPath "oak_leaves.json")) []),
      (Water, BlockMeta Nothing ["minecraft:block/water_still_blue"]),
      (Air, BlockMeta Nothing [])
    ]

texturePath :: FilePath
texturePath = "resource_pack/assets/minecraft/textures/"

modelPath :: FilePath
modelPath = "resource_pack/assets/minecraft/models/block/"

atModelPath :: FilePath -> FilePath
atModelPath = (modelPath <>)

resIdToPath :: Text -> FilePath
resIdToPath rid =
  let strippedName = fromMaybe rid (T.stripPrefix "minecraft:" rid)
   in texturePath ++ T.unpack strippedName ++ ".png"

resolveFaceTexture :: M.Map Text Text -> Text -> Maybe Text
resolveFaceTexture texMap = go
  where
    go t
      | Just ('#', rest) <- T.uncons t = go =<< M.lookup rest texMap
      | otherwise = Just t

faceLayersOf :: BlockModel -> M.Map Direction (ResourceId, Bool)
faceLayersOf bm = M.fromList [(d, (rid, isOv)) | (d, rid, isOv, _) <- facesInfo bm]

facesInfo :: BlockModel -> [(Direction, ResourceId, Bool, Maybe Int)]
facesInfo (BlockModel _ texMap els) = concatMap perElement els
  where
    perElement Element {elFaces} = mapMaybe perFace (M.toList elFaces)
    perFace (dir, FaceDef {fdTexture, fdTintIndex}) =
      let keyName = case T.uncons fdTexture of
            Just ('#', rest) -> rest
            _ -> ""
          isOverlay = keyName == "overlay"
       in case resolveFaceTexture texMap fdTexture of
            Just rid -> Just (dir, rid, isOverlay, fdTintIndex)
            Nothing -> Nothing

modelPathForBlock :: Block -> Maybe FilePath
modelPathForBlock b = bmModelPath =<< M.lookup b blockRegistry

extraTexturesForBlock :: Block -> S.Set ResourceId
extraTexturesForBlock b = S.fromList $ maybe [] bmExtraTextures (M.lookup b blockRegistry)

loadBlockModels :: [Block] -> IO [(Block, Maybe BlockModel)]
loadBlockModels bs = forM bs $ \b -> case modelPathForBlock b of
  -- We should have resources for all blocks other than Water and Air
  Nothing -> if b `elem` [Water, Air] then return (b, Nothing) else error ("No path for resources for block " ++ show b)
  Just p -> do
    em <- loadBlockModel p
    pure $ case em of
      Left _ -> (b, Nothing)
      Right m -> (b, Just m)

perBlockFacesFromModels :: [(Block, Maybe BlockModel)] -> [(Block, M.Map Direction (ResourceId, Bool))]
perBlockFacesFromModels models = [(b, faceLayersOf m) | (b, Just m) <- models]

allResourceIds :: [(Block, M.Map Direction (ResourceId, Bool))] -> S.Set ResourceId
allResourceIds perBlockFaces =
  let faceIds = foldMap (S.fromList . map fst . M.elems . snd) perBlockFaces
      extraIds = foldMap extraTexturesForBlock [minBound .. maxBound]
   in faceIds `S.union` extraIds

loadResId :: Text -> IO (Text, [Image PixelRGBA8])
loadResId rid = do
  img <- loadImage (resIdToPath rid)
  pure (rid, extractSpriteFrames img)

loadFramesAndIndex :: (Foldable f) => f Text -> IO (M.Map Text Int, M.Map Text Int, [Image PixelRGBA8])
loadFramesAndIndex rids = buildMaps 0 M.empty M.empty [] <$> traverse loadResId (toList rids)
  where
    buildMaps :: (Ord k) => Int -> M.Map k Int -> M.Map k Int -> [a] -> [(k, [a])] -> (M.Map k Int, M.Map k Int, [a])
    -- idx: Current index of texture
    -- im: Map of base indices for textures
    -- cm: Map of size of sprites (1 normally, 16 for water sprite)
    buildMaps _ im cm acc [] = (im, cm, acc)
    buildMaps idx im cm acc ((rid, frames) : rest) =
      let count = length frames
       in buildMaps
            (idx + count)
            (M.insert rid idx im)
            (M.insert rid count cm)
            (acc ++ frames)
            rest

buildLayerMaps :: [(Block, M.Map Direction (ResourceId, Bool))] -> M.Map ResourceId Int -> (M.Map BlockFace Int, M.Map BlockFace Int, S.Set Int)
buildLayerMaps perBlockFaces ridIndexMap =
  (M.fromList baseLayerAssocs, M.fromList overlayLayerAssocs, S.fromList overlayIndices)
  where
    baseIndex rid = M.lookup rid ridIndexMap

    allFaces :: [(BlockFace, Int, Bool)]
    allFaces =
      [ ((b, d), idx, isOv)
        | (b, faces) <- perBlockFaces,
          (d, (rid, isOv)) <- M.toList faces,
          Just idx <- [baseIndex rid]
      ]

    -- I hate the formatter sometimes
    baseLayerAssocs = [(bf, idx) | (bf, idx, _) <- allFaces]
    overlayLayerAssocs = [(bf, idx) | (bf, idx, True) <- allFaces]
    overlayIndices = [idx | (_, idx, True) <- allFaces]

tintedBaseRids :: BlockModel -> S.Set ResourceId
tintedBaseRids bm = S.fromList [rid | (_, rid, isOv, mTint) <- facesInfo bm, not isOv, isJust mTint]

leafTintBlocks :: S.Set Block
leafTintBlocks = S.fromList [OakLeaves]

computeTintSets :: [(Block, Maybe BlockModel)] -> M.Map ResourceId Int -> (S.Set Int, S.Set Int)
computeTintSets models ridIndexMap = (grassIdxs, foliageIdxs)
  where
    baseIndex rid = M.lookup rid ridIndexMap
    toIdxSet = S.fromList . mapMaybe baseIndex
    isFoliage p = fst p `elem` leafTintBlocks
    perBlockTinted = [(b, tintedBaseRids bm) | (b, Just bm) <- models]
    (foliage, grass) = partition isFoliage perBlockTinted
    grassRids = foldMap (S.toList . snd) grass
    foliageRids = foldMap (S.toList . snd) foliage
    grassIdxs = toIdxSet grassRids
    foliageIdxs = toIdxSet foliageRids

justOrError :: String -> Maybe a -> a
justOrError = fromMaybe . error

buildBlockAtlas :: IO Atlas
buildBlockAtlas = do
  let blocks = [minBound .. maxBound]
  models <- loadBlockModels blocks
  let perBlockFaces = perBlockFacesFromModels models
      rids = allResourceIds perBlockFaces

  (ridIndexMap, ridFrameCountMap, images) <- loadFramesAndIndex rids
  tex <- createTextureArrayFromImages images

  let baseIndexOf rid = M.lookup rid ridIndexMap
      frameCountOf rid = M.lookup rid ridFrameCountMap

      (baseLayerMap, overlayLayerMap, overlayIdxs) = buildLayerMaps perBlockFaces ridIndexMap

      layerOf :: Block -> Direction -> Int
      layerOf Water _ =
        let waterRid = S.lookupMin (extraTexturesForBlock Water)
         in justOrError "Water base index missing" (waterRid >>= baseIndexOf)
      layerOf b d = justOrError ("Missing base layer for block/dir: " <> show (b, d)) (M.lookup (b, d) baseLayerMap)

      overlayOf :: Block -> Direction -> Maybe Int
      overlayOf b d = M.lookup (b, d) overlayLayerMap

      (waterBase, waterFrames) =
        let waterRid = S.lookupMin (extraTexturesForBlock Water)
         in ( justOrError "Water base texture missing" (waterRid >>= baseIndexOf),
              justOrError "Water frames missing" (waterRid >>= frameCountOf)
            )

      (tintedGrassIdxs, tintedFoliageIdxs) = computeTintSets models ridIndexMap

  pure $ Atlas tex layerOf overlayOf (SpecialIndices waterBase waterFrames tintedGrassIdxs tintedFoliageIdxs overlayIdxs)
