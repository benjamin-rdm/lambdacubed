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
import Data.Maybe (catMaybes, fromMaybe, isJust, mapMaybe)
import Data.Set qualified as S
import Data.Text (Text)
import Data.Text qualified as T
import Game.Block.Properties
import Game.Direction
import Game.World (Block (..))
import Graphics.Rendering.OpenGL.GL qualified as GL
import Rendering.Texture

data SpecialIndices = SpecialIndices
  { siWaterBase :: !Int,
    siWaterFrames :: !Int,
    siTintGrassIndices :: !(S.Set Int),
    siTintFoliageIndices :: !(S.Set Int),
    siOverlayIndices :: !(S.Set Int)
  }

data Atlas = Atlas
  { atTexture :: !GL.TextureObject,
    atLayerOf :: Block -> Direction -> Int,
    atOverlayOf :: Block -> Direction -> Maybe Int,
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

type RidIndexMap = M.Map ResourceId Int

data BlockMeta = BlockMeta
  { bmModelPath :: !(Maybe FilePath),
    bmExtraTextures :: ![ResourceId]
  }

-- TODO: Should this just be a function instead of a Map?
blockRegistry :: M.Map Block BlockMeta
blockRegistry =
  M.fromList
    [ (Dirt, BlockMeta (Just "resource_pack/assets/minecraft/models/block/dirt.json") []),
      (Grass, BlockMeta (Just "resource_pack/assets/minecraft/models/block/grass_block.json") []),
      (Stone, BlockMeta (Just "resource_pack/assets/minecraft/models/block/stone.json") []),
      (Gravel, BlockMeta (Just "resource_pack/assets/minecraft/models/block/gravel.json") []),
      (OakLog, BlockMeta (Just "resource_pack/assets/minecraft/models/block/oak_log.json") []),
      (OakLeaves, BlockMeta (Just "resource_pack/assets/minecraft/models/block/oak_leaves.json") []),
      (Water, BlockMeta Nothing ["minecraft:block/water_still_blue"]),
      (Air, BlockMeta Nothing [])
    ]

resIdToPath :: Text -> FilePath
resIdToPath rid =
  let noNs = fromMaybe rid (T.stripPrefix "minecraft:" rid)
   in "resource_pack/assets/minecraft/textures/" ++ T.unpack noNs ++ ".png"

resolveFaceTexture :: M.Map Text Text -> Text -> Maybe Text
resolveFaceTexture texMap = go
  where
    go t
      | Just ('#', rest) <- T.uncons t = go =<< M.lookup rest texMap
      | otherwise = Just t

faceLayersOf :: BlockModel -> M.Map Direction (ResourceId, Bool)
faceLayersOf bm = M.fromListWith const [(d, (rid, isOv)) | (d, rid, isOv, _) <- facesInfo bm]

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

extraTexturesForBlock :: Block -> [ResourceId]
extraTexturesForBlock b = maybe [] bmExtraTextures (M.lookup b blockRegistry)

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
perBlockFacesFromModels models =
  [(b, faceLayersOf m) | (b, Just m) <- models]

allResourceIds :: [(Block, M.Map Direction (ResourceId, Bool))] -> S.Set ResourceId
allResourceIds perBlockFaces =
  let faceIds = [rid | (_, faces) <- perBlockFaces, (rid, _) <- M.elems faces]
      extraIds = concatMap extraTexturesForBlock [minBound .. maxBound]
   in S.fromList (faceIds ++ extraIds)

loadResId :: Text -> IO (Text, [Image PixelRGBA8])
loadResId rid = do
  let path = resIdToPath rid
  img <- loadImage path
  pure (rid, extractSpriteFrames img)

loadFramesAndIndex :: (Foldable f) => f Text -> IO (M.Map Text Int, M.Map Text Int, [Image PixelRGBA8])
loadFramesAndIndex rids = buildMaps 0 M.empty M.empty [] <$> traverse loadResId (toList rids)
  where
    buildMaps :: (Ord k) => Int -> M.Map k Int -> M.Map k Int -> [a] -> [(k, [a])] -> (M.Map k Int, M.Map k Int, [a])
    buildMaps _ im cm acc [] = (im, cm, reverse acc)
    buildMaps idx im cm acc ((rid, frames) : rest) =
      let count = length frames
       in buildMaps
            (idx + count)
            (M.insert rid idx im)
            (M.insert rid count cm)
            (reverse frames ++ acc)
            rest

buildLayerMaps :: [(Block, M.Map Direction (ResourceId, Bool))] -> RidIndexMap -> (M.Map BlockFace Int, M.Map BlockFace Int, S.Set Int)
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

computeTintSets :: [Maybe BlockModel] -> RidIndexMap -> (S.Set Int, S.Set Int)
computeTintSets models ridIndexMap = (grassIdxs, foliageIdxs)
  where
    baseIndex rid = M.lookup rid ridIndexMap
    isFoliage t = "leaves" `T.isInfixOf` T.toLower t
    -- foldMap ~ concatMap
    tintedRids = foldMap tintedBaseRids (catMaybes models)

    (foliageRids, grassRids) = partition isFoliage (toList tintedRids)

    toIdxSet = S.fromList . mapMaybe baseIndex

    grassIdxs = toIdxSet grassRids
    foliageIdxs = toIdxSet foliageRids

justOrError :: Text -> Maybe a -> a
justOrError = fromMaybe . error . T.unpack

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
      layerOf Water _ = justOrError "Water base index missing" (baseIndexOf "minecraft:block/water_still_blue")
      layerOf b d = justOrError ("Missing base layer for block/dir: " <> T.pack (show (b, d))) (M.lookup (b, d) baseLayerMap)

      overlayOf :: Block -> Direction -> Maybe Int
      overlayOf b d = M.lookup (b, d) overlayLayerMap

      waterBase = justOrError "Water base texture missing" (baseIndexOf "minecraft:block/water_still_blue")
      waterFrames = justOrError "Water frames missing" (frameCountOf "minecraft:block/water_still_blue")

      (tintedGrassIdxs, tintedFoliageIdxs) = computeTintSets (fmap snd models) ridIndexMap

  pure $ Atlas tex layerOf overlayOf (SpecialIndices waterBase waterFrames tintedGrassIdxs tintedFoliageIdxs overlayIdxs)