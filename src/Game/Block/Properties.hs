{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Game.Block.Properties
  ( FaceDef (..),
    Element (..),
    BlockModel (..),
    loadBlockModel,
  )
where

import Data.Aeson
import Data.Aeson.Types (Parser)
import Data.List (isSuffixOf)
import Data.Bifunctor
import Data.Map.Strict qualified as M
import Data.Maybe (fromMaybe)
import Data.Text qualified as T
import Data.Vector qualified as V
import GHC.Generics
import Game.Direction (Direction (..))
import System.FilePath (takeDirectory, (</>))

data FaceDef = FaceDef
  { fdUV :: !(Maybe (Double, Double, Double, Double)),
    fdTexture :: !T.Text,
    fdCullface :: !(Maybe Direction),
    fdTintIndex :: !(Maybe Int)
  }
  deriving (Show, Eq, Generic)

data Element = Element
  { elFrom :: !(Double, Double, Double),
    elTo :: !(Double, Double, Double),
    elFaces :: !(M.Map Direction FaceDef)
  }
  deriving (Show, Eq, Generic)

data BlockModel = BlockModel
  { bmParent :: !(Maybe T.Text),
    bmTextures :: !(M.Map T.Text T.Text),
    bmElements :: ![Element]
  }
  deriving (Show, Eq, Generic)

parseDirection :: T.Text -> Parser Direction
parseDirection = \case
  "north" -> pure NegY
  "south" -> pure PosY
  "west" -> pure NegX
  "east" -> pure PosX
  "up" -> pure PosZ
  "down" -> pure NegZ
  other -> fail $ "Unknown direction: " <> T.unpack other

instance FromJSON Direction where
  parseJSON :: Value -> Parser Direction
  parseJSON = withText "Direction" parseDirection

instance FromJSONKey Direction where
  fromJSONKey :: FromJSONKeyFunction Direction
  fromJSONKey = FromJSONKeyTextParser parseDirection

parseTuple3 :: Value -> Parser (Double, Double, Double)
parseTuple3 = withArray "3-tuple" $ \arr ->
  if V.length arr == 3
    then
      (,,)
        <$> parseJSON (arr V.! 0)
        <*> parseJSON (arr V.! 1)
        <*> parseJSON (arr V.! 2)
    else fail "Expected array of length 3"

parseTuple4 :: Value -> Parser (Double, Double, Double, Double)
parseTuple4 = withArray "4-tuple" $ \arr ->
  if V.length arr == 4
    then
      (,,,)
        <$> parseJSON (arr V.! 0)
        <*> parseJSON (arr V.! 1)
        <*> parseJSON (arr V.! 2)
        <*> parseJSON (arr V.! 3)
    else fail "Expected array of length 4"

instance FromJSON FaceDef where
  parseJSON :: Value -> Parser FaceDef
  parseJSON = withObject "FaceDef" $ \o -> do
    texture <- o .: "texture"
    mUv <- o .:? "uv"
    uv <- case mUv of
      Nothing -> pure Nothing
      Just uvVal -> Just <$> parseTuple4 uvVal
    cullface <- o .:? "cullface"
    tintindex <- o .:? "tintindex"
    pure $ FaceDef uv texture cullface tintindex

instance FromJSON Element where
  parseJSON :: Value -> Parser Element
  parseJSON = withObject "Element" $ \o -> do
    fromVal <- o .: "from"
    from' <- parseTuple3 fromVal
    toVal <- o .: "to"
    to' <- parseTuple3 toVal
    faces <- o .:? "faces" .!= M.empty
    pure $ Element from' to' faces

data BlockModelRaw = BlockModelRaw
  { bmrParent :: !(Maybe T.Text),
    bmrTextures :: !(M.Map T.Text T.Text),
    bmrElements :: ![Element]
  }
  deriving (Show, Eq, Generic)

instance FromJSON BlockModelRaw where
  parseJSON :: Value -> Parser BlockModelRaw
  parseJSON = withObject "BlockModel" $ \o -> do
    parent <- o .:? "parent"
    textures <- o .:? "textures" .!= M.empty
    elements <- o .:? "elements" .!= []
    pure $ BlockModelRaw parent textures elements

loadBlockModel :: FilePath -> IO (Either String BlockModel)
loadBlockModel path = do
  result <- eitherDecodeFileStrict path
  case result of
    Left err -> pure (Left err)
    Right raw -> resolveParent path raw

resolveParent :: FilePath -> BlockModelRaw -> IO (Either String BlockModel)
resolveParent _ (BlockModelRaw Nothing textures elements) =
  pure $ Right $ BlockModel Nothing textures elements
resolveParent currentPath (BlockModelRaw (Just parentRef) childTextures childElements) = do
  let parentPath = resolveParentPath (takeDirectory currentPath) parentRef
  bimap ("Failed to load parent: " <>) (mergeWithParent childTextures childElements) <$> loadBlockModel parentPath
  where
    resolveParentPath dir pRaw =
      let p' = fromMaybe pRaw (T.stripPrefix "minecraft:" pRaw)
          rel = T.unpack p'
          relJson = if ".json" `isSuffixOf` rel then rel else rel ++ ".json"
       in takeDirectory dir </> relJson

    mergeWithParent childTx childEls parent =
      let mergedTextures = childTx `M.union` bmTextures parent
          mergedElements = if null childEls then bmElements parent else childEls
       in BlockModel Nothing mergedTextures mergedElements
