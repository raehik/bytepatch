{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | Aeson instances for various types.
module BytePatch.JSON where

import           BytePatch.Core
import           BytePatch.Pretty
import           BytePatch.Pretty.HexByteString
import           Data.Aeson
import           GHC.Generics       (Generic)
import           Text.Megaparsec
import           Data.Void

instance FromJSON HexByteString where
    parseJSON = withText "hex bytestring" $ \t ->
        case parseMaybe @Void parseHexByteString t of
          Nothing -> fail "failed to parse hex bytestring (TODO)"
          Just t' -> pure (HexByteString t')
instance ToJSON   HexByteString where
    toJSON = String . prettyHexByteString . unHexByteString

jsonCfgCamelDrop :: Int -> Options
jsonCfgCamelDrop x = defaultOptions
  { fieldLabelModifier = camelTo2 '_' . drop x
  , rejectUnknownFields = True }

instance ToJSON   a => ToJSON   (MultiPatches a) where
    toJSON     = genericToJSON     (jsonCfgCamelDrop 3)
    toEncoding = genericToEncoding (jsonCfgCamelDrop 3)
instance FromJSON a => FromJSON (MultiPatches a) where
    parseJSON  = genericParseJSON  (jsonCfgCamelDrop 3)

instance ToJSON   a => ToJSON   (MultiPatch a) where
    toJSON     = genericToJSON     (jsonCfgCamelDrop 2)
    toEncoding = genericToEncoding (jsonCfgCamelDrop 2)
instance FromJSON a => FromJSON (MultiPatch a) where
    parseJSON  = genericParseJSON  (jsonCfgCamelDrop 2)

instance ToJSON   a => ToJSON   (Offset a) where
    toJSON     = genericToJSON     (jsonCfgCamelDrop 1)
    toEncoding = genericToEncoding (jsonCfgCamelDrop 1)
instance FromJSON a => FromJSON (Offset a) where
    parseJSON  = genericParseJSON  (jsonCfgCamelDrop 1)

deriving instance Generic (OverwriteMeta a)
instance ToJSON   a => ToJSON   (OverwriteMeta a) where
    toJSON     = genericToJSON     (jsonCfgCamelDrop 2)
    toEncoding = genericToEncoding (jsonCfgCamelDrop 2)
instance FromJSON a => FromJSON (OverwriteMeta a) where
    parseJSON  = genericParseJSON  (jsonCfgCamelDrop 2)

deriving instance Generic (Overwrite a)
instance ToJSON   a => ToJSON   (Overwrite a) where
    toJSON     = genericToJSON     defaultOptions
    toEncoding = genericToEncoding defaultOptions
instance FromJSON a => FromJSON (Overwrite a) where
    parseJSON  = genericParseJSON  defaultOptions
