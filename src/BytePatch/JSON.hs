{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | Aeson instances for various types.
module BytePatch.JSON where

import           BytePatch.Core
import qualified BytePatch.Pretty               as Pretty
import           BytePatch.Pretty.HexByteString
import           Data.Aeson
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

instance ToJSON   a => ToJSON   (Pretty.CommonMultiEdits a) where
    toJSON     = genericToJSON     $ jsonCfgCamelDrop 4
    toEncoding = genericToEncoding $ jsonCfgCamelDrop 4
instance FromJSON a => FromJSON (Pretty.CommonMultiEdits a) where
    parseJSON  = genericParseJSON  $ jsonCfgCamelDrop 4

instance ToJSON   a => ToJSON   (Pretty.MultiEdit a) where
    toJSON     = genericToJSON     $ jsonCfgCamelDrop 2
    toEncoding = genericToEncoding $ jsonCfgCamelDrop 2
instance FromJSON a => FromJSON (Pretty.MultiEdit a) where
    parseJSON  = genericParseJSON  $ jsonCfgCamelDrop 2

instance ToJSON   a => ToJSON   (Pretty.EditOffset a) where
    toJSON     = genericToJSON     $ jsonCfgCamelDrop 2
    toEncoding = genericToEncoding $ jsonCfgCamelDrop 2
instance FromJSON a => FromJSON (Pretty.EditOffset a) where
    parseJSON  = genericParseJSON  $ jsonCfgCamelDrop 2

instance ToJSON   a => ToJSON   (EditMeta a) where
    toJSON     = genericToJSON     $ jsonCfgCamelDrop 2
    toEncoding = genericToEncoding $ jsonCfgCamelDrop 2
instance FromJSON a => FromJSON (EditMeta a) where
    parseJSON  = genericParseJSON  $ jsonCfgCamelDrop 2

instance ToJSON   a => ToJSON   (Edit a) where
    toJSON     = genericToJSON     defaultOptions
    toEncoding = genericToEncoding defaultOptions
instance FromJSON a => FromJSON (Edit a) where
    parseJSON  = genericParseJSON  defaultOptions
