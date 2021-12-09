{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | Aeson instances for various types.
module BytePatch.JSON where

import           BytePatch.Patch
import qualified BytePatch.Patch.Binary                 as Bin
import qualified BytePatch.Patch.Binary.HexByteString   as Bin
import qualified BytePatch.Patch.Align                  as Align
import           Data.Aeson
import           Text.Megaparsec                        ( parseMaybe )
import           Data.Void

jsonCfgCamelDrop :: Int -> Options
jsonCfgCamelDrop x = defaultOptions
  { fieldLabelModifier = camelTo2 '_' . drop x
  , rejectUnknownFields = True }

instance (ToJSON   (SeekRep s), ToJSON   (dd a), ToJSON   (pd a), ToJSON   a) => ToJSON   (Patch s dd pd a) where
    toJSON     = genericToJSON     $ jsonCfgCamelDrop 5
    toEncoding = genericToEncoding $ jsonCfgCamelDrop 5
instance (FromJSON (SeekRep s), FromJSON (dd a), FromJSON (pd a), FromJSON a) => FromJSON (Patch s dd pd a) where
    parseJSON  = genericParseJSON  $ jsonCfgCamelDrop 5

instance (ToJSON   (SeekRep s), ToJSON   (pd a), ToJSON   a) => ToJSON   (Pos s pd a) where
    toJSON     = genericToJSON     $ jsonCfgCamelDrop 3
    toEncoding = genericToEncoding $ jsonCfgCamelDrop 3
instance (FromJSON (SeekRep s), FromJSON (pd a), FromJSON a) => FromJSON (Pos s pd a) where
    parseJSON  = genericParseJSON  $ jsonCfgCamelDrop 3

instance (ToJSON   (dd a), ToJSON   a) => ToJSON   (Bin.MetaPatch dd a) where
    toJSON     = genericToJSON     $ jsonCfgCamelDrop 2
    toEncoding = genericToEncoding $ jsonCfgCamelDrop 2
instance (FromJSON (dd a), FromJSON a) => FromJSON (Bin.MetaPatch dd a) where
    parseJSON  = genericParseJSON  $ jsonCfgCamelDrop 2

instance (ToJSON   (pd a), ToJSON   a) => ToJSON   (Bin.MetaPos pd a) where
    toJSON     = genericToJSON     $ jsonCfgCamelDrop 2
    toEncoding = genericToEncoding $ jsonCfgCamelDrop 2
instance (FromJSON (pd a), FromJSON a) => FromJSON (Bin.MetaPos pd a) where
    parseJSON  = genericParseJSON  $ jsonCfgCamelDrop 2

instance (ToJSON   (SeekRep s), ToJSON   (pd a), ToJSON   a) => ToJSON   (Align.MetaPos s pd a) where
    toJSON     = genericToJSON     $ jsonCfgCamelDrop 2
    toEncoding = genericToEncoding $ jsonCfgCamelDrop 2
instance (FromJSON (SeekRep s), FromJSON (pd a), FromJSON a) => FromJSON (Align.MetaPos s pd a) where
    parseJSON  = genericParseJSON  $ jsonCfgCamelDrop 2

instance FromJSON Bin.HexByteString where
    parseJSON = withText "hex bytestring" $ \t ->
        case parseMaybe @Void Bin.parseHexByteString t of
          Nothing -> fail "failed to parse hex bytestring (TODO)"
          Just t' -> pure (Bin.HexByteString t')
instance ToJSON   Bin.HexByteString where
    toJSON = String . Bin.prettyHexByteString . Bin.unHexByteString
