{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | Aeson instances for various types.
module BytePatch.JSON where

import           BytePatch.Core
import qualified BytePatch.Patch.Binary                 as Bin
import qualified BytePatch.Patch.Binary.HexByteString   as Bin
import qualified BytePatch.Meta.Align                   as Meta
import           Data.Aeson
import           Text.Megaparsec                        ( parseMaybe )
import           Data.Void

jsonCfgCamelDrop :: Int -> Options
jsonCfgCamelDrop x = defaultOptions
  { fieldLabelModifier = camelTo2 '_' . drop x
  , rejectUnknownFields = True }

instance (ToJSON   (SeekRep s), ToJSON   (d a), ToJSON   a) => ToJSON   (Patch s d a) where
    toJSON     = genericToJSON     $ jsonCfgCamelDrop 5
    toEncoding = genericToEncoding $ jsonCfgCamelDrop 5
instance (FromJSON (SeekRep s), FromJSON (d a), FromJSON a) => FromJSON (Patch s d a) where
    parseJSON  = genericParseJSON  $ jsonCfgCamelDrop 5

instance (ToJSON   (SeekRep s), ToJSON   a) => ToJSON   (Pos s a) where
    toJSON     = genericToJSON     $ jsonCfgCamelDrop 3
    toEncoding = genericToEncoding $ jsonCfgCamelDrop 3
instance (FromJSON (SeekRep s), FromJSON a) => FromJSON (Pos s a) where
    parseJSON  = genericParseJSON  $ jsonCfgCamelDrop 3

instance (ToJSON   (SeekRep s), ToJSON   (d a), ToJSON   a) => ToJSON   (MultiPatch s d a) where
    toJSON     = genericToJSON     $ jsonCfgCamelDrop 10
    toEncoding = genericToEncoding $ jsonCfgCamelDrop 10
instance (FromJSON (SeekRep s), FromJSON (d a), FromJSON a) => FromJSON (MultiPatch s d a) where
    parseJSON  = genericParseJSON  $ jsonCfgCamelDrop 10

instance ToJSON   a => ToJSON   (Bin.Meta a) where
    toJSON     = genericToJSON     $ jsonCfgCamelDrop 1
    toEncoding = genericToEncoding $ jsonCfgCamelDrop 1
instance FromJSON a => FromJSON (Bin.Meta a) where
    parseJSON  = genericParseJSON  $ jsonCfgCamelDrop 1

instance (ToJSON   (d a), ToJSON   a) => ToJSON   (Meta.Align d a) where
    toJSON     = genericToJSON     $ jsonCfgCamelDrop 5
    toEncoding = genericToEncoding $ jsonCfgCamelDrop 5
instance (FromJSON (d a), FromJSON a) => FromJSON (Meta.Align d a) where
    parseJSON  = genericParseJSON  $ jsonCfgCamelDrop 5

instance FromJSON Bin.HexByteString where
    parseJSON = withText "hex bytestring" $ \t ->
        case parseMaybe @Void Bin.parseHexByteString t of
          Nothing -> fail "failed to parse hex bytestring (TODO)"
          Just t' -> pure (Bin.HexByteString t')
instance ToJSON   Bin.HexByteString where
    toJSON = String . Bin.prettyHexByteString . Bin.unHexByteString
