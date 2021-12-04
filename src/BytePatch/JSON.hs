{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Aeson instances for various types.
module BytePatch.JSON where

import           BytePatch.Core
import qualified BytePatch.Patch.Binary                 as Bin
import qualified BytePatch.Patch.Binary.HexByteString   as Bin
import qualified BytePatch.Pretty                       as Pretty
import           Data.Aeson
import           Text.Megaparsec
import           Data.Void

instance FromJSON Bin.HexByteString where
    parseJSON = withText "hex bytestring" $ \t ->
        case parseMaybe @Void Bin.parseHexByteString t of
          Nothing -> fail "failed to parse hex bytestring (TODO)"
          Just t' -> pure (Bin.HexByteString t')
instance ToJSON   Bin.HexByteString where
    toJSON = String . Bin.prettyHexByteString . Bin.unHexByteString

jsonCfgCamelDrop :: Int -> Options
jsonCfgCamelDrop x = defaultOptions
  { fieldLabelModifier = camelTo2 '_' . drop x
  , rejectUnknownFields = True }

instance ToJSON   a => ToJSON   (Pretty.CommonMultiEdits a) where
    toJSON     = genericToJSON     $ jsonCfgCamelDrop 4
    toEncoding = genericToEncoding $ jsonCfgCamelDrop 4
instance FromJSON a => FromJSON (Pretty.CommonMultiEdits a) where
    parseJSON  = genericParseJSON  $ jsonCfgCamelDrop 4

instance (ToJSON   (SeekRep s), ToJSON   a) => ToJSON   (Pretty.MultiEdit s a) where
    toJSON     = genericToJSON     $ jsonCfgCamelDrop 2
    toEncoding = genericToEncoding $ jsonCfgCamelDrop 2
instance (FromJSON (SeekRep s), FromJSON a) => FromJSON (Pretty.MultiEdit s a) where
    parseJSON  = genericParseJSON  $ jsonCfgCamelDrop 2

instance (ToJSON   (SeekRep s), ToJSON   a) => ToJSON   (Pretty.EditOffset s a) where
    toJSON     = genericToJSON     $ jsonCfgCamelDrop 2
    toEncoding = genericToEncoding $ jsonCfgCamelDrop 2
instance (FromJSON (SeekRep s), FromJSON a) => FromJSON (Pretty.EditOffset s a) where
    parseJSON  = genericParseJSON  $ jsonCfgCamelDrop 2

instance ToJSON   a => ToJSON   (Bin.Meta a) where
    toJSON     = genericToJSON     $ jsonCfgCamelDrop 1
    toEncoding = genericToEncoding $ jsonCfgCamelDrop 1
instance FromJSON a => FromJSON (Bin.Meta a) where
    parseJSON  = genericParseJSON  $ jsonCfgCamelDrop 1

instance (ToJSON   (m a), ToJSON   a) => ToJSON   (Edit m a) where
    toJSON     = genericToJSON     $ jsonCfgCamelDrop 4
    toEncoding = genericToEncoding $ jsonCfgCamelDrop 4
instance (FromJSON (m a), FromJSON a) => FromJSON (Edit m a) where
    parseJSON  = genericParseJSON  $ jsonCfgCamelDrop 4

instance (ToJSON   (SeekRep s), ToJSON   (m a), ToJSON   a) => ToJSON   (Patch s m a) where
    toJSON     = genericToJSON     $ jsonCfgCamelDrop 5
    toEncoding = genericToEncoding $ jsonCfgCamelDrop 5
instance (FromJSON (SeekRep s), FromJSON (m a), FromJSON a) => FromJSON (Patch s m a) where
    parseJSON  = genericParseJSON  $ jsonCfgCamelDrop 5
