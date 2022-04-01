-- | Workaround to allow me to place modules where I want.

{-# OPTIONS_GHC -fno-warn-orphans #-}

module BytePatch.Orphans where

import           StreamPatch.Patch.Binary   ( BinRep(..) )
import           Raehik.HexBytestring
import           Data.Void
import           Text.Megaparsec            ( parseMaybe )
import           Data.Aeson

instance BinRep HexBytestring where
    toBinRep = unHexBytestring

instance FromJSON HexBytestring where
    parseJSON = withText "hex bytestring" $ \t ->
        case parseMaybe @Void parseHexBytestring t of
          Nothing -> fail "failed to parse hex bytestring (TODO)"
          Just t' -> pure (HexBytestring t')
instance ToJSON   HexBytestring where
    toJSON = String . prettyHexBytestring . unHexBytestring
