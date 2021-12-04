{- |
A 'Data.ByteString.ByteString' newtype wrapper indicating a human-readable
bytestring, to be displayed in hex form (e.g. 00 12 AB FF).
-}

{-# LANGUAGE TypeFamilies #-}

module BytePatch.Pretty.HexByteString
  ( HexByteString(..)
  , parseHexByteString
  , prettyHexByteString
  ) where

import           BytePatch.PatchRep

import           Text.Megaparsec
import qualified Text.Megaparsec.Char       as MC
import qualified Data.ByteString            as BS
import qualified Data.Char                  as Char
import           Data.Word
import qualified Data.Text                  as Text
import           Data.Text                  ( Text )
import           Data.List                  as List

type Bytes = BS.ByteString

newtype HexByteString = HexByteString { unHexByteString :: Bytes }
    deriving (Eq)

instance Show HexByteString where
    show = Text.unpack . prettyHexByteString . unHexByteString

instance PatchRep HexByteString where
    toPatchRep = Right . unHexByteString

-- | A hex bytestring looks like this: @00 01 89 8a   FEff@. You can mix and
-- match capitalization and spacing, but I prefer to space each byte, full caps.
parseHexByteString :: (MonadParsec e s m, Token s ~ Char) => m Bytes
parseHexByteString = BS.pack <$> parseHexByte `sepBy` MC.hspace

-- | Parse a byte formatted as two hex digits e.g. EF. You _must_ provide both
-- nibbles e.g. @0F@, not @F@. They cannot be spaced e.g. @E F@ is invalid.
--
-- Returns a value 0-255, so can fit in any Num type that can store that.
parseHexByte :: (MonadParsec e s m, Token s ~ Char, Num a) => m a
parseHexByte = do
    c1 <- MC.hexDigitChar
    c2 <- MC.hexDigitChar
    return $ 0x10 * fromIntegral (Char.digitToInt c1) + fromIntegral (Char.digitToInt c2)

prettyHexByteString :: Bytes -> Text
prettyHexByteString =
    Text.concat . List.intersperse (Text.singleton ' ') . fmap (f . prettyHexByte) . BS.unpack
  where
    f :: (Char, Char) -> Text
    f (c1, c2) = Text.cons c1 $ Text.singleton c2

prettyHexByte :: Word8 -> (Char, Char)
prettyHexByte w = (prettyNibble h, prettyNibble l)
  where
    (h,l) = fromIntegral w `divMod` 0x10
    prettyNibble = Char.toUpper . Char.intToDigit
