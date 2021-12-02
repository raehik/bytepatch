{-| Newtype for manipulating length-prefixed strings.

This type is for UTF-8 'Text' that you intend to write out to a length-prefixed
bytestring. The size of the length field is static. You essentially have to
decide the maximum bytesize of the string on creation.
-}

{-# LANGUAGE DataKinds, ScopedTypeVariables#-}

module BytePatch.Pretty.PascalText where

import           BytePatch.Pretty.PatchRep

import qualified Data.Text.Encoding         as Text
import           Data.Text                  ( Text )
import qualified Data.ByteString            as BS
import           GHC.TypeLits
import           Data.Proxy
import           Data.Bits

newtype PascalText (n :: Nat) = PascalText { unPascalText :: Text }

instance KnownNat n => PatchRep (PascalText n) where
    toPatchRep t =
        case encodePascalText t of
          Nothing -> Left "UTF-8 encoded text too long for length prefix field"
          Just bs -> Right bs

encodePascalText :: forall n. KnownNat n => PascalText n -> Maybe BS.ByteString
encodePascalText t = do
    lenBs <- encodeToSizedBE (fromIntegral (natVal (Proxy @n))) (BS.length bs)
    return $ lenBs <> bs
  where
    bs = Text.encodeUtf8 . unPascalText $ t

encodeToSizedBE :: (Integral a, Bits a) => Int -> a -> Maybe BS.ByteString
encodeToSizedBE byteSize x =
    let bs = i2be x
        nulls = byteSize - BS.length bs
     in if   nulls < 0
        then Nothing
        else Just $ BS.replicate nulls 0x00 <> bs

-- | Re-encode an 'Integer' to a little-endian integer stored as a 'ByteString'
--   using the fewest bytes needed to represent it.
--
-- adapated from crypto-api 0.13.3, Crypto.Util.i2bs_unsized
i2be :: (Integral a, Bits a) => a -> BS.ByteString
i2be 0 = BS.singleton 0
i2be i = BS.reverse $ BS.unfoldr (\i' -> if i' <= 0 then Nothing else Just (fromIntegral i', (i' `shiftR` 8))) i

-- 0 -> 255 = 1
-- 256 -> 65535 = 2

ptLengthBytes :: forall n. KnownNat n => PascalText n -> Integer
ptLengthBytes _ = natVal (Proxy @n)
