module BytePatch.PatchRep where

import qualified Data.ByteString            as BS
import qualified Data.Text.Encoding         as Text
import           Data.Text                  ( Text )

-- | Type has a binary representation for using in patchscripts.
--
-- Patchscripts are parsed parameterized over the type to edit. That type needs
-- to become a bytestring for eventual patch application. We're forced into
-- newtypes and typeclasses by Aeson already, so this just enables us to define
-- some important patch generation behaviour in one place. Similarly to Aeson,
-- if you require custom behaviour for existing types (e.g. length-prefixed
-- strings instead of C-style null terminated), define a newtype over it.
--
-- Some values may not have valid patch representations, for example if you're
-- patching a 1-byte length-prefixed string and your string is too long (>255
-- encoded bytes). Thus, 'toPatchRep' is failable.
class PatchRep a where
    toPatchRep :: a -> Either String BS.ByteString

-- | Bytestrings are copied as-is.
instance PatchRep BS.ByteString where
    toPatchRep = Right . id

-- | Text is converted to UTF-8 bytes and null-terminated.
instance PatchRep Text where
    --toPatchRep t = BS.snoc (Text.encodeUtf8 t) 0x00
    toPatchRep = Right . flip BS.snoc 0x00 . Text.encodeUtf8
