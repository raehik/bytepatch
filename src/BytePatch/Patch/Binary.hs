module BytePatch.Patch.Binary
  ( BinRep(..)
  , Meta(..)
  , Cfg(..)
  , Error(..)
  , check
  ) where

import           GHC.Generics       ( Generic )

import qualified Data.ByteString    as BS
import qualified Data.Text.Encoding as Text
import           Data.Text          ( Text )

type Bytes = BS.ByteString

data Error a
  = ErrorBadBinRep a String -- TODO used for both patch data and expected data
  | ErrorUnexpectedNonNull Bytes
  | ErrorDidNotMatchExpected Bytes Bytes
    deriving (Eq, Show, Generic, Functor, Foldable, Traversable)

data Meta a = Meta
  { mNullTerminates :: Maybe Int
  -- ^ Stream segment should be null bytes (0x00) only from this index onwards.

  , mExpected       :: Maybe a
  -- ^ Stream segment should be this.

  } deriving (Eq, Show, Generic, Functor, Foldable, Traversable)

data Cfg = Cfg
  { cfgAllowPartialExpected :: Bool
  -- ^ If enabled, allow partial expected bytes checking. If disabled, then even
  --   if the expected bytes are a prefix of the actual, fail.
  } deriving (Eq, Show, Generic)

check :: BinRep a => Cfg -> Bytes -> Meta a -> Maybe (Error a)
check cfg bs meta =
    case mExpected meta of
      Nothing -> Nothing
      Just aExpected ->
        case toBinRep aExpected of
          Left  errStr -> Just $ ErrorBadBinRep aExpected errStr
          Right bsExpected     ->
            case mNullTerminates meta of
              Nothing -> check' bs bsExpected
              Just nullsFrom ->
                let (bs', bsNulls) = BS.splitAt nullsFrom bs
                 in if   bsNulls == BS.replicate (BS.length bsNulls) 0x00
                    then check' bs' bsExpected
                    else Just $ ErrorUnexpectedNonNull bs
  where
    check' bs' bsExpected =
        case checkExpected cfg bs' bsExpected of
          True  -> Nothing
          False -> Just $ ErrorDidNotMatchExpected bs' bsExpected

checkExpected :: Cfg -> Bytes -> Bytes -> Bool
checkExpected cfg bs bsExpected =
    case cfgAllowPartialExpected cfg of
      True  -> BS.isPrefixOf bs bsExpected
      False -> bs == bsExpected

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
class BinRep a where
    toBinRep :: a -> Either String Bytes

-- | Bytestrings are copied as-is.
instance BinRep BS.ByteString where
    toBinRep = Right . id

-- | Text is converted to UTF-8 bytes and null-terminated.
instance BinRep Text where
    toBinRep = Right . flip BS.snoc 0x00 . Text.encodeUtf8
