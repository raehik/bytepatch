{-| Code for types which have a binary representation for patching.

I try to stay highly generic, but this is primarily aimed at making changes to
executables. Provided instances and some data decisions reflect that:

  * 'Text' encodes to a null-terminated string.
  * 'Meta' includes support for stripping a null bytestring suffix when testing
    equality.
-}

module BytePatch.Patch.Binary
  ( BinRep(..)
  , Meta(..)
  , Cfg(..)
  , Error(..)
  , check
  , binRep
  ) where

import           GHC.Generics       ( Generic )
import           GHC.Natural
import qualified Data.ByteString    as BS
import qualified Data.Text.Encoding as Text
import           Data.Text          ( Text )

type Bytes = BS.ByteString

-- Conversion checks are done when the type is converted to its binary
-- representation. This is enforced at patch time, but may also be done before
-- patching for safety (in which case the conversion at patch time is 'id').
-- Patch checks use stream data, so must be run at patch time.
data Meta a = Meta
  { mNullTerminates :: Maybe Integer
  -- ^ Stream segment should be null bytes (0x00) only from this index onwards.
  --
  -- Patch check.

  , mExpected       :: Maybe a
  -- ^ Stream segment should be this.
  --
  -- Patch check.

  , mMaxBytes       :: Maybe Natural
  -- ^ Maximum number of bytes permitted to write at the associated position.
  --
  -- Conversion check.

  } deriving (Eq, Show, Generic, Functor, Foldable, Traversable)

data Cfg = Cfg
  { cfgAllowPartialExpected :: Bool
  -- ^ If enabled, allow partial expected bytes checking. If disabled, then even
  --   if the expected bytes are a prefix of the actual, fail.
  } deriving (Eq, Show, Generic)

data Error a
  = ErrorBadBinRep a String -- TODO used for both patch data and expected data
  | ErrorUnexpectedNonNull Bytes
  | ErrorDidNotMatchExpected Bytes Bytes
  | ErrorBinRepTooLong Bytes Natural
    deriving (Eq, Show, Generic, Functor, Foldable, Traversable)

binRep :: BinRep a => a -> Maybe Natural -> Either (Error a) Bytes
binRep a mN =
    case toBinRep a of
      Left  err -> Left $ ErrorBadBinRep a err
      Right bs  ->
        case mN of
          Nothing -> Right bs
          Just n  ->
            if   fromIntegral (BS.length bs) > n
            then Left $ ErrorBinRepTooLong bs n
            else Right bs

check :: BinRep a => Cfg -> Bytes -> Meta a -> Either (Error a) ()
check cfg bs meta = do
    case mExpected meta of
      Nothing -> Right ()
      Just aExpected -> do
        bsExpected <- binRep aExpected Nothing -- cheating a bit here
        case mNullTerminates meta of
          Nothing -> check' bs bsExpected
          Just nullsFrom ->
            let (bs', bsNulls) = BS.splitAt (fromIntegral nullsFrom) bs
             in if   bsNulls == BS.replicate (BS.length bsNulls) 0x00
                then check' bs' bsExpected
                else Left $ ErrorUnexpectedNonNull bs
  where
    check' bs' bsExpected =
        case checkExpected cfg bs' bsExpected of
          True  -> Right ()
          False -> Left $ ErrorDidNotMatchExpected bs' bsExpected

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
