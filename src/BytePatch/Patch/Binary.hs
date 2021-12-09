{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards     #-}

{-| Code for types which have a binary representation for patching.

I try to stay highly generic, but this is primarily aimed at making changes to
executables. Provided instances and some data decisions reflect that:

  * 'Text' encodes to a null-terminated string.
  * 'Meta' includes support for stripping a null bytestring suffix when testing
    equality.
-}

module BytePatch.Patch.Binary
  ( BinRep(..)
  , MetaPatch(..)
  , MetaPos(..)
  , Cfg(..)
  , Error(..)
  , check
  , binRep
  , patchBinRep
  ) where

import           BytePatch.Patch

import           GHC.Generics       ( Generic )
import           GHC.Natural
import qualified Data.ByteString    as BS
import qualified Data.Text.Encoding as Text
import qualified Data.Text          as Text
import           Data.Text          ( Text )
import           Data.Either.Combinators

type Bytes = BS.ByteString

data MetaPatch dd a = MetaPatch
  { mdMaxBytes       :: Maybe (SeekRep 'FwdSeek)
  -- ^ Maximum number of bytes permitted to write at the associated position.

  , mdInner          :: dd a
  } deriving (Eq, Show, Generic, Functor, Foldable, Traversable)

data MetaPos pd a = MetaPos
  { mpNullTerminates :: Maybe (SeekRep 'FwdSeek)
  -- ^ Stream segment should be null bytes (0x00) only from this index onwards.

  , mpExpected       :: Maybe a
  -- ^ Stream segment should be this.

  , mpInner          :: pd a
  } deriving (Eq, Show, Generic, Functor, Foldable, Traversable)

{-
deriving instance (Eq   (SeekRep s), Eq   (cd a), Eq   (ad a)) => Eq   (Pos s cd ad a)
deriving instance (Show (SeekRep s), Show (cd a), Show (ad a)) => Show (Pos s cd ad a)
-}

data Cfg = Cfg
  { cfgAllowPartialExpected :: Bool
  -- ^ If enabled, allow partial expected bytes checking. If disabled, then even
  --   if the expected bytes are a prefix of the actual, fail.
  } deriving (Eq, Show, Generic)

data Error a
  = ErrorBadBinRep a String -- ^ used for patch data and all meta data
  | ErrorUnexpectedNonNull Bytes
  | ErrorDidNotMatchExpected Bytes Bytes
  | ErrorBinRepTooLong Bytes Natural
    deriving (Eq, Show, Generic, Functor, Foldable, Traversable)

patchBinRep
    :: (BinRep a, Traversable dd, Traversable pd)
    => Patch s (MetaPatch dd) (MetaPos pd) a
    -> Either (Error a) (Patch s dd (MetaPos pd) Bytes)
patchBinRep p = do
    patchData <- toBinRep' (patchData p)
    patchMeta <- mdInner <$> traverse toBinRep' (patchMeta p)
    patchPos  <- traverse toBinRep' (patchPos p)
    return Patch{..}
  where
    toBinRep' a = mapLeft (\e -> ErrorBadBinRep a e) $ toBinRep a

--checkBinRep :: BinRep => a -> Either String Bytes

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

check :: BinRep a => Cfg -> Bytes -> MetaPos ad a -> Either (Error a) ()
check cfg bs meta = do
    case mpExpected meta of
      Nothing -> Right ()
      Just aExpected -> do
        bsExpected <- binRep aExpected Nothing -- cheating a bit here
        case mpNullTerminates meta of
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

-- | String is the same but goes the long way round, through Text.
instance BinRep String where
    toBinRep = toBinRep . Text.pack
