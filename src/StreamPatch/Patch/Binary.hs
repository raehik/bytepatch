{-# LANGUAGE RecordWildCards #-}

-- TODO rewrite patch/check bits (some overlap)
-- TODO rewrite, rename checks (one for process, one for apply)

module StreamPatch.Patch.Binary
  ( Meta(..)
  , MetaStream(..)
  , Cfg(..)
  , Error(..)
  , patchBinRep
  , BinRep(..)
  , toBinRep'
  , check
  ) where

import           StreamPatch.Patch
import           StreamPatch.HFunctorList

import           GHC.Generics       ( Generic )
import           GHC.Natural
import qualified Data.ByteString    as BS
import qualified Data.Text.Encoding as Text
import qualified Data.Text          as Text
import           Data.Text          ( Text )
import           Data.Either.Combinators
import           Data.Vinyl
import           Data.Functor.Const
import           Data.Vinyl.TypeLevel

data Meta = Meta
  { mMaxBytes :: Maybe (SeekRep 'FwdSeek)
  -- ^ Maximum number of bytes permitted to write at the associated position.
  } deriving (Eq, Show, Generic)

data MetaStream a = MetaStream
  { msNullTerminates :: Maybe (SeekRep 'FwdSeek)
  -- ^ Stream segment should be null bytes (0x00) only from this index onwards.

  , msExpected       :: Maybe a
  -- ^ Stream segment should be this.
  } deriving (Eq, Show, Generic, Functor, Foldable, Traversable)

data Cfg = Cfg
  { cfgAllowPartialExpected :: Bool
  -- ^ If enabled, allow partial expected bytes checking. If disabled, then even
  --   if the expected bytes are a prefix of the actual, fail.
  } deriving (Eq, Show, Generic)

data Error a
  = ErrorBadBinRep a String
  | ErrorUnexpectedNonNull BS.ByteString
  | ErrorDidNotMatchExpected BS.ByteString BS.ByteString
  | ErrorBinRepTooLong BS.ByteString Natural
    deriving (Eq, Show, Generic, Functor, Foldable, Traversable)

patchBinRep
    :: forall a s ss i is r rs
    .  ( BinRep a
       , Traversable (HFunctorList rs)
       , r ~ Const Meta
       , rs ~ RDelete r ss
       , RElem r ss i
       , RSubset rs ss is )
    => Patch s ss a
    -> Either (Error a) (Patch s rs BS.ByteString)
patchBinRep (Patch a s ms) = do
    a' <- toBinRep' a
    let (metaCheck, ms') = hflStrip (checkMeta a' . getConst) ms
    metaCheck
    ms'' <- traverse toBinRep' ms'
    return $ Patch a' s ms''
  where
    checkMeta a' m = case mMaxBytes m of
                       Nothing       -> return ()
                       Just maxBytes ->
                         if   BS.length a' > fromIntegral maxBytes
                         then Left $ ErrorBinRepTooLong a' maxBytes
                         else return ()

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
    toBinRep :: a -> Either String BS.ByteString

toBinRep' :: BinRep a => a -> Either (Error a) BS.ByteString
toBinRep' a = mapLeft (ErrorBadBinRep a) $ toBinRep a

-- | Bytestrings are copied as-is.
instance BinRep BS.ByteString where
    toBinRep = Right . id

-- | Text is converted to UTF-8 bytes and null-terminated.
instance BinRep Text where
    toBinRep = Right . flip BS.snoc 0x00 . Text.encodeUtf8

-- | String is the same but goes the long way round, through Text.
instance BinRep String where
    toBinRep = toBinRep . Text.pack

check :: BinRep a => Cfg -> BS.ByteString -> MetaStream a -> Either (Error a) ()
check cfg bs meta = do
    case msExpected meta of
      Nothing -> Right ()
      Just aExpected -> do
        bsExpected <- checkInner aExpected Nothing -- cheating a bit here
        case msNullTerminates meta of
          Nothing -> check' bs bsExpected
          Just nullsFrom ->
            let (bs', bsNulls) = BS.splitAt (fromIntegral nullsFrom) bs
             in if   bsNulls == BS.replicate (BS.length bsNulls) 0x00
                then check' bs' bsExpected
                else Left $ ErrorUnexpectedNonNull bs
  where
    check' bs' bsExpected =
        case checkExpected bs' bsExpected of
          True  -> Right ()
          False -> Left $ ErrorDidNotMatchExpected bs' bsExpected
    checkInner a mn = do
        bs' <- toBinRep' a
        case mn of
          Nothing -> Right bs'
          Just n  ->
            if   fromIntegral (BS.length bs') > n
            then Left $ ErrorBinRepTooLong bs' n
            else Right bs'
    checkExpected bs' bsExpected =
        case cfgAllowPartialExpected cfg of
          True  -> BS.isPrefixOf bs' bsExpected
          False -> bs' == bsExpected
