{-# LANGUAGE RecordWildCards #-}

-- TODO rewrite patch/check bits (some overlap)
-- TODO rewrite, rename checks (one for process, one for apply)

module StreamPatch.Patch.Binary
  ( Meta(..)
  , MetaPrep(..)
  , Error(..)
  , BinRep(..)
  , binRepify
  ) where

import StreamPatch.Patch
import StreamPatch.HFunctorList

import GHC.Generics       ( Generic )
import GHC.Natural
import Data.ByteString qualified as BS
import Data.Text.Encoding qualified as Text
import Data.Text qualified as Text
import Data.Text ( Text )
import Data.Vinyl
import Data.Functor.Const
import Data.Vinyl.TypeLevel

data Meta a = Meta
  { mNullTerminates :: Maybe (SeekRep 'FwdSeek)
  -- ^ Stream segment should be null bytes (0x00) only from this index onwards.
  } deriving (Generic, Eq, Show, Functor, Foldable, Traversable)

data MetaPrep = MetaPrep
  { mpMaxBytes :: Maybe (SeekRep 'FwdSeek)
  -- ^ Maximum bytelength of binrepped data.
  --
  -- Though binrepping is a safe operation, this is a useful sanity check in
  -- cases where you know the maximum space available.
  --
  -- Note that this is only available for the patch data, not other meta data.
  -- (If you want that, you'll need to shove this field into the patch type.)
  -- itself. Probably not very useful.)
  } deriving (Eq, Show, Generic)

data Error a
  = ErrorBinRepOverlong Natural Natural a BS.ByteString
    deriving (Eq, Show, Generic, Functor, Foldable, Traversable)

binRepify
    :: forall a s ss is r rs
    .  ( BinRep a
       , Traversable (HFunctorList rs)
       , r ~ Const MetaPrep
       , rs ~ RDelete r ss
       , RElem r ss (RIndex r ss)
       , RSubset rs ss is )
    => Patch s ss a
    -> Either (Error a) (Patch s rs BS.ByteString)
binRepify (Patch a s ms) = do
    let bs = toBinRep a
        (metaCheck, ms') = hflStrip (checkMeta bs . getConst) ms
    metaCheck
    let ms'' = fmap toBinRep ms'
    return $ Patch bs s ms''
  where
    checkMeta bs m =
        case mpMaxBytes m of
          Nothing       -> return ()
          Just maxBytes ->
            if   BS.length bs > fromIntegral maxBytes
            then Left $ ErrorBinRepOverlong (fromIntegral (BS.length bs)) maxBytes a bs
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
--
-- TODO make this @Monad m => BinRep m a@ where @m@ specifies context. Pure
-- non-failing can be @m@, pure failing can be @MonadError String m@,
-- impure failing can be @(MonadIO m, MonadError String)@.
--
-- TODO No! This should NOT be failable! My refined stuff from gtvm-hs will fix
-- this. I do everything in data, and redefine this class as pure convenience.
class BinRep a where
    toBinRep   :: a -> BS.ByteString
    fromBinRep :: BS.ByteString -> Maybe a
    fromBinRep = const Nothing
    -- ^ Attempt to recover the "original" value from a bytestring.
    --
    -- Intended to be used for debugging. Our check functions compare converted
    -- bytestrings, not @a@s. This should only be used if a bytestring
    -- comparison fails, to recover a more human-readable error. (This is why we
    -- only ask for a @Maybe@ -- we don't really care if the de-conversion
    -- fails, so we just say "sorry" if it does.
    --
    -- To explain another way, to is printing (1), from is parsing (0-many).

-- | Bytestrings are copied as-is.
instance BinRep BS.ByteString where
    toBinRep = id

-- | Text is converted to UTF-8 bytes and null-terminated.
instance BinRep Text where
    toBinRep = flip BS.snoc 0x00 . Text.encodeUtf8

-- | String is the same but goes the long way round, through Text.
instance BinRep String where
    toBinRep = toBinRep . Text.pack
