{-# LANGUAGE RecordWildCards #-}

-- TODO rewrite patch/check bits (some overlap)
-- TODO rewrite, rename checks (one for process, one for apply)

module StreamPatch.Patch.Binary where

import StreamPatch.Patch
import StreamPatch.HFunctorList

import Binrep

import GHC.Generics       ( Generic )
import GHC.Natural
import Data.ByteString qualified as BS
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
  = ErrorBinRepOverlong Natural Natural a (Maybe BS.ByteString)
  -- ^ If the value was serialized, it's given in the 'Maybe'.
    deriving (Eq, Show, Generic, Functor, Foldable, Traversable)

-- Note that via binrep's 'BLen' typeclass, we can check sizing "for free",
-- without serializing. TODO, I could make another function that serializes,
-- then checks. That way, we can return the serialization attempt to the user,
-- and not require 'BLen'. In exchange, it's less efficient on errors. Both have
-- the same main path complexity.
binRepify
    :: forall a s ss is r rs
    .  ( Put a, BLen a
       , Traversable (HFunctorList rs)
       , r ~ Const MetaPrep
       , rs ~ RDelete r ss
       , RElem r ss (RIndex r ss)
       , RSubset rs ss is )
    => Patch s ss a
    -> Either (Error a) (Patch s rs BS.ByteString)
binRepify (Patch a s ms) = do
    let (m, ms') = hflStrip getConst ms
    checkMeta m
    let bs = runPut a
        bsms = fmap runPut ms'
    return $ Patch bs s bsms
  where
    checkMeta m =
        case mpMaxBytes m of
          Nothing       -> return ()
          Just maxBytes ->
            if   blen a > maxBytes
            then Left $ ErrorBinRepOverlong (blen a) maxBytes a Nothing
            else return ()
