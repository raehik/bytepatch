{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

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

import StreamPatch.Patch.Compare qualified as Compare
import StreamPatch.Patch.Compare hiding ( Meta )

import Optics

data Meta a = Meta
  { mNullTerminates :: Maybe Natural
  -- ^ Stream segment should be null bytes (0x00) only from this index onwards.
  } deriving (Generic, Eq, Show, Functor, Foldable, Traversable)

data MetaPrep = MetaPrep
  { mpMaxBytes :: Maybe Natural
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

-- | Treat the nulls field as a "this is how many extra nulls there are", and
--   amend the compare meta for a patch by appending those nulls, and strip that
--   stream-time bin meta.
--
-- Correct thing to do, but needs lots of changes elsewhere too. Dang.
binRepifyNull
    :: forall ec s r1 r2 ss rs is
    .  ( r1 ~ Meta
       , r2 ~ Compare.Meta ('ViaEq ec)
       , rs ~ RDelete r1 ss
       , RElem r1 ss (RIndex r1 ss)
       , RSubset rs ss is
       , RElem r2 rs (RIndex r2 rs)
       , RecElem Rec r2 r2 rs rs (RIndex r2 rs)
       )
    => Patch s ss BS.ByteString
    -> Patch s rs BS.ByteString
binRepifyNull (Patch a s ms) = do
    let (mNT, ms') = hflStrip @r1 mNullTerminates ms
    case mNT of
      Nothing -> Patch a s ms'
      Just nt ->
        let mCmpLens = hflLens @r2 @r2 @rs @rs
            ms'' = over mCmpLens (\(Compare.Meta x) -> go nt x) ms'
         in Patch a s ms''
  where
    go nt = \case
      Nothing  -> Compare.Meta Nothing
      Just cmp ->
        let cmp' = cmp <> BS.replicate (fromIntegral nt) 0x00
         in Compare.Meta $ Just cmp'
