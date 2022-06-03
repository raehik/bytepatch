-- | TODO remove ASAP. figure out Aeson with Vinyl

{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module StreamPatch.Simple where

import StreamPatch.Patch
import StreamPatch.HFunctorList
import StreamPatch.Patch.Binary qualified as Bin
import StreamPatch.Patch.Compare qualified as Compare
import StreamPatch.Patch.Compare ( Via(..), CompareRep )
import StreamPatch.Patch.Align qualified as Align

import Numeric.Natural
import Data.Functor.Const
import Data.Vinyl
import Data.Vinyl.TypeLevel
import Data.Aeson
import GHC.Generics

data MultiPatch s (v :: Via) a = MultiPatch
  { mpData           :: a
  , mpSeek           :: s
  , mpCompare        :: Maybe (CompareRep v a)
  , mpNullTerminates :: Maybe Natural
  , mpMaxBytes       :: Maybe Natural -- TODO confirm meaning, maybe improve name
  , mpAligned        :: Maybe Integer
  } deriving (Generic)

instance Functor (MultiPatch s ('ViaEq ec)) where
    fmap f (MultiPatch d s c n m a) = MultiPatch (f d) s (fmap f c) n m a
instance Functor (MultiPatch s ('ViaDigest h)) where
    fmap f (MultiPatch d s c n m a) = MultiPatch (f d) s c          n m a

deriving instance (Eq   a, Eq   s, Eq   (CompareRep v a)) => Eq   (MultiPatch s v a)
deriving instance (Show a, Show s, Show (CompareRep v a)) => Show (MultiPatch s v a)

data Aligned p = Aligned
  { alignedAlign   :: Integer
  , alignedPatches :: [p]
  } deriving (Eq, Show, Generic)

--------------------------------------------------------------------------------

jsonCfgCamelDrop :: Int -> Options
jsonCfgCamelDrop x = defaultOptions
  { fieldLabelModifier = camelTo2 '_' . drop x
  , rejectUnknownFields = True }

instance (ToJSON   (CompareRep v a), ToJSON   a, ToJSON   s) => ToJSON   (MultiPatch s v a) where
    toJSON     = genericToJSON     $ jsonCfgCamelDrop 2
    toEncoding = genericToEncoding $ jsonCfgCamelDrop 2
instance (FromJSON (CompareRep v a), FromJSON a, FromJSON s) => FromJSON (MultiPatch s v a) where
    parseJSON  = genericParseJSON  $ jsonCfgCamelDrop 2

instance (ToJSON   p) => ToJSON   (Aligned p) where
    toJSON     = genericToJSON     $ jsonCfgCamelDrop 7
    toEncoding = genericToEncoding $ jsonCfgCamelDrop 7
instance (FromJSON p) => FromJSON (Aligned p) where
    parseJSON  = genericParseJSON  $ jsonCfgCamelDrop 7

--------------------------------------------------------------------------------

convert :: (MultiPatch s v a -> Rec (Flap a) fs) -> MultiPatch s v a -> Patch s fs a
convert f p = Patch { patchData = mpData p
                    , patchSeek = mpSeek p
                    , patchMeta = HFunctorList $ f p }

-- TODO how to clean up (it's like liftA2 over n instead of 2. like a fold)
-- likely 'ap'!
convertBinAlign
    :: forall a v
    .  MultiPatch Integer v a
    -> Patch Integer '[Const (Align.Meta Int), Const Bin.MetaPrep, Compare.Meta v, Bin.Meta] a
convertBinAlign = convert go
  where go p = cmAlign p <+> cmBinPrep p <+> cmCompare p <+> cmBin p

convertBin
    :: forall s a v
    .  MultiPatch s v a
    -> Patch s '[Const Bin.MetaPrep, Compare.Meta v, Bin.Meta] a
convertBin p = convert go p
  where go s = cmBinPrep s <+> cmCompare s <+> cmBin s

convertAlign
    :: forall a v
    .  MultiPatch Integer v a
    -> Patch Integer '[Const (Align.Meta Int)] a
convertAlign p = convert cmAlign p

convertEmpty
    :: forall s a v
    .  MultiPatch s v a
    -> Patch s '[] a
convertEmpty p = convert (const RNil) p

--------------------------------------------------------------------------------

align
    :: forall st a ss rs is i r
    .  ( r ~ Const (Align.Meta st)
       , Num st, Eq st
       , rs ~ RDelete r ss
       , RElem r ss i
       , RSubset rs ss is )
    => Aligned (Patch Integer ss a)
    -> Either (Align.Error st) [Patch st rs a]
align (Aligned a ps) = traverse (Align.align a) ps

--------------------------------------------------------------------------------

cmBin :: MultiPatch s c a -> Rec (Flap a) '[Bin.Meta]
cmBin p = cm $ Bin.Meta { Bin.mNullTerminates = mpNullTerminates p }

cmCompare :: MultiPatch s v a -> Rec (Flap a) '[Compare.Meta v]
cmCompare p = cm $ Compare.Meta { Compare.mCompare = mpCompare p }

cmBinPrep :: MultiPatch s c a -> Rec (Flap a) '[Const Bin.MetaPrep]
cmBinPrep p = cm $ Const $ Bin.MetaPrep { Bin.mpMaxBytes = mpMaxBytes p }

cmAlign :: MultiPatch Integer c a -> Rec (Flap a) '[Const (Align.Meta Int)]
cmAlign p = cm $ Const $ Align.Meta { Align.mExpected = fromInteger <$> mpAligned p }

cm :: f a -> Rec (Flap a) '[f]
cm fa = Flap fa :& RNil

--------------------------------------------------------------------------------

-- | Convenience function to wrap a single meta into an 'HFunctorList'.
metaWrap1 :: f a -> HFunctorList '[f] a
metaWrap1 = HFunctorList . cm

-- | Convenience function for the empty 'HFunctorList'.
metaEmpty :: HFunctorList '[] a
metaEmpty = HFunctorList RNil

--------------------------------------------------------------------------------

convertBackBin :: forall v s a. Patch s '[Compare.Meta v, Bin.Meta] a -> MultiPatch s v a
convertBackBin p = MultiPatch { mpData           = patchData p
                              , mpSeek           = patchSeek p
                              , mpCompare        = Compare.mCompare @v $ hflGet $ patchMeta p
                              , mpNullTerminates = Bin.mNullTerminates $ hflGet $ patchMeta p
                              , mpMaxBytes       = Nothing
                              , mpAligned        = Nothing
                              }
