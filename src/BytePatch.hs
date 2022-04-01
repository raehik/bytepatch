{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module BytePatch where

import BytePatch.Orphans()
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

data MultiPatch (s :: SeekKind) (v :: Via) a = MultiPatch
  { mpData :: a
  , mpAt :: [Seek s v a]
  } deriving (Generic)

instance Functor     (Seek s v) => Functor     (MultiPatch s v) where
    fmap f (MultiPatch d a) = MultiPatch (f d) (map (fmap f) a)

deriving instance (Eq   a, Eq   (SeekRep s), Eq   (CompareRep v a)) => Eq   (MultiPatch s v a)
deriving instance (Show a, Show (SeekRep s), Show (CompareRep v a)) => Show (MultiPatch s v a)

data Seek (s :: SeekKind) (v :: Via) a = Seek
  { sSeek           :: SeekRep s
  , sCompare        :: Maybe (CompareRep v a)
  , sNullTerminates :: Maybe (SeekRep 'FwdSeek)
  , sMaxBytes       :: Maybe (SeekRep 'FwdSeek) -- TODO confirm meaning, maybe improve name
  , sAligned        :: Maybe Natural
  } deriving (Generic)

instance Functor     (Seek s ('ViaEq p)) where
    fmap f (Seek s c n m a) = Seek s (fmap f c) n m a
instance Functor     (Seek s ('ViaHash h)) where
    fmap _ (Seek s c n m a) = Seek s c n m a

deriving instance (Eq   a, Eq   (SeekRep s), Eq   (CompareRep v a)) => Eq   (Seek s v a)
deriving instance (Show a, Show (SeekRep s), Show (CompareRep v a)) => Show (Seek s v a)

-- TODO write a Text newtype for Haskell-style negated integers -- allowing
-- negative hex integers!
data Aligned p = Aligned
  { alignedAlign   :: SeekRep 'RelSeek
  , alignedPatches :: [p]
  } deriving (Eq, Show, Generic)

--------------------------------------------------------------------------------

jsonCfgCamelDrop :: Int -> Options
jsonCfgCamelDrop x = defaultOptions
  { fieldLabelModifier = camelTo2 '_' . drop x
  , rejectUnknownFields = True }

instance (ToJSON   (SeekRep s), ToJSON   (CompareRep v a), ToJSON   a) => ToJSON   (MultiPatch s v a) where
    toJSON     = genericToJSON     $ jsonCfgCamelDrop 2
    toEncoding = genericToEncoding $ jsonCfgCamelDrop 2
instance (FromJSON (SeekRep s), FromJSON (CompareRep v a), FromJSON a) => FromJSON (MultiPatch s v a) where
    parseJSON  = genericParseJSON  $ jsonCfgCamelDrop 2

instance (ToJSON   (SeekRep s), ToJSON   (CompareRep v a), ToJSON   a) => ToJSON   (Seek s v a) where
    toJSON     = genericToJSON     $ jsonCfgCamelDrop 1
    toEncoding = genericToEncoding $ jsonCfgCamelDrop 1
instance (FromJSON (SeekRep s), FromJSON (CompareRep v a), FromJSON a) => FromJSON (Seek s v a) where
    parseJSON  = genericParseJSON  $ jsonCfgCamelDrop 1

instance (ToJSON   p) => ToJSON   (Aligned p) where
    toJSON     = genericToJSON     $ jsonCfgCamelDrop 7
    toEncoding = genericToEncoding $ jsonCfgCamelDrop 7
instance (FromJSON p) => FromJSON (Aligned p) where
    parseJSON  = genericParseJSON  $ jsonCfgCamelDrop 7

--------------------------------------------------------------------------------

convert :: (Seek s v a -> Rec (Flap a) fs) -> MultiPatch s v a -> [Patch s fs a]
convert f p = map go (mpAt p)
  where go s = Patch { patchData = mpData p
                     , patchSeek = sSeek s
                     , patchMeta = HFunctorList $ f s }

-- TODO how to clean up (it's like liftA2 over n instead of 2. like a fold)
-- likely 'ap'!
convertBinAlign
    :: forall s a v
    .  SeekRep s ~ Natural
    => MultiPatch 'RelSeek v a
    -> [Patch 'RelSeek '[Const (Align.Meta s), Const Bin.MetaPrep, Compare.Meta v, Bin.Meta] a]
convertBinAlign p = convert go p
  where go s = cmAlign s <+> cmBinPrep s <+> cmCompare s <+> cmBin s

convertBin
    :: forall s a v
    .  MultiPatch s v a
    -> [Patch s '[Const Bin.MetaPrep, Compare.Meta v, Bin.Meta] a]
convertBin p = convert go p
  where go s = cmBinPrep s <+> cmCompare s <+> cmBin s

convertAlign
    :: forall s a v
    .  SeekRep s ~ Natural
    => MultiPatch 'RelSeek v a
    -> [Patch 'RelSeek '[Const (Align.Meta s)] a]
convertAlign p = convert cmAlign p

convertEmpty
    :: forall s a v
    .  MultiPatch s v a
    -> [Patch s '[] a]
convertEmpty p = convert (const RNil) p

--------------------------------------------------------------------------------

align
    :: forall s a ss rs is i r
    .  ( SeekRep s ~ Natural
       , r ~ Const (Align.Meta s)
       , rs ~ RDelete r ss
       , RElem r ss i
       , RSubset rs ss is )
    => Aligned (Patch 'RelSeek ss a)
    -> Either (Align.Error s) [Patch s rs a]
align (Aligned a ps) = traverse (Align.align a) ps

--------------------------------------------------------------------------------

cmBin :: Seek s c a -> Rec (Flap a) '[Bin.Meta]
cmBin s = cm $ Bin.Meta { Bin.mNullTerminates = sNullTerminates s }

cmCompare :: Seek s v a -> Rec (Flap a) '[Compare.Meta v]
cmCompare s = cm $ Compare.Meta { Compare.mCompare = sCompare s }

cmBinPrep :: Seek s c a -> Rec (Flap a) '[Const Bin.MetaPrep]
cmBinPrep s = cm $ Const $ Bin.MetaPrep { Bin.mpMaxBytes = sMaxBytes s }

cmAlign :: SeekRep s ~ Natural => Seek 'RelSeek c a -> Rec (Flap a) '[Const (Align.Meta s)]
cmAlign s = cm $ Const $ Align.Meta { Align.mExpected = sAligned s }

cm :: f a -> Rec (Flap a) '[f]
cm fa = Flap fa :& RNil

--------------------------------------------------------------------------------

convertBackBin :: forall v s a. Patch s '[Compare.Meta v, Bin.Meta] a -> MultiPatch s v a
convertBackBin p = MultiPatch { mpData = patchData p
                              , mpAt   = [s] }
  where s = Seek { sSeek    = patchSeek p
                 , sCompare        = Compare.mCompare @v $ hflGet $ patchMeta p
                 , sNullTerminates = Bin.mNullTerminates $ hflGet $ patchMeta p
                 , sMaxBytes       = Nothing
                 , sAligned        = Nothing
                 }
