{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module BytePatch where

import           BytePatch.Orphans()
import           StreamPatch.Patch
import           StreamPatch.HFunctorList
import qualified StreamPatch.Patch.Binary   as Bin
import qualified StreamPatch.Patch.Align    as Align

import           Numeric.Natural
import           Data.Functor.Const
import           Data.Vinyl
import           Data.Vinyl.TypeLevel
import           Data.Aeson
import           GHC.Generics

data MultiPatch s a = MultiPatch
  { mpData :: a
  , mpAt :: [Seek s a]
  } deriving (Generic)

deriving instance (Eq   a, Eq   (SeekRep s)) => Eq   (MultiPatch s a)
deriving instance (Show a, Show (SeekRep s)) => Show (MultiPatch s a)

data Seek s a = Seek
  { sSeek :: SeekRep s
  , sExpected :: Maybe a
  , sNullTerminates :: Maybe (SeekRep 'FwdSeek)
  , sMaxBytes :: Maybe (SeekRep 'FwdSeek) -- TODO confirm meaning, maybe improve name
  , sAligned :: Maybe Natural
  } deriving (Generic)

deriving instance (Eq   a, Eq   (SeekRep s)) => Eq   (Seek s a)
deriving instance (Show a, Show (SeekRep s)) => Show (Seek s a)

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

instance (ToJSON   (SeekRep s), ToJSON   a) => ToJSON   (MultiPatch s a) where
    toJSON     = genericToJSON     $ jsonCfgCamelDrop 2
    toEncoding = genericToEncoding $ jsonCfgCamelDrop 2
instance (FromJSON (SeekRep s), FromJSON a) => FromJSON (MultiPatch s a) where
    parseJSON  = genericParseJSON  $ jsonCfgCamelDrop 2

instance (ToJSON   (SeekRep s), ToJSON   a) => ToJSON   (Seek s a) where
    toJSON     = genericToJSON     $ jsonCfgCamelDrop 1
    toEncoding = genericToEncoding $ jsonCfgCamelDrop 1
instance (FromJSON (SeekRep s), FromJSON a) => FromJSON (Seek s a) where
    parseJSON  = genericParseJSON  $ jsonCfgCamelDrop 1

instance (ToJSON   p) => ToJSON   (Aligned p) where
    toJSON     = genericToJSON     $ jsonCfgCamelDrop 7
    toEncoding = genericToEncoding $ jsonCfgCamelDrop 7
instance (FromJSON p) => FromJSON (Aligned p) where
    parseJSON  = genericParseJSON  $ jsonCfgCamelDrop 7

--------------------------------------------------------------------------------

convert :: (Seek s a -> Rec (Flap a) fs) -> MultiPatch s a -> [Patch s fs a]
convert f p = map go (mpAt p)
  where go s = Patch { patchData = mpData p
                     , patchSeek = sSeek s
                     , patchMeta = HFunctorList $ f s }

-- TODO how to clean up (it's like liftA2 over n instead of 2. like a fold)
-- likely 'ap'!
convertBinAlign
    :: forall s a
    .  SeekRep s ~ Natural
    => MultiPatch 'RelSeek a
    -> [Patch 'RelSeek '[Const (Align.Meta s), Const Bin.Meta, Bin.MetaStream] a]
convertBinAlign p = convert go p
  where go s = cmAlign s <+> cmBin s <+> cmBinStream s

convertBin
    :: forall s a
    .  MultiPatch s a
    -> [Patch s '[Const Bin.Meta, Bin.MetaStream] a]
convertBin p = convert go p
  where go s = cmBin s <+> cmBinStream s

convertAlign
    :: forall s a
    .  SeekRep s ~ Natural
    => MultiPatch 'RelSeek a
    -> [Patch 'RelSeek '[Const (Align.Meta s)] a]
convertAlign p = convert cmAlign p

convertEmpty
    :: forall s a
    .  MultiPatch s a
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

cmBin :: Seek s a -> Rec (Flap a) '[Const Bin.Meta]
cmBin s = cm $ Const $ Bin.Meta { Bin.mMaxBytes = sMaxBytes s }

cmBinStream :: Seek s a -> Rec (Flap a) '[Bin.MetaStream]
cmBinStream s = cm $ Bin.MetaStream
    { Bin.msNullTerminates = sNullTerminates s
    , Bin.msExpected = sExpected s }

cmAlign :: SeekRep s ~ Natural => Seek 'RelSeek a -> Rec (Flap a) '[Const (Align.Meta s)]
cmAlign s = cm $ Const $ Align.Meta { Align.mExpected = sAligned s }

cm :: f a -> Rec (Flap a) '[f]
cm fa = Flap fa :& RNil
