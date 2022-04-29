{- | Core patch type: patches, seeks, metadata container.

This library is based around patches to streams i.e. containers indexed by the
natural numbers (or integers, depending on your view). As such, we restrict what
a seek can look like. Parts of the codebase could be generalized to work over
any seek kind, so you could e.g. write a text patcher that uses line and column
positions to seek through the data. But you can't transform line and column to
byte position, at least not without parsing the file. So it would need a lot of
thought and careful design to generalize in that direction.
-}

{-# LANGUAGE TemplateHaskell #-}

module StreamPatch.Patch where

import StreamPatch.HFunctorList
import Numeric.Natural ( Natural )

import GHC.Generics ( Generic )
import Data.Aeson

import Data.Singletons.TH
-- required for deriving instances (seems like bug)
import Prelude.Singletons hiding ( AbsSym0, Compare )

$(singletons [d|
    -- | What a stream seek value means.
    data SeekKind
      = FwdSeek -- ^ seeks only move cursor forward
      | RelSeek -- ^ seeks are relative e.g. to a universal base, or a stream cursor
      | AbsSeek -- ^ seeks specify an exact offset in stream
        deriving stock (Eq, Show)
    |])
deriving stock instance Generic SeekKind

-- | Get the representation for a 'SeekKind'.
type family SeekRep (s :: SeekKind) where
    SeekRep 'FwdSeek = Natural
    SeekRep 'RelSeek = Integer
    SeekRep 'AbsSeek = Natural

-- | A single patch on a stream of 'a'.
data Patch (s :: SeekKind) fs a = Patch
  { patchData :: a
  , patchSeek :: SeekRep s
  , patchMeta :: HFunctorList fs a
  } deriving stock (Generic)

deriving stock instance (Eq   a, Eq   (SeekRep s), Eq   (HFunctorList fs a)) => Eq   (Patch s fs a)
deriving stock instance (Show a, Show (SeekRep s), Show (HFunctorList fs a)) => Show (Patch s fs a)
deriving stock instance Functor     (HFunctorList fs) => Functor     (Patch s fs)
deriving stock instance Foldable    (HFunctorList fs) => Foldable    (Patch s fs)
deriving stock instance Traversable (HFunctorList fs) => Traversable (Patch s fs)

instance ( ToJSON a, ToJSON (SeekRep s), ToJSON (HFunctorList fs a)
         ) => ToJSON (Patch s fs a)
