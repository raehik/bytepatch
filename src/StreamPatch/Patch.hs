{-# LANGUAGE UndecidableInstances #-}

{- | Core patch type: patches, seeks, metadata container.

This library is based around patches to streams i.e. containers indexed by the
natural numbers (or integers, depending on your view). As such, we restrict what
a seek can look like. Parts of the codebase could be generalized to work over
any seek kind, so you could e.g. write a text patcher that uses line and column
positions to seek through the data. But you can't transform line and column to
byte position, at least not without parsing the file. So it would need a lot of
thought and careful design to generalize in that direction.
-}

module StreamPatch.Patch where

import StreamPatch.HFunctorList

import GHC.Generics ( Generic )
import Data.Data ( Data, Typeable )
import Data.Aeson

-- | A single patch on a stream of 'a'.
data Patch s fs a = Patch
  { patchData :: a
  , patchSeek :: s
  , patchMeta :: HFunctorList fs a
  } deriving stock (Generic)

deriving stock instance (Eq   a, Eq   s, Eq   (HFunctorList fs a)) => Eq   (Patch s fs a)
deriving stock instance (Show a, Show s, Show (HFunctorList fs a)) => Show (Patch s fs a)
deriving stock instance (Data a, Data s, Data (HFunctorList fs a), Typeable fs) => Data (Patch s fs a)
deriving stock instance Functor     (HFunctorList fs) => Functor     (Patch s fs)
deriving stock instance Foldable    (HFunctorList fs) => Foldable    (Patch s fs)
deriving stock instance Traversable (HFunctorList fs) => Traversable (Patch s fs)

instance (ToJSON   a, ToJSON   s, ToJSON   (HFunctorList fs a)) => ToJSON   (Patch s fs a)
instance (FromJSON a, FromJSON s, FromJSON (HFunctorList fs a)) => FromJSON (Patch s fs a)
