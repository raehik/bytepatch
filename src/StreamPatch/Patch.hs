-- | Core patch type definitions: patches, seeks, metadata.

module StreamPatch.Patch where

import           StreamPatch.HFunctorList
import           Numeric.Natural            ( Natural )
import           Data.Kind
import           GHC.Generics               ( Generic )

type Patch :: SeekKind -> [Type -> Type] -> Type -> Type
data Patch s fs a = Patch
  { patchData :: a
  , patchSeek :: SeekRep s
  , patchMeta :: HFunctorList fs a
  } deriving (Generic)

deriving instance (Eq   a, Eq   (SeekRep s), Eq   (HFunctorList fs a)) => Eq   (Patch s fs a)
deriving instance (Show a, Show (SeekRep s), Show (HFunctorList fs a)) => Show (Patch s fs a)
deriving instance Functor     (HFunctorList fs) => Functor     (Patch s fs)
deriving instance Foldable    (HFunctorList fs) => Foldable    (Patch s fs)
deriving instance Traversable (HFunctorList fs) => Traversable (Patch s fs)

-- | What a patch seek value means.
data SeekKind
  = FwdSeek -- ^ seeks only move cursor forward
  | RelSeek -- ^ seeks are relative e.g. to a universal base, or a stream cursor
  | AbsSeek -- ^ seeks specify an exact offset in stream
    deriving (Eq, Show, Generic)

-- | Get the representation for a 'SeekKind'. Allows us a bit more safety.
type family SeekRep (s :: SeekKind) where
    SeekRep 'FwdSeek = Natural
    SeekRep 'RelSeek = Integer
    SeekRep 'AbsSeek = Natural
