-- | Core patch type definitions: patches, seeks, metadata.

module BytePatch.Patch where

import           Data.Kind
import           GHC.Generics ( Generic )
import           GHC.Natural

type PatchLike = SeekKind -> (Type -> Type) -> (Type -> Type) -> Type -> Type
type Patch :: PatchLike
data Patch s dd pd a = Patch
  { patchData :: a
  , patchMeta :: dd a
  , patchPos  :: Pos s pd a
  } deriving (Generic, Functor, Foldable, Traversable)

deriving instance (Eq   (SeekRep s), Eq   (dd a), Eq   (pd a), Eq   a) => Eq   (Patch s dd pd a)
deriving instance (Show (SeekRep s), Show (dd a), Show (pd a), Show a) => Show (Patch s dd pd a)

data Pos s pd a = Pos
  { posSeek :: SeekRep s
  , posMeta :: pd a
  } deriving (Generic, Functor, Foldable, Traversable)

deriving instance (Eq   (SeekRep s), Eq   (d a)) => Eq   (Pos s d a)
deriving instance (Show (SeekRep s), Show (d a)) => Show (Pos s d a)

-- | What a patch seek value means.
data SeekKind
  = FwdSeek -- ^ seeks only move cursor forward
  | RelSeek -- ^ seeks are relative e.g. to a universal base, or a stream cursor
  | AbsSeek -- ^ seeks specify an exact offset in stream
    deriving (Eq, Show, Generic)

-- | Get the representation for a 'SeekKind'.
type family SeekRep (s :: SeekKind) where
    SeekRep 'FwdSeek = Natural
    SeekRep 'RelSeek = Integer
    SeekRep 'AbsSeek = Natural
