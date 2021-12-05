-- | Core patch type definitions: patches, positions, seek kinds.

module BytePatch.Patch where

import GHC.Generics ( Generic )
import GHC.Natural

-- | A located edit to a stream of 'a' with some metadata 'd'.
data Patch (s :: SeekKind) d a = Patch
  { patchData :: a
  , patchPos  :: Pos s (d a)
  } deriving (Generic, Functor, Foldable, Traversable)

deriving instance (Eq (SeekRep s), Eq (d a), Eq a) => Eq (Patch s d a)
deriving instance (Show (SeekRep s), Show (d a), Show a) => Show (Patch s d a)

-- | A "position" in a stream with some associated metadata 'a'.
--
-- The type of the seek value depends on its meaning: some are 'Natural's (e.g.
-- absolute offsets), others are 'Integer's (e.g. cursor-based seeks).
data Pos (s :: SeekKind) a = Pos
  { posSeek :: SeekRep s
  , posMeta :: a
  } deriving (Generic, Functor, Foldable, Traversable)

deriving instance (Eq (SeekRep s), Eq a) => Eq (Pos s a)
deriving instance (Show (SeekRep s), Show a) => Show (Pos s a)

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

-- | A single edit to multiple places in a stream of 'a' with some metadata 'd'.
data MultiPatch (s :: SeekKind) d a = MultiPatch
  { multiPatchData :: a
  , multiPatchPos  :: [Pos s (d a)]
  } deriving (Generic, Functor, Foldable, Traversable)

deriving instance (Eq (SeekRep s), Eq (d a), Eq a) => Eq (MultiPatch s d a)
deriving instance (Show (SeekRep s), Show (d a), Show a) => Show (MultiPatch s d a)
