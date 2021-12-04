module BytePatch.Core where

import GHC.Generics ( Generic )
import GHC.Natural

-- | A located edit to a stream of 'a' with some metadata 'd'.
data Patch (s :: SeekKind) d a = Patch
  { patchData :: a
  , patchPos  :: Pos s (d a)
  } deriving (Generic, Functor, Foldable, Traversable)

deriving instance (Eq (SeekRep s), Eq (d a), Eq a) => Eq (Patch s d a)
deriving instance (Show (SeekRep s), Show (d a), Show a) => Show (Patch s d a)

-- The type of the seek value depends on its meaning: absolutely-located patches
-- use 'Natural's, while cursor-based patches use 'Integer's.
data Pos (s :: SeekKind) a = Pos
  { posSeek :: SeekRep s
  , posMeta :: a
  } deriving (Generic, Functor, Foldable, Traversable)

deriving instance (Eq (SeekRep s), Eq a) => Eq (Pos s a)
deriving instance (Show (SeekRep s), Show a) => Show (Pos s a)

-- | What a patch seek value means.
data SeekKind
  = FwdSeek    -- ^ seeks only move cursor forward
  | CursorSeek -- ^ seeks move cursor forward or backward
  | AbsSeek    -- ^ seeks specify an exact offset in stream
    deriving (Eq, Show, Generic)

-- | Get the representation for a 'SeekKind'.
type family SeekRep (s :: SeekKind) where
    SeekRep 'FwdSeek    = Natural
    SeekRep 'CursorSeek = Integer
    SeekRep 'AbsSeek    = Natural

-- | A single edit to multiple places in a stream of 'a' with some metadata 'd'.
data MultiPatch (s :: SeekKind) d a = MultiPatch
  { multiPatchData :: a
  , multiPatchPos  :: [Pos s (d a)]
  } deriving (Generic, Functor, Foldable, Traversable)

deriving instance (Eq (SeekRep s), Eq (d a), Eq a) => Eq (MultiPatch s d a)
deriving instance (Show (SeekRep s), Show (d a), Show a) => Show (MultiPatch s d a)

expandMultiPatch :: MultiPatch s d a -> [Patch s d a]
expandMultiPatch mp = map go (multiPatchPos mp)
  where go pos = Patch (multiPatchData mp) pos
