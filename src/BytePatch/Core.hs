{-# LANGUAGE DataKinds, TypeFamilies, UndecidableInstances #-}

module BytePatch.Core where

import GHC.Generics ( Generic )
import GHC.Natural

-- | A located edit to a stream of 'a'.
--
-- The type of the seek value depends on its meaning: absolutely-located patches
-- use 'Natural's, while cursor-based patches use 'Integer's.
data Patch (s :: SeekKind) m a = Patch (SeekRep s) (Edit m a)
    deriving (Generic, Functor, Foldable, Traversable)

deriving instance (Eq (SeekRep s), Eq (m a), Eq a) => Eq (Patch s m a)
deriving instance (Show (SeekRep s), Show (m a), Show a) => Show (Patch s m a)

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

-- | Data to add to a stream.
data Edit m a = Edit
  { editData :: a
  , editMeta :: m a
  } deriving (Eq, Show, Generic, Functor, Foldable, Traversable)
