{-# LANGUAGE DataKinds, TypeFamilies, UndecidableInstances #-}

module BytePatch.Core where

import GHC.Generics ( Generic )
import GHC.Natural

-- | A located edit to a stream of 'a'.
--
-- The type of the seek value depends on its meaning: absolutely-located patches
-- use 'Natural's, while cursor-based patches use 'Integer's.
data Patch (s :: SeekKind) a = Patch (SeekRep s) (Edit a)
    deriving (Generic, Functor, Foldable, Traversable)

deriving instance (Eq (SeekRep s), Eq a) => Eq (Patch s a)
deriving instance (Show (SeekRep s), Show a) => Show (Patch s a)

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
data Edit a = Edit
  { editData :: a
  , editMeta :: EditMeta a
  } deriving (Eq, Show, Generic, Functor, Foldable, Traversable)

-- | Various optional metadata defining expected existing data for an 'Edit'.
data EditMeta a = EditMeta
  { emNullTerminates :: Maybe Int
  -- ^ Stream segment should be null bytes (0x00) only from this index onwards.

  , emExpected       :: Maybe a
  -- ^ Stream segment should be this.

  } deriving (Eq, Show, Generic, Functor, Foldable, Traversable)
