{-# LANGUAGE DataKinds #-}

module BytePatch.Linear.Core where

import BytePatch.Core
import GHC.Generics ( Generic )

-- | Whether associated offsets are absolute or relative.
--
-- TODO an absolute offset should be a Natural actually!
data OffsetType = AbsOffset | RelOffset

data WithOffset (o :: OffsetType) a = WithOffset Int a
    deriving (Eq, Show, Generic, Functor, Foldable, Traversable)

-- | When we say "patch", we mean a "standalone" edit using an absolute offset.
type Patch a = WithOffset 'AbsOffset (Edit a)

-- | When we say "patch script", we're specifically referring to an ordered list
--   edits with relative offsets (TODO: currently positive or negative, but
--   really should make that clear / provide more options, e.g. linear vs.
--   fwd-seeking only).
type PatchScript a = [WithOffset 'RelOffset (Edit a)]
