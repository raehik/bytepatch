module BytePatch.Core where

import GHC.Generics ( Generic )

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
