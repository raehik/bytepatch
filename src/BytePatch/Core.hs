module BytePatch.Core where

-- | A single in-place edit.
--
-- Overwrites may store extra metadata that can be used at patch time to
-- validate input data (i.e. check we're patching the expected file).
data Overwrite a = Overwrite a (OverwriteMeta a)
    deriving (Eq, Show)

-- | Optional patch time data for an overwrite.
data OverwriteMeta a = OverwriteMeta
  { omNullTerminates :: Maybe Int
  -- ^ Stream segment should be null bytes (0x00) only from this index onwards.

  , omExpected       :: Maybe a
  -- ^ Stream segment should be this.
  } deriving (Eq, Show, Functor, Foldable, Traversable)
