module BytePatch.Linear.Core where

import           BytePatch.Core

-- | A list of "keep n bytes, write data in-place" actions.
type Patchscript a = [(Int, Overwrite a)]

-- | Write the given data into the given offset.
data Patch a = Patch
  { patchContents :: a
  , patchOffset   :: Int
  , patchMeta     :: OverwriteMeta a
  } deriving (Eq, Show)
