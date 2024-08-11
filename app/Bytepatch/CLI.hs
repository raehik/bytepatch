module Bytepatch.CLI where

import Bytepatch.CLI.Patch
--import Bytepatch.CLI.Transform
import GHC.Generics ( Generic )

-- | Top-level bytepatch command selection.
data CCmd
  = CCmdPatch' CCmdPatch
  -- | CCmdTransform' CCmdTransform
    deriving stock (Generic, Show, Eq)
