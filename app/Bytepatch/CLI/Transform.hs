module Bytepatch.CLI.Transform where

import Bytepatch.CLI.Common
import GHC.Generics ( Generic )

-- | @bytepatch transform@
data CCmdTransform = CCmdTransform
  { cCmdTransformPatchscriptIn  :: CStream
  , cCmdTransformPatchscriptOut :: CStream
  , cCmdTransformDataTypeIn     :: CPatchData
  , cCmdTransformDataTypeOut    :: CPatchData
  } deriving stock (Generic, Show, Eq)

-- Check transformations can require the target file while data transforms never
-- do, so we separate these concepts.
