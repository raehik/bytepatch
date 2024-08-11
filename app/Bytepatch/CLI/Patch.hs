module Bytepatch.CLI.Patch where

import Bytepatch.CLI.Common
import GHC.Generics ( Generic )

-- | @bytepatch patch@
data CCmdPatch = CCmdPatch
  { cCmdPatchPatchscript :: FilePath
  , cCmdPatchSchema      :: CPatchSchema
  , cCmdPatchStrategy    :: CPatchStrategy
  , cCmdPatchPrintBinary :: Bool
  -- ^ Whether to print patched binary outputs to stdout (anti footgun).
  } deriving stock (Generic, Show, Eq)

-- | How and where to apply the patchscript.
data CPatchStrategy
  = CPatchStrategyInplace FilePath
  -- ^ Patch the provided file in-place.
  --
  -- Requires a seekable, read-write stream, so we limit to files.

  | CPatchStrategyRW CStream CStream
  -- ^ Patch by reading from the first stream and writing to the second.
    deriving stock (Generic, Show, Eq)

{-
main :: CCmdPatch -> IO ()
main c =
    case cPsData (cCmdPatchSchema c) of
      CPatchDataHex ->
        --decodePsFileYaml @
        Yaml.decodeFileEither (cCmdPatchPatchscript c) >>= \case
          Left  e  -> error $ show e
          Right ps ->
      _ -> error "unsupported patch data type"

decodePsFileYaml :: forall a chk. FilePath -> IO [Patch AbsSeek Int chk a]
decodePsFileYaml fp =
    Yaml.decodeFileEither (cCmdPatchPatchscript c) fp >>= \case
      Left  e  -> error $ show e
      Right ps -> pure ps
-}
