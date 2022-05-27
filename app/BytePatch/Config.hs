module BytePatch.Config where

import GHC.Generics ( Generic )
import Data.Data ( Typeable, Data )
import StreamPatch.Patch.Compare qualified as Compare

import Binrep.Type.Text qualified as BR.Text
import Binrep.Type.ByteString qualified as BR.ByteString
import Binrep.Type.Assembly qualified as BR.Asm

import Raehik.CLI.Stream

data C = C
  { cPsFmt  :: CPsFmt
  , cPsPath :: Path 'In "patchscript file"
  , cCmd    :: CCmd
  } deriving stock (Generic, Typeable, Data, Show, Eq)

data CPsFmt = CPsFmt
  { cPsFmtData    :: CData
  , cPsFmtAlign   :: CAlign
  , cPsFmtCompare :: Compare.Via
  , cPsFmtSeek    :: ()
  } deriving stock (Generic, Typeable, Data, Show, Eq)

-- | What the patch data indicates (base and target representation).
data CData
  = CDataBytes
  -- ^ Raw bytes (via hex), patched directly over a bytestring.

  | CDataTextBin BR.Text.Encoding BR.ByteString.Rep
  -- ^ Text, which is to be encoded to the given character encoding and placed
  --   in the given bytestring representation, then patched over a bytestring.

  | CDataAsm BR.Asm.Arch
  -- ^ Assembly instructions for the given architecture, which is to be
  --   assembled into machine code for the architecture, then patched over a
  --   bytestring.

  | CDataText
  -- ^ Text, which is to be patched directly over @Text@.
    deriving stock (Generic, Typeable, Data, Show, Eq)

data CCmd
  = CCmdPatch' CCmdPatch
  | CCmdCompile' CCmdCompile
    deriving stock (Generic, Typeable, Data, Show, Eq)

data CCmdPatch = CCmdPatch
  { cCmdPatchStreamIn    :: Stream 'In  "stream to patch"
  , cCmdPatchStreamOut   :: Stream 'Out "stream"
  , cCmdPatchPrintBinary :: Bool
  } deriving stock (Generic, Typeable, Data, Show, Eq)

-- TODO
data CCmdCompile = CCmdCompile
    deriving stock (Generic, Typeable, Data, Show, Eq)

data CAlign
  = CAlign   -- ^ Patch has alignment data.
  | CNoAlign -- ^ Patch does not have alignment data.
    deriving stock (Generic, Typeable, Data, Show, Eq)
