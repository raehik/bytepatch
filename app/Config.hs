module Config where

import           GHC.Generics ( Generic )
import qualified StreamPatch.Patch.Binary as Bin

data Config = Config
  { cfgPatchscript :: FilePath
  , cfgStreamPair  :: CStreamPair
  , cfgPatchFormat :: CPatchFormat
  , cfgPrintBinary :: Bool
  } deriving (Eq, Show, Generic)

data CStreamPair = CStreamPair
  { cStreamPairIn  :: CStream
  , cStreamPairOut :: CStream
  } deriving (Eq, Show, Generic)

-- | "Single file" stream.
data CStream
  = CStreamFile FilePath
  | CStreamStd
    deriving (Eq, Show, Generic)

data CPatchFormat = CPatchFormat
  { cpfDataType :: CPatchDataType
  , cpfAlign    :: CPatchAlign
  , cpfBinCfg   :: Bin.Cfg
  } deriving (Eq, Show, Generic)

data CPatchAlign
  = CAlignPatch     -- ^ Patch has alignment data.
  | CNoAlignPatch   -- ^ Patch does not have alignment data.
    deriving (Eq, Show, Generic)

data CPatchDataType
  = CTextPatch      -- ^ Patch 'Text' over a 'Text' stream.
  | CBinPatch       -- ^ Patch bytestrings over a bytestream.
  | CTextBinPatch   -- ^ Patch 'Text' over a bytestream.
  | CAsmBinPatch    -- ^ Patch ARMv8 Thumb (LE) assembly over a bytestream.
  | CAsmsBinPatch   -- ^ Patch many ARMv8 Thumb (LE) instrs over a bytestream.
    deriving (Eq, Show, Generic)
