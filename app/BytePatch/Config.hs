module BytePatch.Config where

import GHC.Generics ( Generic )
import StreamPatch.Patch.Compare qualified as Compare
import StreamPatch.Patch ( SeekKind )

data Config = Config
  { patchscriptFormat :: CPatchscriptFormat
  , patchscriptPath   :: FilePath
  , cmd               :: CCmd
  } deriving (Eq, Show, Generic)

data CCmd
  = CCmdPatch' CCmdPatch
  | CCmdCompile' CCmdCompile
    deriving (Eq, Show, Generic)

data CCmdPatch = CCmdPatch
  { streamPair  :: CStreamPair
  , printBinary :: Bool
  } deriving (Eq, Show, Generic)

-- TODO
data CCmdCompile = CCmdCompile
    deriving (Eq, Show, Generic)

data CStreamPair = CStreamPair
  { streamIn  :: CStream
  , streamOut :: CStream
  } deriving (Eq, Show, Generic)

-- | "Single file" stream.
data CStream
  = CStreamFile FilePath
  | CStreamStd
    deriving (Eq, Show, Generic)

data CPatchscriptFormat = CPatchscriptFormat
  { dataType :: CPatchDataType
  , align    :: CAlign
  , compare  :: Compare.Via
  , seek     :: SeekKind
  } deriving (Eq, Show, Generic)

data CAlign
  = CAlign   -- ^ Patch has alignment data.
  | CNoAlign -- ^ Patch does not have alignment data.
    deriving (Eq, Show, Generic)

data CPatchDataType
  = CTextPatch      -- ^ Patch 'Text' over a 'Text' stream.
  | CBinPatch       -- ^ Patch bytestrings over a bytestream.
  | CTextBinPatch   -- ^ Patch 'Text' over a bytestream.
  | CAsmBinPatch    -- ^ Patch ARMv8 Thumb (LE) assembly over a bytestream.
  | CAsmsBinPatch   -- ^ Patch many ARMv8 Thumb (LE) instrs over a bytestream.
    deriving (Eq, Show, Generic)
