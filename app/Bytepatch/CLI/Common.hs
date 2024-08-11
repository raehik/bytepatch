module Bytepatch.CLI.Common where

import GHC.Generics ( Generic )

data CPatchSchema = CPatchSchema
  { cPsData  :: CPatchData
  , cPsCheck :: CCheck
  } deriving stock (Generic, Show, Eq)

-- | What check to perform when patching.
data CCheck
  = CCheckEquality
  | CCheckDigest CHashFunc
    deriving stock (Generic, Show, Eq)

-- | Supported hash functions.
data CHashFunc = CHashFuncBlake3
    deriving stock (Generic, Show, Eq)

--newtype Check a = Check { unCheck :: MaxLen Int (NulltermStrip a) }

data CStream
  = CStreamStd
  -- ^ Standard stream. Contextual: stdin when input, stdout when output.

  | CStreamFile FilePath
    deriving stock (Generic, Show, Eq)

data CPatchData
  = CPatchDataHex  -- ^ Raw bytes via pretty hex.
  | CPatchDataText CPDTextEnc CPDBytestringRep
  -- ^ Text, with character encoding and string representation.
  | CPatchDataAsm  CPDAsm -- ^ Assembly snippet (text).
    deriving stock (Generic, Show, Eq)

-- | Supported text encodings. Should be able to re-encode to from UTF-8 without
--   failure.
data CPDTextEnc
  = CPDTextEncUtf8 -- ^ UTF-8 (= no change)
  | CPDTextEncSjis -- ^ Shift-JIS
    deriving stock (Generic, Show, Eq)

data CPDBytestringRep
  = CPDBytestringRepC
  -- ^ C-string: null terminator, no nulls permitted in data.

  | CPDBytestringRepPascal CPDBytestringRepPascalLen
  -- ^ Pascal string: length prefixed
    deriving stock (Generic, Show, Eq)

-- | Length of the length prefix integer.
data CPDBytestringRepPascalLen
  = CPDBytestringRepPascalLen1 -- ^ Word8
  | CPDBytestringRepPascalLen2 -- ^ Word16
  | CPDBytestringRepPascalLen4 -- ^ Word32
  | CPDBytestringRepPascalLen8 -- ^ Word64
    deriving stock (Generic, Show, Eq)

-- | Supported assembly languages/configurations.
data CPDAsm -- TODO
    deriving stock (Generic, Show, Eq)

{-
  { cPsFmtData    :: CData
  , cPsFmtAlign   :: CAlign
  , cPsFmtCompare :: Compare.Via
  , cPsFmtSeek    :: ()
  } deriving stock (Generic, Show, Eq)

data CAlign
  = CAlign   -- ^ Patch has alignment data.
  | CNoAlign -- ^ Patch does not have alignment data.
    deriving stock (Generic, Show, Eq)
-}
