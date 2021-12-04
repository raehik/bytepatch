module Config where

import qualified BytePatch.Patch.Binary as Bin

data Config = Config
  { cfgPatchCfg    :: Bin.Cfg
  , cfgPatchscript :: FilePath
  , cfgStreamInOut :: CStreamInOut
  } deriving (Eq, Show)

-- | "Single file" stream.
data CStream
  = CStreamFile FilePath
  | CStreamStd
    deriving (Eq, Show)

newtype CStreamInOut = CStreamInOut { unCStreamInOut :: (CStream, CStream) } deriving (Eq, Show)
