module Config where

import qualified BytePatch.Linear.Patch

data Config = Config
  { cfgPatchCfg    :: BytePatch.Linear.Patch.Cfg
  , cfgPatchscript :: FilePath
  , cfgStreamInOut :: CStreamInOut
  } deriving (Eq, Show)

-- | "Single file" stream.
data CStream
  = CStreamFile FilePath
  | CStreamStd
    deriving (Eq, Show)

newtype CStreamInOut = CStreamInOut { unCStreamInOut :: (CStream, CStream) } deriving (Eq, Show)
