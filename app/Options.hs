module Options ( parse ) where

import           Config
import           Options.Applicative
import qualified BytePatch.Linear.Patch as Patch
import           Control.Monad.IO.Class

parse :: MonadIO m => m Config
parse = execParserWithDefaults desc pConfig
  where desc = "Patch bytestrings (TODO)."

pConfig :: Parser Config
pConfig = Config
    <$> pPatchCfg
    <*> strArgument (metavar "PATCHSCRIPT" <> help "Path to patchscript")
    <*> pCStreamInOut

pPatchCfg :: Parser Patch.Cfg
pPatchCfg = Patch.Cfg <$> pAllowRepatch <*> pExpectExact
  where
    pAllowRepatch = switch $ long "allow-repatch" <> help "Override safety checks and only warn if it appears we're repatching a patched file (CURRENTLY NONFUNCTIONAL)"
    pExpectExact = flag True False $ long "expect-exact" <> help "When checking expected bytes, require an exact match (rather than the expected being a prefix of the actual)"

pCStreamInOut :: Parser CStreamInOut
pCStreamInOut = CStreamInOut <$> liftA2 (,) pCSIn pCSOut
  where
    pCSIn    = pFileArg <|> pStdin
    pCSOut   = pFileOpt <|> pure CStreamStd
    pFileArg = CStreamFile <$> strArgument (metavar "FILE" <> help "Input file")
    pFileOpt = CStreamFile <$> strOption (metavar "FILE" <> long "out-file" <> short 'o' <> help "Output file")
    pStdin   = flag' CStreamStd (long "stdin"  <> help "Use stdin")

--------------------------------------------------------------------------------

-- | Execute a 'Parser' with decent defaults.
execParserWithDefaults :: MonadIO m => String -> Parser a -> m a
execParserWithDefaults desc p = liftIO $ customExecParser
    (prefs $ showHelpOnError)
    (info (helper <*> p) (progDesc desc))
