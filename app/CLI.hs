module CLI ( parse ) where

import           Config
import           Options.Applicative
import           Control.Monad.IO.Class
import qualified StreamPatch.Patch.Binary as Bin

parse :: MonadIO m => m Config
parse = execParserWithDefaults desc pConfig
  where desc = "Patch bytestrings (TODO)."

pConfig :: Parser Config
pConfig = Config
    <$> strArgument (metavar "PATCHSCRIPT" <> help "Path to patchscript")
    <*> pCStreamPair
    <*> pCPatchFormat
    <*> pPrintBinary

pCStreamPair :: Parser CStreamPair
pCStreamPair = CStreamPair <$> pCSIn <*> pCSOut
  where
    pCSIn    = pFileArg <|> pStdin
    pCSOut   = pFileOpt <|> pure CStreamStd
    pFileArg = CStreamFile <$> strArgument (metavar "FILE" <> help "Input file")
    pFileOpt = CStreamFile <$> strOption (metavar "FILE" <> long "out-file" <> short 'o' <> help "Output file")
    pStdin   = flag' CStreamStd (long "stdin"  <> help "Use stdin")

pCPatchFormat :: Parser CPatchFormat
pCPatchFormat = CPatchFormat <$> pCPatchDataType <*> pCPatchAlign <*> pBinCfg

pCPatchDataType :: Parser CPatchDataType
pCPatchDataType = option (maybeReader mapper) $
       long "type"
    <> short 't'
    <> help "How to interpret & use patch data (binary/text/text-only/asm)"
    <> metavar "PATCH_TYPE"
  where mapper = \case "text-only" -> Just CTextPatch
                       "bin"       -> Just CBinPatch
                       "binary"    -> Just CBinPatch
                       "text"      -> Just CTextBinPatch
                       "asm"       -> Just CAsmBinPatch
                       "asms"      -> Just CAsmsBinPatch
                       _           -> Nothing

pCPatchAlign :: Parser CPatchAlign
pCPatchAlign = flag CNoAlignPatch CAlignPatch $
       long "aligned"
    <> help "Parse alignment data"

pBinCfg :: Parser Bin.Cfg
pBinCfg = Bin.Cfg <$> pExactMatch
  where
    pExactMatch = flag True False $
           long "match-exact"
        <> help "Expected binary data must match exactly (rather than be a prefix)"

pPrintBinary :: Parser Bool
pPrintBinary = switch $
       long "print-binary"
    <> help "Force print binary to stdout"

--------------------------------------------------------------------------------

-- | Execute a 'Parser' with decent defaults.
execParserWithDefaults :: MonadIO m => String -> Parser a -> m a
execParserWithDefaults desc p = liftIO $ customExecParser
    (prefs $ showHelpOnError)
    (info (helper <*> p) (progDesc desc))
