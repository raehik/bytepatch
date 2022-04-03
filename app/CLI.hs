{-# LANGUAGE OverloadedRecordDot #-}

module CLI ( parse ) where

import Config
import Options.Applicative
import Control.Monad.IO.Class
import StreamPatch.Patch.Compare qualified as Compare
import StreamPatch.Patch ( SeekKind(..) )

parse :: MonadIO m => m Config
parse = execParserWithDefaults desc pConfig
  where desc = "Patch bytestrings (TODO)."

pConfig :: Parser Config
pConfig = Config
    <$> pCPatchscriptFormat
    <*> strArgument (metavar "PATCHSCRIPT" <> help "Path to patchscript")
    <*> pCCmd

pCCmd :: Parser CCmd
pCCmd = hsubparser $
       cmd' "patch"   descPatch   (CCmdPatch'   <$> pCCmdPatch)
    <> cmd' "compile" descCompile (CCmdCompile' <$> pCCmdCompile)
  where
    descPatch   = "Apply patchscript to a stream."
    descCompile = "\"Compile\" patchscript to a processed form."

pCCmdPatch :: Parser CCmdPatch
pCCmdPatch = CCmdPatch <$> pCStreamPair <*> pPrintBinary

pCCmdCompile :: Parser CCmdCompile
pCCmdCompile = pure CCmdCompile

pCStreamPair :: Parser CStreamPair
pCStreamPair = CStreamPair <$> pCSIn <*> pCSOut
  where
    pCSIn    = pFileArg <|> pStdin
    pCSOut   = pFileOpt <|> pure CStreamStd
    pFileArg = CStreamFile <$> strArgument (metavar "FILE" <> help "Input file")
    pFileOpt = CStreamFile <$> strOption (metavar "FILE" <> long "out-file" <> short 'o' <> help "Output file")
    pStdin   = flag' CStreamStd (long "stdin"  <> help "Use stdin")

pCPatchscriptFormat :: Parser CPatchscriptFormat
pCPatchscriptFormat = CPatchscriptFormat <$> pCPatchDataType
                                         <*> pCAlign
                                         <*> pCCompareVia
                                         <*> pSeekKind

pSeekKind :: Parser SeekKind
pSeekKind = option (maybeReader mapper) $
       long "seek"
    <> short 's'
    <> help "Seek type (abs/fwd/rel)"
    <> metavar "SEEK_TYPE"
  where mapper = \case "abs" -> Just AbsSeek
                       "fwd" -> Just FwdSeek
                       "rel" -> Just RelSeek
                       _     -> Nothing

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

pCAlign :: Parser CAlign
pCAlign = flag CNoAlign CAlign $
       long "aligned"
    <> help "Parse alignment data"

pPrintBinary :: Parser Bool
pPrintBinary = switch $
       long "print-binary"
    <> help "Force print binary to stdout"

pCCompareVia :: Parser Compare.Via
pCCompareVia = option (maybeReader mapper) $
       long "compare"
    <> short 'c'
    <> help "Comparison strategy (equal/prefix/size/hashB3)"
    <> metavar "COMPARISON_STRATEGY"
  where mapper = \case "equal"  -> Just $ Compare.ViaEq Compare.Exact
                       "prefix" -> Just $ Compare.ViaEq Compare.PrefixOf
                       "size"   -> Just Compare.ViaSize
                       "hashB3" -> Just $ Compare.ViaHash Compare.HashFuncB3
                       _        -> Nothing


--------------------------------------------------------------------------------

-- | Execute a 'Parser' with decent defaults.
execParserWithDefaults :: MonadIO m => String -> Parser a -> m a
execParserWithDefaults desc p = liftIO $ customExecParser
    (prefs $ showHelpOnError)
    (info (helper <*> p) (progDesc desc))

-- | Shorthand for the way I always write commands.
cmd' :: String -> String -> Parser a -> Mod CommandFields a
cmd' name desc p = command name (info p (progDesc desc))
