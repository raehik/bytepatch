{-# LANGUAGE OverloadedRecordDot #-}

module BytePatch.CLI ( parse ) where

import BytePatch.Config
import Raehik.CLI.Stream

import Options.Applicative
import Control.Monad.IO.Class
import StreamPatch.Patch.Compare qualified as Compare
import StreamPatch.Patch ( SeekKind(..) )

import Binrep.Type.Assembly qualified as BR.Asm
import Binrep.Type.ByteString qualified as BR.ByteString
import Binrep.Type.Text qualified as BR.Text

parse :: MonadIO m => m C
parse = execParserWithDefaults desc pC
  where desc = "Patch data in a stream."

pC :: Parser C
pC = C <$> pCPsFmt <*> pPathIn <*> pCCmd

pCPsFmt :: Parser CPsFmt
pCPsFmt = CPsFmt <$> pCData
                 <*> pCAlign
                 <*> pCCompareVia
                 <*> pSeekKind

pCCmd :: Parser CCmd
pCCmd = hsubparser $
       cmd' "patch"   descPatch   (CCmdPatch'   <$> pCCmdPatch)
    <> cmd' "compile" descCompile (CCmdCompile' <$> pCCmdCompile)
  where
    descPatch   = "Apply patchscript to a stream."
    descCompile = "\"Compile\" patchscript to a processed form."

pCCmdPatch :: Parser CCmdPatch
pCCmdPatch = CCmdPatch <$> pStreamIn <*> pStreamOut <*> pPrintBinary

pCCmdCompile :: Parser CCmdCompile
pCCmdCompile = pure CCmdCompile

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

pCData :: Parser CData
pCData = option (maybeReader mapper) $
       long "type"
    <> short 't'
    <> help "Patch data meaning (see docs for full help)"
    <> metavar "PATCH_TYPE"
  where mapper = \case "bin"              -> Just CDataBytes
                       "text-bin,utf8,c"  -> Just $ CDataTextBin BR.Text.UTF8 BR.ByteString.C
                       "asm,armv8thumble" -> Just $ CDataAsm BR.Asm.ArmV8ThumbLE
                       "text-plain"       -> Just CDataText
                       _                  -> Nothing

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
                       "hashB3" -> Just $ Compare.ViaDigest Compare.B3
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
