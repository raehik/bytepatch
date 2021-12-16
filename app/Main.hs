{-# LANGUAGE AllowAmbiguousTypes #-}

module Main ( main ) where

import           Config
import qualified CLI

import           StreamPatch.Patch
import qualified StreamPatch.Patch.Align  as Align
import qualified StreamPatch.Patch.Linearize as Linear
import qualified StreamPatch.Patch.Binary as Bin
import           StreamPatch.Patch.Binary ( BinRep )
import qualified StreamPatch.Apply as Apply
import           BytePatch as BP
import qualified BytePatch.HexByteString as HexBS
import           BytePatch.HexByteString ( HexByteString )

import qualified System.Exit as System
import           Numeric.Natural
import           Data.Vinyl
import           Data.Vinyl.TypeLevel
import           Data.Functor.Const
import           Data.Either.Combinators ( mapLeft )
import           Control.Monad.IO.Class
import           Control.Monad.Reader
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import qualified Data.Yaml as Yaml
import           Data.Yaml ( FromJSON )
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import           Data.Text ( Text )
import           Optics
import           Data.Generics.Product.Any

type Bytes = BS.ByteString

data Error
  = ErrorYaml     Yaml.ParseException
  | ErrorAlign    String
  | ErrorBin      String
  | ErrorLinear   String
  | ErrorBinPatch String
  | Error         String
    deriving (Show)

main :: IO ()
main = CLI.parse >>= runReaderT run

run :: forall m. (MonadIO m, MonadReader Config m) => m ()
run = readPatchscript >>= processPatchScript >>= writePatched
  where
    readPatchscript = asks cfgPatchscript >>= readStream . CStreamFile

    processPatchScript psBs = do
        cpf <- asks cfgPatchFormat
        case cpfDataType cpf of
          CTextPatch    -> processText psBs
          CBinPatch     -> processBin' @HexByteString psBs
          CTextBinPatch -> processBin' @Text          psBs

    processText :: Bytes -> m Bytes
    processText psBs = do
        cpf <- asks cfgPatchFormat
        case processAny @Text psBs (cpfAlign cpf) of
          Left err -> quit err
          Right ps -> patchTextAwkward ps

    processBin' :: forall a. (BinRep a, FromJSON a, Show a) => Bytes -> m Bytes
    processBin' psBs = do
        cpf <- asks cfgPatchFormat
        case processBin @a psBs (cpfAlign cpf) of
          Left err -> quit err
          Right ps -> patchBin ps

    patchBin ps = do
        bsIn <- asks (cStreamPairIn . cfgStreamPair) >>= readStream
        binCfg <- asks $ cpfBinCfg . cfgPatchFormat
        case Apply.runPureFwdBin binCfg ps bsIn of
          Left err -> quit $ ErrorBinPatch (showErrorBin err)
          Right bsOut -> return $ BL.toStrict bsOut

    patchTextAwkward :: [Patch 'FwdSeek '[] Text] -> m Bytes
    patchTextAwkward ps = do
        bsIn <- asks (cStreamPairIn . cfgStreamPair) >>= readStream
        let psStr  = map (fmap Text.unpack) ps
            strIn  = Text.unpack $ Text.decodeUtf8 bsIn
            strOut = Apply.runPureSimpleFwdList psStr strIn
            bsOut  = Text.encodeUtf8 $ Text.pack strOut
        return bsOut

    writePatched bs =
        asks (cStreamPairOut . cfgStreamPair) >>= \case
          CStreamFile fp -> liftIO $ BS.writeFile fp bs
          CStreamStd     -> asks (cpfDataType . cfgPatchFormat) >>= \case
            CTextPatch -> liftIO $ BS.putStr bs
            _          -> asks cfgPrintBinary >>= \case
              True  -> liftIO $ BS.putStr bs
              False -> liftIO $ logWarn $
                     "refusing to print binary to stdout:"
                  <> " write to a file with --out-file FILE"
                  <> " or use --print-binary flag to override"

logWarn :: MonadIO m => String -> m ()
logWarn msg = liftIO $ putStrLn $ "bytepatch: warning: " <> msg

-- | Parses your bytes into some binreppable patch form (aligned or not,
--   depending on flag), converts it to the internal patch form, binreps it, and
--   linearizes. The binrep type is consumed inside the function, making it
--   ambiguous, so you need to pass it a type via @processBin \@Text@.
--
-- Note that we linearize *after* binrepping. This is important -- it means the
-- seeks are byte offsets, not using the pre-binrepped type.
processBin
    :: forall a. (BinRep a, FromJSON a, Show a) => Bytes -> CPatchAlign
    -> Either Error [Patch 'FwdSeek '[Bin.MetaStream] Bytes]
processBin bs = \case
  CAlignPatch   ->     mapLeft ErrorYaml (Yaml.decodeEither' @[Aligned (MultiPatch 'RelSeek a)] bs)
                   >>= mapLeft (ErrorAlign . show) . fmap concat . traverse (wrapAlign (BP.convertBinAlign @'AbsSeek))
                   >>= mapLeft (ErrorBin . showErrorBin) . traverse Bin.patchBinRep
                   >>= mapLeft (ErrorLinear . show) . Linear.linearize
  CNoAlignPatch ->     mapLeft ErrorYaml (Yaml.decodeEither' @[MultiPatch 'AbsSeek a] bs)
                   >>= mapLeft (ErrorBin . showErrorBin) . traverse Bin.patchBinRep . concat . map BP.convertBin
                   >>= mapLeft (ErrorLinear . show) . Linear.linearize

-- | Like 'processBin' but no binrepping. Similarly beautiful and ambiguous.
processAny
    :: forall a. (Show a, FromJSON a, Linear.HasLength a) => Bytes -> CPatchAlign
    -> Either Error [Patch 'FwdSeek '[] a]
processAny bs = \case
  CAlignPatch   ->     mapLeft ErrorYaml (Yaml.decodeEither' @[Aligned (MultiPatch 'RelSeek a)] bs)
                   >>= mapLeft (ErrorAlign . show) . fmap concat . traverse (wrapAlign (BP.convertAlign @'AbsSeek))
                   >>= mapLeft (ErrorLinear . show) . Linear.linearize
  CNoAlignPatch ->     mapLeft ErrorYaml (Yaml.decodeEither' @[MultiPatch 'AbsSeek a] bs)
                   >>= mapLeft (ErrorLinear . show) . Linear.linearize . concat . map BP.convertEmpty

wrapAlign
    :: ( SeekRep s ~ Natural
       , r ~ Const (Align.Meta s)
       , rs ~ RDelete r ss
       , RElem r ss i
       , RSubset rs ss is )
    => (MultiPatch 'RelSeek a -> [Patch 'RelSeek ss a])
    -> Aligned (MultiPatch 'RelSeek a)
    -> Either (Align.Error s) [Patch s rs a]
wrapAlign f = align . over (the @"alignedPatches") (concat . map f)

quit :: MonadIO m => Error -> m a
quit err = do
    liftIO $ putStrLn $ "bytepatch: error: " <> errStr
    liftIO $ System.exitWith $ System.ExitFailure errExitCode
  where
    (errExitCode, errStr) = case err of
      ErrorYaml     e -> (1, "while parsing YAML: " <> show e)
      ErrorAlign    e -> (2, "while aligning: "     <> e)
      ErrorBin      e -> (3, "while converting to binary representation: " <> e)
      ErrorLinear   e -> (4, "while linearizing: " <> e)
      ErrorBinPatch e -> (5, "while applying binaryized patch: " <> e)
      Error         e -> (10, e)

readStream :: MonadIO m => CStream -> m Bytes
readStream s = liftIO $
    case s of
      CStreamStd     -> BS.getContents
      CStreamFile fp -> BS.readFile fp

-- stupid but how else do I do this without pluggin HexByteString into Binary
-- guess I just gotta write ACTUAL error handling. ffs
showErrorBin :: Show a => Bin.Error a -> String
showErrorBin = \case
  Bin.ErrorBadBinRep a s -> "ErrorBadBinRep " <> show a <> " " <> s
  Bin.ErrorUnexpectedNonNull bs -> "ErrorUnexpectedNonNull " <> go bs
  Bin.ErrorDidNotMatchExpected bs1 bs2 -> "ErrorDidNotMatchExpected " <> go bs1 <> " " <> go bs2
  Bin.ErrorBinRepTooLong bs n -> "ErrorBinRepTooLong " <> go bs <> " " <> show n
  where go bs = "\"" <> Text.unpack (HexBS.prettyHexByteString bs) <> "\""

{-

writeStream :: MonadIO m => Bytes -> CStream -> m ()
writeStream bs s = liftIO $
    case s of
      CStreamStd     -> BS.putStr bs
      CStreamFile fp -> BS.writeFile fp bs

tryDecodeYaml
    :: forall a m. (FromJSON a, MonadIO m)
    => FilePath -> m (Either Yaml.ParseException a)
tryDecodeYaml fp = Yaml.decodeEither' <$> liftIO (BS.readFile fp)

-}
