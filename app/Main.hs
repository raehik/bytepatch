{-# LANGUAGE AllowAmbiguousTypes, OverloadedRecordDot, OverloadedStrings #-}

module Main ( main ) where

import           Config
import qualified CLI
import qualified Compile

import GHC.Generics ( Generic )

import           StreamPatch.Patch
import qualified StreamPatch.Patch.Align  as Align
import qualified StreamPatch.Patch.Linearize as Linear
import qualified StreamPatch.Patch.Binary as Bin
import           StreamPatch.Patch.Binary ( BinRep )
import qualified StreamPatch.Patch.Compare as Compare
import           StreamPatch.Patch.Compare ( Via(..), EqualityCheck(..), HashFunc(..), Compare )
import qualified StreamPatch.Apply as Apply
import           BytePatch as BP
import           Raehik.HexBytestring
import           StreamPatch.Patch.Binary.Asm ( Arch(..) )
import qualified StreamPatch.Patch.Binary.Asm as Asm
import           StreamPatch.HFunctorList ( Flap, HFunctorList )

import qualified System.Exit as System
import           Numeric.Natural
import           Data.Vinyl
import           Data.Vinyl.TypeLevel
import           Data.Functor.Const
import           Data.Either.Combinators ( mapLeft )
import           Control.Monad.IO.Class
import           Control.Monad.Reader
import           Control.Monad.Except
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import qualified Data.Yaml as Yaml
import           Data.Yaml ( FromJSON )
import qualified Data.Yaml.Pretty as YamlPretty
import           Data.Text ( Text )
import           Optics
import           Data.Generics.Product.Any

type Bytes = BS.ByteString

-- Errors that can occur during patchscript processing. Everything except
-- reading the patchscript, and writing out the successfully patched file.
data Error
  = ErrorYaml     Yaml.ParseException
  | ErrorAlign    String
  | ErrorLinear   String
  | Error         String
  | ErrorUnimplemented
  | ErrorProcessBinRep String -- can't do Bin.Error, has a typevar (until we put the typevar in this data, which will happen eventually)
  | ErrorProcessAsm Asm.Error
  | ErrorProcessApply String
    deriving (Generic, Show)

main :: IO ()
main = CLI.parse >>= runReaderT run

processDecode :: forall a m. (MonadError Error m, FromJSON a) => Bytes -> m a
processDecode = liftMapProcessError ErrorYaml . Yaml.decodeEither'

processAlign
    :: forall v a s m r rs ss i is
    .  ( SeekRep s ~ Natural
       , r ~ Const (Align.Meta s)
       , rs ~ RDelete r ss
       , RElem r ss i
       , RSubset rs ss is
       , MonadError Error m )
    => (MultiPatch 'RelSeek v a -> [Patch 'RelSeek ss a])
    -> [Aligned (MultiPatch 'RelSeek v a)]
    -> m [Patch s rs a]
processAlign f = liftMapProcessError (ErrorAlign . show) . fmap concat . traverse (wrapAlign f)

processLinearize
    :: forall a m fs
    .  ( Linear.HasLength a
       , Show a, ReifyConstraint Show (Flap a) fs, RMap fs, RecordToList fs
       , MonadError Error m )
    => [Patch 'AbsSeek fs a]
    -> m [Patch 'FwdSeek fs a]
processLinearize = liftMapProcessError (ErrorLinear . show) . Linear.linearize

liftMapProcessError :: MonadError Error m => (e -> Error) -> Either e a -> m a
liftMapProcessError f = liftEither . mapLeft f

processAsm
    :: forall (arch :: Arch) s fs m
    .  (MonadError Error m, Asm.Target arch, Traversable (HFunctorList fs))
    => [Patch s fs (Asm.AsmInstr arch)]
    -> m [Patch s fs (Asm.MachineInstr arch)]
processAsm = liftMapProcessError ErrorProcessAsm . traverse (traverse (Asm.assembleInstr @arch))

processAsms
    :: forall (arch :: Arch) s fs m
    .  (MonadError Error m, Asm.Target arch, Traversable (HFunctorList fs))
    => [Patch s fs [Asm.AsmInstr arch]]
    -> m [Patch s fs [Asm.MachineInstr arch]]
processAsms = liftMapProcessError ErrorProcessAsm . traverse (traverse (traverse (Asm.assembleInstr @arch)))

processBin
    :: forall a s ss is r rs m
    .  ( MonadError Error m
       , BinRep a
       , r ~ Const Bin.MetaPrep
       , rs ~ RDelete r ss
       , RElem r ss (RIndex r ss)
       , RSubset rs ss is
       , Traversable (HFunctorList rs)
       , Show a
       )
    => [Patch s ss a]
    -> m [Patch s rs Bytes]
processBin = liftMapProcessError (ErrorProcessBinRep . show) . traverse Bin.binRepify

liftProcessError :: MonadIO m => ExceptT Error m a -> m a
liftProcessError action =
    runExceptT action >>= \case
      Left  e -> quit e
      Right a -> return a

patchPureBinCompareFwd
    :: forall v m
    .  (MonadIO m, Compare v Bytes)
    => CStream
    -> [Patch 'FwdSeek '[Compare.Meta v, Bin.Meta] Bytes]
    -> m Bytes
patchPureBinCompareFwd si ps = do
    bsIn <- readStream si
    case Apply.runPureBinCompareFwd ps bsIn of
      Left  e     -> quit $ ErrorProcessApply $ show e
      Right bsOut -> return $ BL.toStrict bsOut

run :: forall m. (MonadIO m, MonadReader Config m) => m ()
run = readPatchscriptBs >>= liftProcessError . usePatchscriptBs
  where
    readPatchscriptBs = asks patchscriptPath >>= readStream . CStreamFile

    usePatchscriptBs bs = do
        cfg <- ask
        let cpf = cfg.patchscriptFormat
        case cfg.patchscriptFormat.dataType of
          CTextPatch    -> throwError $ ErrorUnimplemented
          CBinPatch     -> runBinCompareFwd @HexBytestring cpf cfg.cmd bs pure
          CTextBinPatch -> runBinCompareFwd @Text cpf cfg.cmd bs pure
          CAsmBinPatch  -> runBinCompareFwd cpf cfg.cmd bs (processAsm @'ArchArmV8ThumbLE)
          CAsmsBinPatch -> runBinCompareFwd cpf cfg.cmd bs (processAsms @'ArchArmV8ThumbLE)

cmdPatchBinCompareFwd
    :: forall v m
    .  (MonadIO m, Compare v Bytes)
    => CCmdPatch
    -> [Patch 'FwdSeek '[Compare.Meta v, Bin.Meta] Bytes]
    -> m ()
cmdPatchBinCompareFwd cfg ps = do
    bs <- patchPureBinCompareFwd cfg.streamPair.streamIn ps
    case cfg.streamPair.streamOut of
      CStreamFile fp -> liftIO $ BS.writeFile fp bs
      CStreamStd     -> case cfg.printBinary of
        True  -> liftIO $ BS.putStr bs
        False -> liftIO $ logWarn $
                     "refusing to print binary to stdout:"
                  <> " write to a file with --out-file FILE"
                  <> " or use --print-binary flag to override"

-- TODO allows MonadIO in the n-rank function oops. just because lazy. pain to
-- fix so W/E LOL
runBinCompareFwd
    :: forall a b m
    .  ( MonadIO m, MonadError Error m
       , FromJSON a, BinRep b, Show b
       )
    => CPatchscriptFormat -> CCmd -> Bytes
    -> (forall fs. Traversable (HFunctorList fs) => [Patch 'AbsSeek fs a] -> m [Patch 'AbsSeek fs b])
    -> m ()
runBinCompareFwd psfmt cmd' bs preBinRep =
    case psfmt.align of
      CNoAlign -> case psfmt.compare of
        ViaEq Exact -> do
          ps <-     processDecode bs
                >>= return . concat . map (BP.convertBin @'AbsSeek)
                >>= preBinRep
                >>= processBin @b
                >>= processLinearize
          case cmd' of
            CCmdPatch'   cfgPatch   ->
              cmdPatchBinCompareFwd @('ViaEq 'Exact) cfgPatch ps
            CCmdCompile' _cfgCompile -> do
              let ps' = map (Compile.compilePatch @('ViaHash 'HashFuncB3)) ps
                  ps'' = fmap (fmap HexBytestring . convertBackBin) ps'
              liftIO $ BS.putStr $ YamlPretty.encodePretty yamlPrettyCfg ps''
        ViaHash HashFuncB3 -> do
          ps <-     processDecode bs
                >>= return . concat . map (BP.convertBin @'AbsSeek)
                >>= preBinRep
                >>= processBin @b
                >>= processLinearize
          case cmd' of
            CCmdPatch'   cfgPatch   ->
              cmdPatchBinCompareFwd @('ViaHash 'HashFuncB3) cfgPatch ps
            CCmdCompile' _cfgCompile -> do
              let ps' = fmap (fmap HexBytestring . convertBackBin) ps
              liftIO $ BS.putStr $ YamlPretty.encodePretty yamlPrettyCfg ps'
        _ -> throwError $ ErrorUnimplemented
      CAlign -> case psfmt.compare of
        ViaEq Exact -> do
          ps <-     processDecode bs
                >>= processAlign (BP.convertBinAlign @'AbsSeek)
                >>= preBinRep
                >>= processBin @b
                >>= processLinearize
          case cmd' of
            CCmdPatch'   cfgPatch   ->
              cmdPatchBinCompareFwd @('ViaEq 'Exact) cfgPatch ps
            CCmdCompile' _cfgCompile -> do
              let ps' = map (Compile.compilePatch @('ViaHash 'HashFuncB3)) ps
                  ps'' = fmap (fmap HexBytestring . convertBackBin) ps'
              liftIO $ BS.putStr $ YamlPretty.encodePretty yamlPrettyCfg ps''
        ViaHash HashFuncB3 -> do
          ps <-     processDecode bs
                >>= processAlign (BP.convertBinAlign @'AbsSeek)
                >>= preBinRep
                >>= processBin @b
                >>= processLinearize
          case cmd' of
            CCmdPatch'   cfgPatch   ->
              cmdPatchBinCompareFwd @('ViaHash 'HashFuncB3) cfgPatch ps
            CCmdCompile' _cfgCompile -> do
              let ps' = fmap (fmap HexBytestring . convertBackBin) ps
              liftIO $ BS.putStr $ YamlPretty.encodePretty yamlPrettyCfg ps'
        _ -> throwError $ ErrorUnimplemented

logWarn :: MonadIO m => String -> m ()
logWarn msg = liftIO $ putStrLn $ "bytepatch: warning: " <> msg

wrapAlign
    :: ( SeekRep s ~ Natural
       , r ~ Const (Align.Meta s)
       , rs ~ RDelete r ss
       , RElem r ss i
       , RSubset rs ss is )
    => (MultiPatch 'RelSeek v a -> [Patch 'RelSeek ss a])
    -> Aligned (MultiPatch 'RelSeek v a)
    -> Either (Align.Error s) [Patch s rs a]
wrapAlign f = BP.align . over (the @"alignedPatches") (concat . map f)

quit :: MonadIO m => Error -> m a
quit err = do
    liftIO $ putStrLn $ "bytepatch: error: " <> errStr
    liftIO $ System.exitWith $ System.ExitFailure errExitCode
  where
    (errExitCode, errStr) = case err of
      ErrorYaml     e -> (1, "while parsing YAML: "<>show e)
      ErrorAlign    e -> (2, "while aligning: "<>e)
      ErrorProcessBinRep e -> (3, "while converting to binary representation: "<>e)
      ErrorLinear   e -> (4, "while linearizing: "<>e)
      ErrorProcessApply e -> (5, "while applying patch: "<>e)
      ErrorProcessAsm e -> (6, "while assembling: "<>show e)
      ErrorUnimplemented -> (10, "feature not yet implemented")
      Error         e -> (20, e)

readStream :: MonadIO m => CStream -> m Bytes
readStream s = liftIO $
    case s of
      CStreamStd     -> BS.getContents
      CStreamFile fp -> BS.readFile fp

-- silly stuff
yamlPrettyCfg :: YamlPretty.Config
yamlPrettyCfg = YamlPretty.setConfCompare cmp $ YamlPretty.setConfDropNull True $ YamlPretty.defConfig
  where
    cmp "data" _  = LT
    cmp _  "data" = GT
    cmp "seek" _ = LT
    cmp _ "seek" = GT
    cmp k1     k2 = Prelude.compare k1 k2
