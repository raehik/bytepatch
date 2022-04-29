{-# LANGUAGE AllowAmbiguousTypes, OverloadedRecordDot #-}

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
import           StreamPatch.Patch.Compare ( Via(..), SVia(..), SEqualityCheck(..), HashFunc(..), SHashFunc(..), Compare )
import qualified StreamPatch.Apply as Apply
import           BytePatch as BP
import           Raehik.HexBytestring
import Binrep.Type.Assembly qualified as Asm
import Binrep.Type.Assembly.Assemble qualified as Asm
import Binrep.Type.Assembly.Assemble ( Assemble )
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
import           Data.Text ( Text )
import           Optics
import           Data.Generics.Product.Any

import Data.Singletons ( withSomeSing, Sing, SingI )

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
  | ErrorProcessAssemble String
  | ErrorProcessApply String
    deriving (Generic, Show)

main :: IO ()
main = CLI.parse >>= runReaderT run

processDecode :: forall a m. (MonadError Error m, FromJSON a) => Bytes -> m a
processDecode = liftMapProcessError ErrorYaml . Yaml.decodeEither'

processAlign
    :: forall (v :: Via) a s m r rs ss is
    .  ( SeekRep s ~ Natural
       , r ~ Const (Align.Meta s)
       , rs ~ RDelete r ss
       , RElem r ss (RIndex r ss)
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
    :: forall (arch :: Asm.Arch) s fs m
    .  (MonadError Error m, Assemble arch, Traversable (HFunctorList fs))
    => [Patch s fs (Asm.AsmInstr arch)]
    -> m [Patch s fs (Asm.MachineCode arch)]
processAsm =
      liftMapProcessError ErrorProcessAssemble
    . traverse (traverse (Asm.assemble1 @arch))

processAsms
    :: forall (arch :: Asm.Arch) s fs m
    .  (MonadError Error m, Assemble arch, Traversable (HFunctorList fs))
    => [Patch s fs [Asm.AsmInstr arch]]
    -> m [Patch s fs (Asm.MachineCode arch)]
processAsms =
      liftMapProcessError ErrorProcessAssemble
    . traverse (traverse (Asm.assemble @arch))

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
      ErrorProcessAssemble e -> (6, "while assembling: "<>e)
      ErrorUnimplemented -> (10, "feature not yet implemented")
      Error         e -> (20, e)

readStream :: MonadIO m => CStream -> m Bytes
readStream s = liftIO $
    case s of
      CStreamStd     -> BS.getContents
      CStreamFile fp -> BS.readFile fp

run :: forall m. (MonadIO m, MonadReader Config m) => m ()
run = readPatchscriptBs >>= liftProcessError . usePatchscriptBs
  where
    readPatchscriptBs = asks patchscriptPath >>= readStream . CStreamFile
    usePatchscriptBs bs = do
        cfg <- ask
        let cpf = cfg.patchscriptFormat
        withSomeSing cpf.compare (f cfg bs)

    -- using singletons simply to automatically bring type into scope
    f :: forall (v :: Via). Config -> Bytes -> Sing v -> ExceptT Error m ()
    f cfg bs = \case
          SViaEq   SExact      -> do
            ps <- prep @v cfg.patchscriptFormat bs
            case cfg.cmd of
              CCmdPatch'   cfg' -> cmdPatchBinCompareFwd cfg' ps
              CCmdCompile' cfg' ->
                let ps' = map (Compile.compilePatch @('ViaHash 'HashFuncB3)) ps
                 in Compile.runCompileCompareBin cfg' ps'
          SViaHash SHashFuncB3 -> do
            ps <- prep @v cfg.patchscriptFormat bs
            case cfg.cmd of
              CCmdPatch'   cfg' -> cmdPatchBinCompareFwd cfg' ps
              CCmdCompile' cfg' -> Compile.runCompileCompareBin cfg' ps
          _ -> throwError $ ErrorUnimplemented

-- Parse and prepare/normalize a binrep-compare patchscript, polymorphic on the
-- comparison strategy. We can't handle that in here, because you need it when
-- processing the command, since different comparison strategies require
-- different handling, and some are invalid.
prep
    :: forall v m
    -- grr
     . ( MonadError Error m
       , FromJSON (Compare.CompareRep v Text)
       , FromJSON (Compare.CompareRep v HexBytestring)
       , FromJSON (Compare.CompareRep v (Asm.AsmInstr 'Asm.ArchArmV8ThumbLE))
       , FromJSON (Compare.CompareRep v [Asm.AsmInstr 'Asm.ArchArmV8ThumbLE])
       , Show     (Compare.CompareRep v Bytes)
       -- , Traversable (Compare.Meta v)
       , SingI v
       )
    => CPatchscriptFormat
    -> Bytes
    -> m [Patch 'FwdSeek '[Compare.Meta v, Bin.Meta] Bytes]
prep cfg bs = case cfg.dataType of
  CTextPatch    -> throwError $ ErrorUnimplemented
  CBinPatch     -> prep' @HexBytestring cfg pure bs
  CTextBinPatch -> prep' @Text          cfg pure bs
  CAsmBinPatch  -> prep' cfg (processAsm @'Asm.ArchArmV8ThumbLE) bs
  CAsmsBinPatch -> prep' cfg (processAsms @'Asm.ArchArmV8ThumbLE) bs

-- Binrep-compare, parsing @a@ and failably converting to @b@, In many cases,
-- you may want to parse and binrep the same type -- in such cases, use 'pure'.
prep'
    :: forall a b v m
    .  ( FromJSON a, BinRep b, Show b
       , FromJSON (Compare.CompareRep v a)
       , SingI v
       , Show     (Compare.CompareRep v Bytes)
       , MonadError Error m
       )
    => CPatchscriptFormat
    -> (forall s fs. Traversable (HFunctorList fs) => [Patch s fs a] -> m [Patch s fs b])
    -> Bytes
    -> m [Patch 'FwdSeek '[Compare.Meta v, Bin.Meta] Bytes]
prep' cfg fBin bs = withSomeSing cfg.seek go
  where
    go :: forall (s :: SeekKind). Sing s -> m [Patch 'FwdSeek '[Compare.Meta v, Bin.Meta] Bytes]
    go = \case
      SAbsSeek -> case cfg.align of
        CAlign ->     processDecode bs
                  >>= processAlign @v (BP.convertBinAlign @s)
                  >>= fBin
                  >>= processBin @b
                  >>= processLinearize
        CNoAlign ->     processDecode bs
                    >>= return . concat . map (BP.convertBin @s)
                    >>= fBin
                    >>= processBin @b
                    >>= processLinearize
      SFwdSeek -> case cfg.align of
        CAlign ->     processDecode bs
                  >>= processAlign @v (BP.convertBinAlign @s)
                  >>= fBin
                  >>= processBin @b
        CNoAlign ->     processDecode bs
                    >>= return . concat . map (BP.convertBin @s)
                    >>= fBin
                    >>= processBin @b
      SRelSeek -> throwError $ ErrorUnimplemented
