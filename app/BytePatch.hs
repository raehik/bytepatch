{-# LANGUAGE AllowAmbiguousTypes, OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module BytePatch where

import BytePatch.Config
import Raehik.CLI.Stream
import BytePatch.CLI qualified as CLI
import StreamPatch.Patch.Compile qualified as Compile

import GHC.Generics ( Generic )

import StreamPatch.Patch
import StreamPatch.HFunctorList ( Flap, HFunctorList )
import StreamPatch.Patch.Align qualified as Align
import StreamPatch.Patch.Linearize.InPlace qualified as Linear
import StreamPatch.Patch.Binary qualified as Bin
import StreamPatch.Patch.Compare qualified as Compare
import StreamPatch.Patch.Compare ( Via(..), SVia(..), SEqualityCheck(..), HashFunc(..), SHashFunc(..), Compare, CompareRep )
import StreamPatch.Apply qualified as Apply
import StreamPatch.Simple as Simple

import Binrep.Extra.HexByteString

import Binrep.Type.Assembly qualified as Asm
import Binrep.Type.Assembly.Assemble qualified as Asm
import Binrep.Type.Assembly.Assemble ( Assemble )
import Binrep.Type.Text qualified as BR.Text
import Binrep.Type.ByteString qualified as BR.ByteString

import Binrep
import Binrep.Type.ByteString ( AsByteString )

import Refined

import qualified System.Exit as System
import Data.Vinyl
import Data.Vinyl.TypeLevel
import Data.Functor.Const
import Data.Either.Combinators ( mapLeft )
import Control.Monad.IO.Class
import Control.Monad.Reader
import Control.Monad.Except
import Data.ByteString qualified as B
import Data.ByteString.Lazy qualified as BL
import Data.Yaml qualified as Yaml
import Data.Yaml.Pretty qualified as Yaml.Pretty
import Data.Aeson ( ToJSON, FromJSON )
import Data.Text ( Text )
import Optics
import Data.Generics.Product.Any

import Data.Singletons ( withSomeSing, Sing, SingI )

type Bytes = B.ByteString

--------------------------------------------------------------------------------

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

logWarn :: MonadIO m => String -> m ()
logWarn msg = liftIO $ putStrLn $ "bytepatch: warning: " <> msg

liftProcessError :: MonadIO m => ExceptT Error m a -> m a
liftProcessError action =
    runExceptT action >>= \case
      Left  e -> quit e
      Right a -> return a

liftMapProcessError :: MonadError Error m => (e -> Error) -> Either e a -> m a
liftMapProcessError f = liftEither . mapLeft f

--------------------------------------------------------------------------------

processDecode :: forall a m. (MonadError Error m, FromJSON a) => Bytes -> m a
processDecode = liftMapProcessError ErrorYaml . Yaml.decodeEither'

processAlign
    :: forall (v :: Via) a m r rs ss is
    .  ( r ~ Const (Align.Meta Int)
       , rs ~ RDelete r ss
       , RElem r ss (RIndex r ss)
       , RSubset rs ss is
       , MonadError Error m )
    => (MultiPatch Integer v a -> Patch Integer ss a)
    -> [Aligned (MultiPatch Integer v a)]
    -> m [Patch Int rs a]
processAlign f = liftMapProcessError (ErrorAlign . show) . fmap concat . traverse (wrapAlign f)

processLinearize
    :: forall a m fs
    .  ( Linear.HasLength a
       , Show a, ReifyConstraint Show (Flap a) fs, RMap fs, RecordToList fs
       , MonadError Error m )
    => [Patch Linear.Len fs a]
    -> m [Patch Linear.Len fs a]
processLinearize = liftMapProcessError (ErrorLinear . show) . Linear.linearizeInPlace

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
       , Put a, BLen a
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

--------------------------------------------------------------------------------

main :: IO ()
main = CLI.parse >>= runReaderT run

run :: forall m. (MonadIO m, MonadReader C m) => m ()
run = readPatchscriptBs >>= liftProcessError . usePatchscriptBs
  where
    readPatchscriptBs = asks cPsPath >>= liftIO . B.readFile . unPath
    usePatchscriptBs bs = do
        cfg <- ask
        let cpf = cfg.cPsFmt
        withSomeSing cpf.cPsFmtCompare (process cfg bs)

-- using singletons simply to automatically bring type into scope
process
    :: forall (v :: Via) m
    .  ( MonadIO m, MonadReader C m )
    => C -> Bytes -> Sing v -> ExceptT Error m ()
process cfg bs = \case
      SViaEq SExact -> do
        ps <- prep @v cfg.cPsFmt bs
        case cfg.cCmd of
          CCmdPatch'   cfg'  -> cmdPatchBinCompareFwd cfg' ps
          CCmdCompile' _cfg' ->
            let ps'  = map (Compile.compilePatch @('ViaDigest 'B3)) ps
                ps'' = asPrettyYamlHexMultiPatch convertBackBin ps'
             in liftIO $ B.putStr ps''
      SViaDigest SB3 -> do
        ps <- prep @v cfg.cPsFmt bs
        case cfg.cCmd of
          CCmdPatch'   cfg'  -> cmdPatchBinCompareFwd cfg' ps
          CCmdCompile' _cfg' ->
              let ps' = asPrettyYamlHexMultiPatch convertBackBin ps
               in liftIO $ B.putStr ps'
      SViaEq SPrefixOf -> do
        ps <- prep @v cfg.cPsFmt bs
        case cfg.cCmd of
          CCmdPatch'   cfg'  -> cmdPatchBinCompareFwd cfg' ps
          CCmdCompile' _cfg' -> throwError $ ErrorUnimplemented
      _ -> throwError $ ErrorUnimplemented

patchPureBinCompareFwd
    :: forall v s m
    .  (MonadIO m, Compare v Bytes)
    => Stream 'In s
    -> [Patch Int '[Compare.Meta v, Bin.Meta] Bytes]
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
    -> [Patch Int '[Compare.Meta v, Bin.Meta] Bytes]
    -> m ()
cmdPatchBinCompareFwd c ps = do
    bs <- patchPureBinCompareFwd c.cCmdPatchStreamIn ps
    case c.cCmdPatchStreamOut of
      Path' (Path fp) -> liftIO $ B.writeFile fp bs
      Std      -> case c.cCmdPatchPrintBinary of
        True  -> liftIO $ B.putStr bs
        False -> liftIO $ logWarn $
                     "refusing to print binary to stdout:"
                  <> " write to a file with --out-file FILE"
                  <> " or use --print-binary flag to override"

-- TODO fix all of this, it got weird with seekrep removal
wrapAlign
    :: ( r ~ Const (Align.Meta Int)
       , rs ~ RDelete r ss
       , RElem r ss i
       , RSubset rs ss is )
    => (MultiPatch Integer v a -> Patch Integer ss a)
    -> Aligned (MultiPatch Integer v a)
    -> Either (Align.Error Int) [Patch Int rs a]
wrapAlign f = Simple.align . over (the @"alignedPatches") (map f)

readStream :: forall s m. MonadIO m => Stream 'In s -> m Bytes
readStream = liftIO . \case Std             -> B.getContents
                            Path' (Path fp) -> B.readFile fp

-- Parse and prepare/normalize a binrep-compare patchscript, polymorphic on the
-- comparison strategy. We can't handle that in here, because you need it when
-- processing the command, since different comparison strategies require
-- different handling, and some are invalid.
prep
    :: forall v m
    -- grr
     . ( MonadError Error m
       , FromJSON (CompareRep v Text)
       , FromJSON (CompareRep v HexByteString)
       , FromJSON (CompareRep v (Asm.AsmInstr 'Asm.ArmV8ThumbLE))
       , FromJSON (CompareRep v [Asm.AsmInstr 'Asm.ArmV8ThumbLE])
       , Show     (CompareRep v Bytes)
       -- , Traversable (Compare.Meta v)
       , SingI v
       )
    => CPsFmt
    -> Bytes
    -> m [Patch Int '[Compare.Meta v, Bin.Meta] Bytes]
prep c bs = case c.cPsFmtData of
  CDataBytes -> prep' @HexByteString c (return . fmap unHexPatch) bs
  CDataTextBin BR.Text.UTF8 BR.ByteString.C ->
      prep' @Text @(AsByteString 'BR.ByteString.C) c (binTextifyPatches @'BR.Text.UTF8) bs
  CDataAsm Asm.ArmV8ThumbLE -> prep' c (processAsm @'Asm.ArmV8ThumbLE) bs
  CDataText -> throwError ErrorUnimplemented
  cDataX -> throwError $ Error $ "can't yet handle patchscript data type: "<>show cDataX

unHexPatch
    :: Functor (HFunctorList fs)
    => Patch s fs HexByteString
    -> Patch s fs B.ByteString
unHexPatch = fmap unHex

binTextifyPatches
    :: forall (enc :: BR.Text.Encoding) (rep :: BR.ByteString.Rep) s fs m
    .  ( Predicate enc Text, BR.Text.Encode enc, Predicate rep B.ByteString
       , MonadError Error m, Traversable (HFunctorList fs) )
    => [Patch s fs Text]
    -> m [Patch s fs (AsByteString rep)]
binTextifyPatches = traverse (binTextifyPatch @enc)

binTextifyPatch
    :: forall (enc :: BR.Text.Encoding) (rep :: BR.ByteString.Rep) s fs m
    .  ( Predicate enc Text, BR.Text.Encode enc, Predicate rep B.ByteString
       , MonadError Error m, Traversable (HFunctorList fs) )
    => Patch s fs Text
    -> m (Patch s fs (AsByteString rep))
binTextifyPatch p = liftEither $ do
    pTextEnc <- liftMapProcessError (Error . show) $ traverse (refine @enc) p
    pBs <- liftMapProcessError (Error . show) $ traverse (BR.Text.encodeToRep @rep) pTextEnc
    return pBs

-- Binrep-compare, parsing @a@ and failably converting to @b@, In many cases,
-- you may want to parse and binrep the same type -- in such cases, use 'pure'.
prep'
    :: forall a b v m
    .  ( FromJSON a, Put b, BLen b, Show b
       , FromJSON (CompareRep v a)
       , SingI v
       , Show     (CompareRep v Bytes)
       , MonadError Error m
       )
    => CPsFmt
    -> (forall s fs. Traversable (HFunctorList fs) => [Patch s fs a] -> m [Patch s fs b])
    -> Bytes
    -> m [Patch Int '[Compare.Meta v, Bin.Meta] Bytes]
prep' c fBin bs =
    case c.cPsFmtAlign of
      CAlign ->     processDecode bs
                >>= processAlign @v Simple.convertBinAlign
                >>= fBin
                >>= processBin @b
                >>= processLinearize
      CNoAlign ->     processDecode bs
                  >>= return . map Simple.convertBin
                  >>= fBin
                  >>= processBin @b
                  >>= processLinearize

asPrettyYamlHexMultiPatch
    :: forall v s a fs
    .  ( ToJSON (CompareRep v (Hex a))
       , ToJSON (Hex a)
       , ToJSON s
       , Functor (MultiPatch s v)
       )
    => (Patch s fs a -> MultiPatch s v a)
    -> [Patch s fs a]
    -> B.ByteString
asPrettyYamlHexMultiPatch f ps =
      let psSimple    = fmap f ps
          psSimpleHex = fmap (fmap Hex) psSimple
       in Yaml.Pretty.encodePretty yamlPrettyCfg psSimpleHex

-- silly stuff
yamlPrettyCfg :: Yaml.Pretty.Config
yamlPrettyCfg = Yaml.Pretty.setConfCompare cmp $ Yaml.Pretty.setConfDropNull True $ Yaml.Pretty.defConfig
  where
    cmp "data" _  = LT
    cmp _  "data" = GT
    cmp "seek" _ = LT
    cmp _ "seek" = GT
    cmp k1     k2 = Prelude.compare k1 k2
