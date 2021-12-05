{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module Main ( main ) where

import           Config
import qualified Options

import qualified BytePatch.Apply                        as Apply
import qualified BytePatch.Patch.Binary                 as Bin
import           BytePatch.Patch.Binary                 ( BinRep )
import qualified BytePatch.Patch.Binary.HexByteString   as Bin
import qualified BytePatch.Align                        as Align
import           BytePatch.JSON()

import           Control.Monad.IO.Class
import qualified Data.ByteString                        as BS
import qualified Data.ByteString.Lazy                   as BL
import qualified Data.Yaml                              as Yaml
import           Data.Aeson                             ( FromJSON )

import qualified Data.Text                              as Text
import qualified Data.Text.Encoding                     as Text
import           Data.Text                              ( Text )
import           BytePatch.Patch
import           Data.Functor.Const

main :: IO ()
main = Options.parse >>= run''

{-
run :: MonadIO m => Config -> m ()
run cfg = do
    tryReadPatchscript @Bin.HexByteString (cfgPatchscript cfg) >>= \case
      Nothing -> quit "couldn't parse patchscript"
      Just ps ->
        case normalizeSimple ps of
          Nothing -> quit "failed while normalizing patchscript"
          Just p  ->
            case Linear.gen p of
              (_, genErrs@(_:_)) -> quit' "patchscript generation failed" genErrs
              (ps', []) -> do
                bs <- readStream' io
                case Patch.patchBinPure (cfgPatchCfg cfg) ps' bs of
                  Left patchErr -> quit' "patching failed" patchErr
                  Right bs' -> writeStream' io (BL.toStrict bs')
  where
    io = cfgStreamInOut cfg
-}

-- hilarious little bit
run' :: MonadIO m => Config -> m ()
run' cfg = do
    tryDecodeYaml @[Patch 'FwdSeek (Const ()) String] (cfgPatchscript cfg) >>= \case
      Nothing -> quit "couldn't parse patchscript"
      Just ps -> do
        d <- Text.unpack . Text.decodeUtf8 <$> readStream' io
        let d' = Apply.patchListPure ps d
        writeStream' io $ Text.encodeUtf8 $ Text.pack d'
  where
    io = cfgStreamInOut cfg

run'' :: MonadIO m => Config -> m ()
run'' cfg = do
    -- Align.Aligned (Patch 'RelSeek (Align.Meta 'FwdSeek Bin.Meta) Text)
    -- = "a text patch that aligns to forward-seek" (+ bin meta??)
    tryDecodeYaml @(Align.Aligned (Patch 'RelSeek (Align.Meta 'FwdSeek Bin.Meta) Text)) (cfgPatchscript cfg) >>= \case
      Nothing -> quit "couldn't parse patchscript"
      Just ps ->
        case Align.align ps of
          Left  err -> quit' "couldn't align patchscript" err
          Right ps' -> quit "TODO"
  where
    io = cfgStreamInOut cfg

readStream :: MonadIO m => CStream -> m BS.ByteString
readStream stream = liftIO $
    case stream of
      CStreamStd     -> BS.getContents
      CStreamFile fp -> BS.readFile fp

readStream' :: MonadIO m => CStreamInOut -> m BS.ByteString
readStream' = readStream . fst . unCStreamInOut

writeStream :: MonadIO m => CStream -> BS.ByteString -> m ()
writeStream stream bs = liftIO $
    case stream of
      CStreamStd     -> BS.putStr bs
      CStreamFile fp -> BS.writeFile fp bs

writeStream' :: MonadIO m => CStreamInOut -> BS.ByteString -> m ()
writeStream' stream = writeStream (snd (unCStreamInOut stream))

quit' :: (MonadIO m, Show a) => String -> a -> m ()
quit' msg a = liftIO $ do
    putStrLn $ "bytepatch: error: " <> msg
    print a

quit :: MonadIO m => String -> m ()
quit = liftIO . putStrLn

--tryReadPatchscript :: forall a m. (BinRep a, FromJSON a, MonadIO m) => FilePath -> m (Maybe [BinMultiPatches a])
tryReadPatchscript :: forall a m. (BinRep a, FromJSON a, MonadIO m) => FilePath -> m (Maybe [MultiPatch 'AbsSeek Bin.Meta a])
tryReadPatchscript = tryDecodeYaml

tryDecodeYaml :: forall a m. (FromJSON a, MonadIO m) => FilePath -> m (Maybe a)
tryDecodeYaml fp = do
    bs <- liftIO $ BS.readFile fp
    case Yaml.decodeEither' bs of
      Left  err    -> liftIO (print err) >> return Nothing
      Right parsed -> return $ Just parsed

showPatchFile :: forall a m. (FromJSON a, Show a, MonadIO m) => FilePath -> m ()
showPatchFile fp = do
    tryDecodeYaml @a fp >>= \case
      Nothing -> quit "couldn't parse patchscript"
      Just ps -> liftIO $ print ps
