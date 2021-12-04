{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main ( main ) where

import           Config
import qualified Options

import           BytePatch.Pretty
import           BytePatch.Patch.Binary                 ( BinRep )
import qualified BytePatch.Patch.Binary.HexByteString   as Bin
import           BytePatch.JSON()
import qualified BytePatch.Patch                        as Patch
import qualified BytePatch.Linear                       as Linear

import           Control.Monad.IO.Class
import qualified Data.ByteString                        as BS
import qualified Data.ByteString.Lazy                   as BL
import qualified Data.Yaml                              as Yaml
import           Data.Aeson                             ( FromJSON )

import qualified Data.Text.Encoding                     as Text
import           Data.Text                              ( Text )
import           BytePatch.Core
import           Data.Functor.Const

main :: IO ()
main = Options.parse >>= run

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

-- hilarious little bit
run' :: MonadIO m => Config -> m ()
run' cfg = do
    tryDecodeYaml @[Patch 'FwdSeek (Const ()) Text] (cfgPatchscript cfg) >>= \case
      Nothing -> quit "couldn't parse patchscript"
      Just ps -> do
        d <- Text.decodeUtf8 <$> readStream' io
        let d' = Patch.patchPureText ps d
        writeStream' io $ Text.encodeUtf8 d'
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

tryReadPatchscript :: forall a m. (BinRep a, FromJSON a, MonadIO m) => FilePath -> m (Maybe [CommonMultiEdits a])
tryReadPatchscript = tryDecodeYaml

tryDecodeYaml :: (FromJSON a, MonadIO m) => FilePath -> m (Maybe a)
tryDecodeYaml fp = do
    bs <- liftIO $ BS.readFile fp
    case Yaml.decodeEither' bs of
      Left  _      -> return Nothing
      Right parsed -> return $ Just parsed