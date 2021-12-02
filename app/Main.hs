{-# LANGUAGE ScopedTypeVariables #-}

module Main ( main ) where

import           Config
import qualified Options

import           BytePatch.Pretty
import           BytePatch.Pretty.PatchRep
import           BytePatch.JSON
import qualified BytePatch.Linear.Patch     as Linear
import qualified BytePatch.Linear.Gen       as Linear

import           Control.Monad.IO.Class
import qualified Data.ByteString            as BS
import qualified Data.Yaml                  as Yaml
import           Data.Aeson                 ( FromJSON )
import           Data.Text                  ( Text )

type Bytes = BS.ByteString

main :: IO ()
main = Options.parse >>= run

run :: MonadIO m => Config -> m ()
run cfg = do
    return ()
    tryReadPatchscript @Text (cfgPatchscript cfg) >>= \case
      Nothing -> quit "couldn't parse patchscript"
      Just ps ->
        case normalizeSimple ps of
          Nothing -> quit "failed while normalizing patchscript"
          Just p  ->
            case Linear.gen p of
              (_, _:_)  -> quit "failed while generating patchscript"
              (ps', []) -> quit "TODO actually patch"

quit :: MonadIO m => String -> m ()
quit = liftIO . putStrLn

tryReadPatchscript :: forall a m. (PatchRep a, FromJSON a, MonadIO m) => FilePath -> m (Maybe [MultiPatches a])
tryReadPatchscript = tryDecodeYaml

tryDecodeYaml :: (FromJSON a, MonadIO m) => FilePath -> m (Maybe a)
tryDecodeYaml fp = do
    bs <- liftIO $ BS.readFile fp
    case Yaml.decodeEither' bs of
      Left  _      -> return Nothing
      Right parsed -> return $ Just parsed
