{-# LANGUAGE DataKinds, TypeFamilies #-}

{- | Low-level patchscript processing and application.

Patchscripts are applied as a list of @(skip x, write in-place y)@ commands. An
offset-based format is much simpler to use, however. This module processes such
offset patchscripts into a "linear" patchscript, and provides a stream patching
algorithm that can be applied to any forward-seeking byte stream.

Some core types are parameterized over the stream type/patch content. This
enables writing patches in any form (e.g. UTF-8 text), which are then processed
into an applicable patch by transforming edits into a concrete binary
representation (e.g. null-terminated UTF-8 bytestring). See TODO module for
more.

TODO old docs. This is now heavily parameterized over patch and stream type, and
things are split up into lots of modules.
-}

module BytePatch.Patch
  (
  -- * Patch interface
    MonadFwdStream(..)
  , MonadCursorStream(..)
  , Error(..)

  -- * Prepared patchers
  , patchBinPure

  -- * General patchers
  , patchBin
  , patch

  ) where

import           BytePatch.Core
import           BytePatch.Patch.Binary     ( BinRep(..) )
import qualified BytePatch.Patch.Binary     as Bin

import           GHC.Natural
import           Data.Kind
import           Data.Functor.Const
import qualified Data.ByteString            as BS
import qualified Data.ByteString.Lazy       as BL
import qualified Data.ByteString.Builder    as BB
import           Control.Monad.State
import           Control.Monad.Reader
import           System.IO                  ( Handle, SeekMode(..), hSeek )

type Bytes = BS.ByteString

-- TODO also require reporting cursor position (for error reporting)
class Monad m => MonadFwdStream m where
    type Chunk m :: Type

    -- | Read a number of bytes without advancing the cursor.
    readahead :: Natural -> m (Chunk m)

    -- | Advance cursor without reading.
    advance :: Natural -> m ()

    -- | Insert bytes into the stream at the cursor position, overwriting
    --   existing bytes.
    overwrite :: Chunk m -> m ()

instance Monad m => MonadFwdStream (StateT (Bytes, BB.Builder) m) where
    type Chunk (StateT (Bytes, BB.Builder) m) = Bytes
    readahead n = BS.take (fromIntegral n) <$> gets fst
    advance n = do
        (src, out) <- get
        let (bs, src') = BS.splitAt (fromIntegral n) src
        put (src', out <> BB.byteString bs)
    overwrite bs = do
        (src, out) <- get
        let (_, src') = BS.splitAt (BS.length bs) src
        put (src', out <> BB.byteString bs)

instance MonadIO m => MonadFwdStream (ReaderT Handle m) where
    type Chunk (ReaderT Handle m) = Bytes
    readahead n = do
        hdl <- ask
        bs <- liftIO $ BS.hGet hdl (fromIntegral n)
        liftIO $ hSeek hdl RelativeSeek (- fromIntegral n)
        return bs
    advance n = do
        hdl <- ask
        liftIO $ hSeek hdl RelativeSeek (fromIntegral n)
    overwrite bs = do
        hdl <- ask
        liftIO $ BS.hPut hdl bs

-- | A forward stream, but backward too.
class MonadFwdStream m => MonadCursorStream m where
    -- | Move cursor.
    move :: Integer -> m ()

instance MonadIO m => MonadCursorStream (ReaderT Handle m) where
    move n = do
        hdl <- ask
        liftIO $ hSeek hdl RelativeSeek (fromIntegral n)

-- | Errors encountered during patch time.
data Error a
  = ErrorPatchOverlong
  | ErrorPatchUnexpectedNonnull
  | ErrorPatchDidNotMatchExpected Bytes Bytes
  | ErrorPatchDataFailedToConvertToBytes a String -- TODO used for both data and expected
    deriving (Eq, Show)

data ErrorBin a
  = ErrorBin (Bin.Error a)
  | ErrorBinPatchError (Error a)
    deriving (Eq, Show)

patchBin
    :: (BinRep a, MonadFwdStream m, Chunk m ~ Bytes)
    => Bin.Cfg -> [Patch 'FwdSeek Bin.Meta a]
    -> m (Maybe (ErrorBin a))
patchBin cfg = go
  where
    go [] = return Nothing
    go (Patch n (Edit ed meta):es) = do
        case toBinRep ed of
          Left errBinRep ->
            return $ Just $ ErrorBin $ Bin.ErrorBadBinRep ed errBinRep
          Right bs -> do
            advance n
            bsStream <- readahead $ fromIntegral $ BS.length bs -- TODO catch overlong error
            case Bin.check cfg bsStream meta of
              Just err -> return $ Just $ ErrorBin err
              Nothing  -> overwrite bs >> go es

-- | Attempt to apply a patchscript to a 'Data.ByteString.ByteString'.
patchBinPure :: Bin.Cfg -> [Patch 'FwdSeek Bin.Meta Bytes] -> BS.ByteString -> Either (ErrorBin Bytes) BL.ByteString
patchBinPure cfg ps bs =
    let (mErr, (bsRemaining, bbPatched)) = runState (patchBin cfg ps) (bs, mempty)
        bbPatched' = bbPatched <> BB.byteString bsRemaining
     in case mErr of
          Just err -> Left err
          Nothing  -> Right $ BB.toLazyByteString bbPatched'

-- LMAO this is awesome. it's so useless and yet. beautiful
patch
    :: (MonadFwdStream m, Chunk m ~ a)
    => [Patch 'FwdSeek (Const ()) a]
    -> m ()
patch = mapM_ $ \(Patch n (Edit d (Const ()))) -> advance n >> overwrite d
