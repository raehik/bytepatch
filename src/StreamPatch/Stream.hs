module StreamPatch.Stream where

import           GHC.Natural
import           Data.Kind
import qualified Data.ByteString            as BS
import qualified Data.ByteString.Builder    as BB
import           Control.Monad.State
import           Control.Monad.Reader
import           System.IO                  ( Handle, SeekMode(..), hSeek )

import qualified Data.List                  as List

-- TODO also require reporting cursor position (for error reporting)
class Monad m => MonadFwdInplaceStream m where
    type Chunk m :: Type

    -- | Read a number of bytes without advancing the cursor.
    readahead :: Natural -> m (Chunk m)

    -- | Advance cursor without reading.
    advance :: Natural -> m ()

    -- | Insert bytes into the stream at the cursor position, overwriting
    --   existing bytes.
    overwrite :: Chunk m -> m ()

-- TODO Need MonoTraversable to define for Text, ByteString etc easily. Bleh. I
-- think Snoyman's advice is to reimplement. Also bleh.
instance Monad m => MonadFwdInplaceStream (StateT ([a], [a]) m) where
    type Chunk (StateT ([a], [a]) m) = [a]
    readahead n = List.take (fromIntegral n) <$> gets fst
    advance n = do
        (src, out) <- get
        let (bs, src') = List.splitAt (fromIntegral n) src
        put (src', out <> bs)
    overwrite bs = do
        (src, out) <- get
        let (_, src') = List.splitAt (List.length bs) src
        put (src', out <> bs)

instance MonadIO m => MonadFwdInplaceStream (ReaderT Handle m) where
    type Chunk (ReaderT Handle m) = BS.ByteString
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

instance Monad m => MonadFwdInplaceStream (StateT (BS.ByteString, BB.Builder) m) where
    type Chunk (StateT (BS.ByteString, BB.Builder) m) = BS.ByteString
    readahead n = BS.take (fromIntegral n) <$> gets fst
    advance n = do
        (src, out) <- get
        let (bs, src') = BS.splitAt (fromIntegral n) src
        put (src', out <> BB.byteString bs)
    overwrite bs = do
        (src, out) <- get
        let (_, src') = BS.splitAt (BS.length bs) src
        put (src', out <> BB.byteString bs)

-- | A forward stream, but backward too.
class MonadFwdInplaceStream m => MonadCursorInplaceStream m where
    -- | Move cursor.
    move :: Integer -> m ()

instance MonadIO m => MonadCursorInplaceStream (ReaderT Handle m) where
    move n = do
        hdl <- ask
        liftIO $ hSeek hdl RelativeSeek (fromIntegral n)

class MonadFwdInplaceStream m => MonadFwdStream m where
    insert :: Chunk m -> m ()
