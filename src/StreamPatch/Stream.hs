{- | Stream monads.

By stream, I mean a container indexed by the naturals(/integers). Note that not
all parts of streampatch are limited to this, but it's an extremely useful
invariant, without which patch linearization & application gets a whole lot more
complex. So for now, streams it is.

These are designed to support easy pure and impure implementations. That's the
reasoning behind forward-only streams, and separating overwrites (easy impure,
mid pure) from inserts (hard impure, ease pure).
-}

module StreamPatch.Stream where

import Data.Kind
import Data.ByteString qualified as B
import Data.ByteString.Builder qualified as BB
import Control.Monad.State
import Control.Monad.Reader
import System.IO qualified as IO
import Data.List qualified as List

-- | Streams supporting forward seeking and in-place edits (length never
--   changes).
class Monad m => FwdInplaceStream m where
    type Chunk m :: Type

    -- | The unsigned integral type used for indexing. 'Int' has a sign, but is
    --   often used internally; 'Integer' also gets some use; 'Natural' is the
    --   most mathematically honest. I leave the decision up to the instance to
    --   allow them to be as efficient as possible.
    type Index m :: Type

    -- | Read a number of elements forward without moving the cursor.
    --
    -- Argument must be positive.
    readahead :: Index m -> m (Chunk m)

    -- | Overlay a chunk onto the stream at the current cursor position,
    --   overwriting existing elements.
    --
    -- Moves the cursor to the right by the length of the chunk.
    overwrite :: Chunk m -> m ()

    -- | Move cursor forwards without reading. Must be positive.
    advance :: Index m -> m ()

    -- | Get the current cursor position.
    --
    -- Intended for error messages.
    getCursor :: m (Index m)

instance Monad m => FwdInplaceStream (StateT (B.ByteString, BB.Builder, Int) m) where
    type Chunk (StateT (B.ByteString, BB.Builder, Int) m) = B.ByteString
    type Index (StateT (B.ByteString, BB.Builder, Int) m) = Int
    readahead n = get >>= \(src, _, _) -> return $ B.take n src
    overwrite bs = do
        (src, out, pos) <- get
        let (_, src') = B.splitAt (B.length bs) src
            out' = out <> BB.byteString bs
            pos' = pos + B.length bs
        put (src', out', pos')
    advance n = do
        (src, out, pos) <- get
        let (bs, src') = B.splitAt n src
            out' = out <> BB.byteString bs
            pos' = pos + n
        put (src', out', pos')
    getCursor = get >>= \(_, _, pos) -> return pos

instance MonadIO m => FwdInplaceStream (ReaderT IO.Handle m) where
    type Chunk (ReaderT IO.Handle m) = B.ByteString
    type Index (ReaderT IO.Handle m) = Integer
    readahead n = do
        hdl <- ask
        bs <- liftIO $ B.hGet hdl $ fromInteger n -- TODO Integer -> Int :(
        liftIO $ IO.hSeek hdl IO.RelativeSeek (-n)
        pure bs
    overwrite bs = do
        hdl <- ask
        liftIO $ B.hPut hdl bs
    advance n = do
        hdl <- ask
        liftIO $ IO.hSeek hdl IO.RelativeSeek n
    getCursor = do
        hdl <- ask
        pos <- liftIO $ IO.hTell hdl
        return $ fromInteger pos

-- TODO Need MonoTraversable to define for Text, ByteString etc easily. Bleh. I
-- think Snoyman's advice is to reimplement. Also bleh.
instance Monad m => FwdInplaceStream (StateT ([a], [a], Int) m) where
    type Chunk (StateT ([a], [a], Int) m) = [a]
    type Index (StateT ([a], [a], Int) m) = Int
    readahead n = get >>= \(src, _, _) -> return $ List.take n src
    overwrite bs = do
        (src, out, pos) <- get
        let (_, src') = List.splitAt (List.length bs) src
            out' = out <> bs
            pos' = pos + List.length bs
        put (src', out', pos')
    advance n = do
        (src, out, pos) <- get
        let (bs, src') = List.splitAt (fromIntegral n) src
            out' = out <> bs
            pos' = pos + n
        put (src', out', pos')
    getCursor = get >>= \(_, _, pos) -> return pos

-- | Streams supporting forward seeking and arbitrary edits.
class FwdInplaceStream m => FwdStream m where
    -- | Write a chunk into the stream at the current cursor position.
    --
    -- Moves the cursor to the right by the length of the chunk.
    write :: Chunk m -> m ()

    -- | Delete a sized chunk at the current cursor position.
    --
    -- Argument must be positive.
    delete :: Index m -> m ()

instance Monad m => FwdStream (StateT (B.ByteString, BB.Builder, Int) m) where
    write bs = do
        (src, out, pos) <- get
        let out' = out <> BB.byteString bs
            pos' = pos + B.length bs
        put (src, out', pos')
    delete n = do
        (src, out, pos) <- get
        let src' = B.drop n src
        put (src', out, pos)
