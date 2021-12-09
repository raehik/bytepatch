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

module BytePatch.Apply
  (
  -- * Patch interface
    MonadFwdStream(..)
  , MonadCursorStream(..)

  -- * Prepared patchers
  , patchListPure

  -- * General patchers
  , patch
  , patchBin

  ) where

import           BytePatch.Patch
import           BytePatch.Patch.Binary     ( BinRep(..) )
import qualified BytePatch.Patch.Binary     as Bin

import           GHC.Natural
import           Data.Kind
import           Data.Functor.Const
import           Data.Either.Combinators
import qualified Data.ByteString            as BS
import qualified Data.ByteString.Lazy       as BL
import qualified Data.ByteString.Builder    as BB
import           Control.Monad.State
import           Control.Monad.Reader
import           System.IO                  ( Handle, SeekMode(..), hSeek )

import qualified Data.List                  as List

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

-- TODO Need MonoTraversable to define for Text, ByteString etc easily. Bleh. I
-- think Snoyman's advice is to reimplement. Also bleh.
instance Monad m => MonadFwdStream (StateT ([a], [a]) m) where
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

-- | A forward stream, but backward too.
class MonadFwdStream m => MonadCursorStream m where
    -- | Move cursor.
    move :: Integer -> m ()

instance MonadIO m => MonadCursorStream (ReaderT Handle m) where
    move n = do
        hdl <- ask
        liftIO $ hSeek hdl RelativeSeek (fromIntegral n)

patch
    :: (MonadFwdStream m, Chunk m ~ a)
    => [Patch 'FwdSeek (Const ()) (Const ()) a]
    -> m ()
patch = mapM_ $ \(Patch d (Const ()) (Pos n (Const ()))) -> advance n >> overwrite d

patchBin
    :: (MonadFwdStream m, Chunk m ~ a)
    => [Patch 'FwdSeek (Const ()) (Bin.MetaPos (Const ())) a]
    -> m (Either TODO_ERRORS ())
patchBin = undefined

patchListPure :: [Patch 'FwdSeek (Const ()) (Const ()) [a]] -> [a] -> [a]
patchListPure ps a =
    let (aRemaining, aPatched) = execState (patch ps) (a, mempty)
     in aPatched <> aRemaining
