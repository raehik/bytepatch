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
-}

module BytePatch.Linear.Patch
  (
  -- * Patch interface
    MonadFwdStream(..)
  , MonadCursorStream(..)
  , Error(..)

  -- * Prepared patchers
  , patchPure

  -- * General patcher
  , patchBytes

  ) where

import           BytePatch.Core
import           BytePatch.Patch.Binary     ( BinRep(..) )
import qualified BytePatch.Patch.Binary     as Bin

import           GHC.Natural
import           Data.Kind
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

-- This is fun: there is minimal extra fuss in adding the patch representation
-- stuff directly at patch time as well. I prefer doing as much as possible
-- before applying a patch, but no problem, @toBinRep (_ :: Bytes) = id@.
patchBytes
    :: (BinRep a, MonadFwdStream m, Chunk m ~ Bytes)
    => Bin.Cfg -> [Patch 'FwdSeek Bin.Meta a]
    -> m (Maybe (ErrorBin a))
patchBytes cfg = go
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
patchPure :: Bin.Cfg -> [Patch 'FwdSeek Bin.Meta Bytes] -> BS.ByteString -> Either (ErrorBin Bytes) BL.ByteString
patchPure cfg ps bs =
    let (mErr, (bsRemaining, bbPatched)) = runState (patchBytes cfg ps) (bs, mempty)
        bbPatched' = bbPatched <> BB.byteString bsRemaining
     in case mErr of
          Just err -> Left err
          Nothing  -> Right $ BB.toLazyByteString bbPatched'

{-
The following would be nice:

    patchDirect
        :: (MonadFwdStream m, Chunk m ~ a) -- also need stuff like Eq a...
        => Cfg -> [Patch 'FwdSeek a]
        -> m (Maybe (Error a))

But it would require a little more interface rethinking, because Cfg and patch
meta store binary-specialized stuff.
-}

{-
patchBytes'
    :: (MonadFwdStream m, Chunk m ~ a, BinRep a, ) -- TODO how get length from a
    => Bin.Cfg -> [Patch 'FwdSeek a]
    -> m (Maybe _)
patchBytes' cfg = go
  where
    go [] = return Nothing
    go (Patch n (Edit ed meta):es) = do
        advance n
        bsStream <- readahead $ fromIntegral -- TODO catch overlong error
        Bin.check cfg ed 
        case toBinRep ed of
          Left errBinRep ->
            return $ Just $ ErrorPatchDataFailedToConvertToBytes ed errBinRep
          Right bs -> do

            -- if provided, strip trailing nulls from to-overwrite bytestring
            case tryStripNulls bsStream (emNullTerminates meta) of
              Nothing -> return $ Just ErrorPatchUnexpectedNonnull
              Just bsStream' -> do

                -- if provided, check the to-overwrite bytestring matches expected
                case emExpected meta of
                  Nothing -> overwrite bs >> go es
                  Just expected ->
                    case toBinRep expected of
                      Left errBinRep ->
                        return $ Just $ ErrorPatchDataFailedToConvertToBytes expected errBinRep
                      Right expectedBs ->
                        case checkExpected bsStream' expectedBs of
                          Just (bsa, bse) ->
                            return $ Just $ ErrorPatchDidNotMatchExpected bsa bse
                          Nothing -> overwrite bs >> go es

    tryStripNulls bs = \case
      Nothing        -> Just bs
      Just nullsFrom ->
        let (bs', bsNulls) = BS.splitAt nullsFrom bs
         in if   bsNulls == BS.replicate (BS.length bsNulls) 0x00
            then Just bs'
            else Nothing

    checkExpected bs bsExpected =
        case cfgAllowPartialExpected cfg of
          True  -> if   BS.isPrefixOf bs bsExpected
                   then Nothing
                   else Just (bs, bsExpected)
          False -> if   bs == bsExpected
                   then Nothing
                   else Just (bs, bsExpected)
-}
