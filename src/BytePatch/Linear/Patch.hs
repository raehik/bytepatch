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
    MonadFwdByteStream(..)
  , Cfg(..)
  , Error(..)

  -- * Prepared patchers
  , patchPure

  -- * General patcher
  , patch

  ) where

import           BytePatch.Core

import           GHC.Natural
import qualified Data.ByteString         as BS
import qualified Data.ByteString.Lazy    as BL
import qualified Data.ByteString.Builder as BB
import           Control.Monad.State
import           Control.Monad.Reader
import           System.IO               ( Handle, SeekMode(..), hSeek )
import           Optics

type Bytes = BS.ByteString

-- TODO also require reporting cursor position (for error reporting)
class Monad m => MonadFwdByteStream m where
    -- | Read a number of bytes without advancing the cursor.
    readahead :: Natural -> m Bytes

    -- | Advance cursor without reading.
    advance :: Natural -> m ()

    -- | Insert bytes into the stream at the cursor position, overwriting
    --   existing bytes.
    overwrite :: Bytes -> m ()

instance Monad m => MonadFwdByteStream (StateT (Bytes, BB.Builder) m) where
    readahead n = BS.take (fromIntegral n) <$> gets fst
    advance n = do
        (src, out) <- get
        let (bs, src') = BS.splitAt (fromIntegral n) src
        put (src', out <> BB.byteString bs)
    overwrite bs = do
        (src, out) <- get
        let (_, src') = BS.splitAt (BS.length bs) src
        put (src', out <> BB.byteString bs)

instance MonadIO m => MonadFwdByteStream (ReaderT Handle m) where
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

-- | Patch time config.
data Cfg = Cfg
  { cfgWarnIfLikelyReprocessing :: Bool
  -- ^ If we determine that we're repatching an already-patched stream, continue
  --   with a warning instead of failing.

  , cfgAllowPartialExpected :: Bool
  -- ^ If enabled, allow partial expected bytes checking. If disabled, then even
  --   if the expected bytes are a prefix of the actual, fail.
  } deriving (Eq, Show)

-- | Errors encountered during patch time.
data Error
  = ErrorPatchOverlong
  | ErrorPatchUnexpectedNonnull
  | ErrorPatchDidNotMatchExpected Bytes Bytes
    deriving (Eq, Show)

patch
    :: MonadFwdByteStream m
    => Cfg -> [Patch 'FwdSeek Bytes]
    -> m (Maybe Error)
patch cfg = go
  where
    go [] = return Nothing
    go (Patch n (Edit bs meta):es) = do
        advance n
        bsStream <- readahead $ fromIntegral $ BS.length bs -- TODO catch overlong error

        -- if provided, strip trailing nulls from to-overwrite bytestring
        case tryStripNulls bsStream (emNullTerminates meta) of
          Nothing -> return $ Just ErrorPatchUnexpectedNonnull
          Just bsStream' -> do

            -- if provided, check the to-overwrite bytestring matches expected
            case checkExpected bsStream' (emExpected meta) of
              Just (bsa, bse) -> return $ Just $ ErrorPatchDidNotMatchExpected bsa bse
              Nothing -> overwrite bs >> go es

    tryStripNulls bs = \case
      Nothing        -> Just bs
      Just nullsFrom ->
        let (bs', bsNulls) = BS.splitAt nullsFrom bs
         in if   bsNulls == BS.replicate (BS.length bsNulls) 0x00
            then Just bs'
            else Nothing

    checkExpected bs = \case
      Nothing -> Nothing
      Just bsExpected ->
        case cfgAllowPartialExpected cfg of
          True  -> if   BS.isPrefixOf bs bsExpected
                   then Nothing
                   else Just (bs, bsExpected)
          False -> if   bs == bsExpected
                   then Nothing
                   else Just (bs, bsExpected)

-- | Attempt to apply a patchscript to a 'Data.ByteString.ByteString'.
patchPure :: Cfg -> [Patch 'FwdSeek Bytes] -> BS.ByteString -> Either Error BL.ByteString
patchPure cfg ps bs =
    let (mErr, (bsRemaining, bbPatched)) = runState (patch cfg ps) (bs, mempty)
        bbPatched' = bbPatched <> BB.byteString bsRemaining
     in case mErr of
          Just err -> Left err
          Nothing  -> Right $ BB.toLazyByteString bbPatched'
