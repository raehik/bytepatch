{-# LANGUAGE OverloadedStrings #-} -- TODO TMP

module BytePatch.Patch
  (
  -- * Core patch algorithm
    apply
  , MonadFwdByteStream
  , Cfg(..)
  , Error(..)

  -- * Patchscript generation
  , gen
  , Patch(..)
  , Patchscript
  , Overwrite(..)
  , OverwriteMeta(..)
  , ErrorGen(..)
  ) where

import qualified Data.ByteString         as BS
import qualified Data.ByteString.Builder as BB
import           Control.Monad.State
import           Control.Monad.Reader
import           System.IO               ( Handle, SeekMode(..), hSeek )
import           Data.List               ( sortBy )

type Bytes = BS.ByteString

-- TODO also require reporting cursor position (for error reporting)
class Monad m => MonadFwdByteStream m where
    -- | Read a number of bytes without advancing the cursor.
    readahead :: Int -> m Bytes

    -- | Advance cursor without reading.
    advance :: Int -> m ()

    -- | Insert bytes into the stream at the cursor position, overwriting
    --   existing bytes.
    overwrite :: Bytes -> m ()

instance Monad m => MonadFwdByteStream (StateT (Bytes, BB.Builder) m) where
    readahead n = do
        (src, out) <- get
        let (bs, src') = BS.splitAt n src
        put (src', out)
        return bs
    advance n = do
        (src, out) <- get
        let (bs, src') = BS.splitAt n src
        put (src', out <> BB.byteString bs)
    overwrite bs = do
        (src, out) <- get
        let (_, src') = BS.splitAt (BS.length bs) src
        put (src', out <> BB.byteString bs)

instance MonadIO m => MonadFwdByteStream (ReaderT Handle m) where
    readahead n = do
        hdl <- ask
        bs <- liftIO $ BS.hGet hdl n
        liftIO $ hSeek hdl RelativeSeek (- fromIntegral n)
        return bs
    advance n = do
        hdl <- ask
        liftIO $ hSeek hdl RelativeSeek (fromIntegral n)
    overwrite bs = do
        hdl <- ask
        liftIO $ BS.hPut hdl bs

-- | A list of "skip x bytes, overwrite with bytestring y" actions.
type Patchscript a = [(Int, Overwrite a)]

-- | A single replacement (in-place edit).
--
-- Replacements may store extra metadata that can be used at patch time to
-- validate input data (i.e. patching correct file).
data Overwrite a = Overwrite a (OverwriteMeta a)
    deriving (Eq, Show)

-- | Optional patch time data for an overwrite.
data OverwriteMeta a = OverwriteMeta
  { omNullTerminates :: Maybe Int
  -- ^ Stream segment should be null bytes (0x00) only from this index onwards.

  , omExpected       :: Maybe a
  -- ^ Stream segment should be this.
  } deriving (Eq, Show, Functor)

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

apply :: MonadFwdByteStream m => Cfg -> Patchscript Bytes -> m (Maybe Error)
apply cfg = go
  where
    go [] = return Nothing
    go ((n, Overwrite bs meta):es) = do
        advance n
        bsStream <- readahead $ BS.length bs -- TODO catch overlong error

        -- if provided, strip trailing nulls from to-overwrite bytestring
        case tryStripNulls bsStream (omNullTerminates meta) of
          Nothing -> return $ Just ErrorPatchUnexpectedNonnull
          Just bsStream' -> do

            -- if provided, check the to-overwrite bytestring matches expected
            case checkExpected bsStream' (omExpected meta) of
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

-- | Write the given data into the given offset.
data Patch a = Patch a Int (OverwriteMeta a)
    deriving (Eq, Show)

-- | Error encountered during patchscript generation.
data ErrorGen a
  = ErrorGenOverlap (Patch a) (Patch a)
  -- ^ Two patches wrote to the same offset.
  --
  -- TODO: we could allow this e.g. by selecting one overwrite that "wins"
  -- (likely via user annotation) and rewriting the other one to remove the
  -- collision.
    deriving (Eq, Show)

gen :: [Patch Bytes] -> (Patchscript Bytes, [ErrorGen Bytes])
gen pList =
    let pList'                 = sortBy comparePatchOffsets pList
        (_, script, errors, _) = execState (go pList') (0, [], [], undefined)
        -- I believe the undefined is inaccessible providing the first patch has
        -- a non-negative offset (negative offsets are forbidden)
     in (reverse script, reverse errors)
  where
    comparePatchOffsets (Patch _ o1 _) (Patch _ o2 _) = compare o1 o2
    go :: (MonadState (Int, Patchscript Bytes, [ErrorGen Bytes], Patch Bytes) m) => [Patch Bytes] -> m ()
    go [] = return ()
    go (p@(Patch bs offset meta):ps) = do
        (cursor, script, errors, prevPatch) <- get
        case trySkipTo offset cursor of
          -- next offset is behind current cursor: overlapping patches
          -- record error, recover via dropping patch
          Left _ -> do
            let e = ErrorGenOverlap p prevPatch
            let errors' = e : errors
            put (cursor, script, errors', p)
            go ps
          Right skip -> do
            let cursor' = cursor + skip + BS.length bs
                o       = Overwrite bs meta
            put (cursor', (skip, o):script, errors, p)
            go ps
    trySkipTo to from =
        let diff = to - from in if diff >= 0 then Right diff else Left (-diff)

{-
finishPurePatch :: (BS.ByteString, BB.Builder) -> BL.ByteString
finishPurePatch (src, out) = BB.toLazyByteString $ out <> BB.byteString src

tmpExPatchscript :: Patchscript
tmpExPatchscript =
  [ (1, "ABC")
  , (2, "DEFG") ]

tmpExInitialState :: (BS.ByteString, BB.Builder)
tmpExInitialState = ("abcdefghijklmnopqrstuvwxyz", mempty)
-}

