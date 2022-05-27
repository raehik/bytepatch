{-# LANGUAGE AllowAmbiguousTypes #-}

module StreamPatch.Apply where

import GHC.Generics ( Generic )

import StreamPatch.Patch
import StreamPatch.Stream
import StreamPatch.HFunctorList
import StreamPatch.Patch.Binary qualified as Bin
import StreamPatch.Patch.Compare qualified as Compare
import StreamPatch.Patch.Compare ( Compare(..), compareTo )
import StreamPatch.Patch.Linearize.InPlace ( HasLength, getLength )

import Numeric.Natural
import Data.Vinyl
import Data.ByteString qualified as BS
import Data.ByteString.Builder qualified as BB
import Data.ByteString.Lazy qualified as BL
import Control.Monad.State
import StreamPatch.Util ( traverseM_ )

import Control.Monad.Except

data Error
  = ErrorCompare String
  | ErrorBinUnexpectedNonNull BS.ByteString
    deriving (Generic, Eq, Show)

applyBinCompareFwd
    :: forall m v. (MonadFwdInplaceStream m, Compare v BS.ByteString, Chunk m ~ BS.ByteString)
    => [Patch Natural '[Compare.Meta v, Bin.Meta] BS.ByteString]
    -> m (Either Error ())
applyBinCompareFwd = traverseM_ $ \(Patch bs s (HFunctorList (Flap cm :& Flap bm :& RNil))) -> runExceptT $ do
    -- advance to patch location
    lift $ advance s

    -- read same number of bytes as patch data
    bsStream  <- lift $ readahead $ getLength bs

    -- check for & strip expected terminating nulls
    bsStream' <- doNullTermCheck bsStream (Bin.mNullTerminates bm)

    -- compare with expected data
    doCompare bsStream' $ Compare.mCompare cm

    -- if that was all successful, write patch in-place
    lift $ overwrite bs
  where
    err = throwError
    doCompare bs' = \case
      Nothing   -> return ()
      Just cmp -> do
        case compareTo @v cmp bs' of
          Nothing -> return ()
          Just e -> err $ ErrorCompare e
    doNullTermCheck bs' = \case
      Nothing -> return bs'
      Just nt ->
        let (bs'', bsNulls) = BS.splitAt (fromIntegral nt) bs'
         in if   bsNulls == BS.replicate (BS.length bsNulls) 0x00
            then return bs''
            else err $ ErrorBinUnexpectedNonNull bs'

runPureBinCompareFwd
    :: (Compare v BS.ByteString)
    => [Patch Natural '[Compare.Meta v, Bin.Meta] BS.ByteString]
    -> BS.ByteString
    -> Either Error BL.ByteString
runPureBinCompareFwd ps bs =
    let (mErr, (bsRemaining, bbPatched)) = runState (applyBinCompareFwd ps) (bs, mempty)
        bbPatched' = bbPatched <> BB.byteString bsRemaining
     in case mErr of
          Left  e  -> Left e
          Right () -> Right $ BB.toLazyByteString bbPatched'

applyFwd
    :: (MonadFwdInplaceStream m, Chunk m ~ a)
    => [Patch Natural '[] a]
    -> m ()
applyFwd =
    mapM_ $ \(Patch a s (HFunctorList RNil)) -> advance s >> overwrite a

-- stupid because no monotraversable :< so gotta use String
runPureFwdList
    :: [Patch Natural '[] [a]]
    -> [a]
    -> [a]
runPureFwdList ps start =
    let ((), (remaining, patched)) = runState (applyFwd ps) (start, mempty)
     in patched <> remaining

applyFwdCompare
    :: forall v a m
    .  ( MonadFwdInplaceStream m, Chunk m ~ a
       , MonadError Error m
       , Compare v a, HasLength a )
    => [Patch Natural '[Compare.Meta v] a]
    -> m ()
applyFwdCompare = mapM_ $ \(Patch a s (HFunctorList (Flap cm :& RNil))) -> do
    advance s
    aStream <- readahead $ getLength a
    case Compare.mCompare cm of
      Nothing   -> return ()
      Just aCmp -> case compareTo @v aCmp aStream of
                     Nothing -> return ()
                     Just e  -> throwError $ ErrorCompare e
    overwrite a

{-
-- stupid because no monotraversable :<
runPureFwdCompareList
    :: Compare v [a]
    => [Patch 'FwdSeek '[Compare.Meta v] [a]]
    -> [a]
    -> Either Error [a]
runPureFwdCompareList ps start =
    -- also got to runExceptT
    let ((), (remaining, patched)) = runState (applyFwdCompare ps) (start, mempty)
     in patched <> remaining
-}
