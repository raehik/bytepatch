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
    :: forall v m
    .  ( FwdInplaceStream m, Chunk m ~ BS.ByteString
       , Compare v BS.ByteString, Num (Index m) )
    => [Patch (Index m) '[Compare.Meta v, Bin.Meta] BS.ByteString]
    -> m (Either Error ())
applyBinCompareFwd = traverseM_ $ \(Patch bs s (HFunctorList (Flap cm :& Flap bm :& RNil))) -> runExceptT $ do
    -- advance to patch location
    lift $ advance s

    -- read same number of bytes as patch data
    bsStream  <- lift $ readahead $ fromIntegral $ getLength bs

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
    => [Patch Int '[Compare.Meta v, Bin.Meta] BS.ByteString]
    -> BS.ByteString
    -> Either Error BL.ByteString
runPureBinCompareFwd ps bs =
    let initState = (bs, mempty :: BB.Builder, 0 :: Int)
        (mErr, (bsRemaining, bbPatched, _)) = runState (applyBinCompareFwd ps) initState
        bbPatched' = bbPatched <> BB.byteString bsRemaining
     in case mErr of
          Left  e  -> Left e
          Right () -> Right $ BB.toLazyByteString bbPatched'

applyFwd
    :: (FwdInplaceStream m, Chunk m ~ a)
    => [Patch (Index m) '[] a]
    -> m ()
applyFwd =
    mapM_ $ \(Patch a s (HFunctorList RNil)) ->
        advance s >> overwrite a

runPureFwdList
    :: [Patch Int '[] [a]]
    -> [a]
    -> [a]
runPureFwdList ps start =
    let ((), (remaining, patched, _)) = runState (applyFwd ps) (start, mempty, 0 :: Int)
     in patched <> remaining

applyFwdCompare
    :: forall a v m
    .  ( FwdInplaceStream m, Chunk m ~ a
       , Compare v a, HasLength a, Num (Index m) )
    => [Patch (Index m) '[Compare.Meta v] a]
    -> m (Either Error ())
applyFwdCompare = traverseM_ $ \(Patch a s (HFunctorList (Flap cm :& RNil))) -> do
    advance s
    aStream <- readahead $ fromIntegral $ getLength a
    case Compare.mCompare cm of
      Nothing   -> do
        x <- overwrite a
        return $ Right x
      Just aCmp -> case compareTo @v aCmp aStream of
                     Nothing -> return $ Right ()
                     Just e  -> return $ Left $ ErrorCompare e

runPureFwdCompareString
    :: Compare v String
    => [Patch Int '[Compare.Meta v] String]
    -> String
    -> Either Error String
runPureFwdCompareString ps start =
    let (r, (remaining, patched, _)) = runState (applyFwdCompare ps) (start, "", 0 :: Int)
    in  case r of
          Left err -> Left err
          Right () -> Right $ patched <> remaining
