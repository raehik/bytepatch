module StreamPatch.Apply where

import           StreamPatch.Stream
import           StreamPatch.Patch
import           StreamPatch.HFunctorList
import qualified StreamPatch.Patch.Binary   as Bin
import           StreamPatch.Patch.Binary   ( BinRep )

import           Data.Vinyl
import qualified Data.ByteString            as BS
import qualified Data.ByteString.Builder    as BB
import qualified Data.ByteString.Lazy       as BL
import           Control.Monad.State
import           StreamPatch.Util           ( traverseM_ )

-- TODO how to clean up, use Either monad inside m? (lift didn't work)
applyBinFwd
    :: forall a m. (MonadFwdInplaceStream m, Chunk m ~ BS.ByteString, BinRep a)
    => Bin.Cfg
    -> [Patch 'FwdSeek '[Bin.MetaStream] a]
    -> m (Either (Bin.Error a) ())
applyBinFwd cfg = traverseM_ $ \(Patch a s (HFunctorList (Flap m :& RNil))) -> do
    case Bin.toBinRep' a of
      Left err -> return $ Left err
      Right bs -> do
        advance s
        bsStream <- readahead $ fromIntegral $ BS.length bs
        case Bin.check cfg bsStream m of
          Left err -> return $ Left err
          Right () -> do
            overwrite bs
            return $ Right ()

runPureFwdBin
    :: BinRep a
    => Bin.Cfg
    -> [Patch 'FwdSeek '[Bin.MetaStream] a]
    -> BS.ByteString
    -> Either (Bin.Error a) BL.ByteString
runPureFwdBin cfg ps bs =
    let (mErr, (bsRemaining, bbPatched)) = runState (applyBinFwd cfg ps) (bs, mempty)
        bbPatched' = bbPatched <> BB.byteString bsRemaining
     in case mErr of
          Left err -> Left err
          Right () -> Right $ BB.toLazyByteString bbPatched'

applySimpleFwd
    :: (MonadFwdInplaceStream m, Chunk m ~ a)
    => [Patch 'FwdSeek '[] a]
    -> m ()
applySimpleFwd =
    mapM_ $ \(Patch a s (HFunctorList RNil)) -> advance s >> overwrite a

-- stupid because no monotraversable :<
runPureSimpleFwdList
    :: [Patch 'FwdSeek '[] [a]]
    -> [a]
    -> [a]
runPureSimpleFwdList ps start =
    let ((), (remaining, patched)) = runState (applySimpleFwd ps) (start, mempty)
     in patched <> remaining
