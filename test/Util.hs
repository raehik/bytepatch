{-# OPTIONS_GHC -fno-warn-orphans #-}

module Util where

import           StreamPatch.Patch
import           Data.Vinyl
import           Text.Megaparsec
import           Data.Void
import           GHC.Natural
import           Test.QuickCheck

--parseFromCharStream :: (MonadParsec e s m, Token s ~ Char) => m a -> s -> Maybe a
parseFromCharStream :: (Stream s, Token s ~ Char) => Parsec Void s a -> s -> Maybe a
parseFromCharStream parser text = parseMaybe parser text

-- TODO this is in quickcheck-instances
instance Arbitrary Natural where
  arbitrary = arbitrarySizedNatural
  shrink    = shrinkIntegral

makePatch :: a -> Natural -> Patch 'FwdSeek '[] a
makePatch a n = Patch a n $ FunctorRec RNil

makePatch' :: forall s a. a -> SeekRep s -> Patch s '[] a
makePatch' a n = Patch a n $ FunctorRec RNil

makePatchscript :: [(Natural, a)] -> [Patch 'FwdSeek '[] a]
makePatchscript = map go
  where go (n, a) = makePatch a n
