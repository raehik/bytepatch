{-# OPTIONS_GHC -fno-warn-orphans #-}

module Util where

import StreamPatch.Patch
import StreamPatch.HFunctorList
import Data.Vinyl
import GHC.Natural
import Test.QuickCheck

-- TODO this is in quickcheck-instances
instance Arbitrary Natural where
  arbitrary = arbitrarySizedNatural
  shrink    = shrinkIntegral

makePatch :: a -> Natural -> Patch 'FwdSeek '[] a
makePatch a n = Patch a n $ HFunctorList RNil

makePatch' :: forall s a. a -> SeekRep s -> Patch s '[] a
makePatch' a n = Patch a n $ HFunctorList RNil

makePatchscript :: [(Natural, a)] -> [Patch 'FwdSeek '[] a]
makePatchscript = map go
  where go (n, a) = makePatch a n
