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

makePatch :: a -> s -> Patch s '[] a
makePatch a s = Patch a s $ HFunctorList RNil

makePatchscript :: [(s, a)] -> [Patch s '[] a]
makePatchscript = map go
  where go (n, a) = makePatch a n
