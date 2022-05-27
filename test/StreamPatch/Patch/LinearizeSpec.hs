module StreamPatch.Patch.LinearizeSpec ( spec ) where

import StreamPatch.Patch.Linearize.InPlace
import StreamPatch.Patch.Linearize.Insert
import Test.Hspec
import Util

import StreamPatch.Patch
import Data.Functor.Const
import Data.Either.Combinators
import GHC.Natural
import Optics
import Data.Generics.Product.Any

spec :: Spec
spec = do
    let p  = makePatch
    it "linearizes a simple valid in-place patch" $ do
      linearizeInPlace   [p "1" 1, p "5" 5, p "2" 2]
        `shouldBe` Right [p "1" 1, p "2" 0, p "5" 2]
    it "TODO" $ do
      linearize'  [1,2,3,4,5] `shouldBe` Right (Just ( 1, [1,1,1,1]))
      linearize' [-1,2,3,4,5] `shouldBe` Right (Just (-1, [3,1,1,1]))
