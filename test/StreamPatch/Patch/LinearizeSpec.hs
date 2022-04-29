module StreamPatch.Patch.LinearizeSpec ( spec ) where

import StreamPatch.Patch.Linearize
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
    let p  = makePatch' @'AbsSeek
        p' = makePatch' @'FwdSeek
    it "linearizes a simple valid patch" $ do
      linearize [p "1" 1, p "5" 5, p "2" 2] `shouldBe` Right [p' "1" 1, p' "2" 0, p' "5" 2]
