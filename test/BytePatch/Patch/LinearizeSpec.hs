module BytePatch.Patch.LinearizeSpec ( spec ) where

import           BytePatch.Patch.Linearize
import qualified BytePatch.Patch.Linearize  as Linearize
import           Test.Hspec
import           Util

import           BytePatch.Patch
import           Data.Functor.Const
import           Data.Either.Combinators
import           GHC.Natural
import           Optics
import           Data.Generics.Product.Any

spec :: Spec
spec = do
    let p = \d s -> Patch d $ Pos s $ Const ()
    it "linearizes a simple valid patch" $ do
      linearize [p "1" 1, p "5" 5, p "2" 2] `shouldBe` Right [p "1" 1, p "2" 0, p "5" 2]
