module StreamPatch.Patch.AlignSpec ( spec ) where

import StreamPatch.Patch.Align
import Test.Hspec
import Util

import StreamPatch.Patch
import StreamPatch.HFunctorList
import Data.Vinyl
import Data.Functor.Const
import GHC.Natural

makeUnalignedPatch
    :: [(Integer, a)]
    -> [Patch 'RelSeek '[Const (Meta 'FwdSeek)] a]
makeUnalignedPatch = map go
  where go (n, a) = Patch a n $ HFunctorList $ Flap (Const (Meta Nothing)) :& RNil

align'
    :: Integer -> [(Integer, String)]
    -> Either (Error 'FwdSeek) [Patch 'FwdSeek '[] String]
align' sBase = traverse (align sBase) . makeUnalignedPatch

spec :: Spec
spec = do
    it "aligns valid simple patches" $ do
      align' 0 [] `shouldBe` Right (makePatchscript [])
      align' 0 [(0, "XXX")] `shouldBe` Right (makePatchscript [(0, "XXX")])
      align' 5 [(0, "XXX"), ((-5), "XXX")] `shouldBe` Right (makePatchscript [(5, "XXX"), (0, "XXX")])
