module BytePatch.AlignSpec ( spec ) where

import           BytePatch.Align
import           Test.Hspec
import           Util

import           BytePatch.Patch
import           Data.Functor.Const
import           GHC.Natural

genAlignedPatch
    :: SeekRep s ~ Natural
    => Integer -> [(Integer, a)]
    -> Aligned [Patch 'RelSeek (Meta s(Const ())) a]
genAlignedPatch aBase = Aligned aBase . map go
  where go (n, a) = Patch a $ Pos n $ Meta Nothing $ Const ()

genNatPatch :: SeekRep s ~ Natural => [(Natural, a)] -> [Patch s (Const ()) a]
genNatPatch = map go
  where go (n, a) = Patch a $ Pos n $ Const ()

align'
    :: Integer -> [(Integer, String)]
    -> Either (Error 'FwdSeek) [Patch 'FwdSeek (Const ()) String]
align' aBase = alignList . genAlignedPatch aBase

spec :: Spec
spec = do
    it "aligns valid simple patches" $ do
      align' 0 [] `shouldBe` Right (genNatPatch [])
      align' 0 [(0, "XXX")] `shouldBe` Right (genNatPatch [(0, "XXX")])
      align' 5 [(0, "XXX"), ((-5), "XXX")] `shouldBe` Right (genNatPatch [(5, "XXX"), (0, "XXX")])
