module BytePatch.AlignSpec ( spec ) where

import           BytePatch.Align
import           Test.Hspec
import           Util

import           BytePatch.Core
import           Data.Functor.Const
import           GHC.Natural

genAlignedPatch :: SeekRep s ~ Natural => [(Integer, a)] -> [Patch 'CursorSeek (Meta s (Const ())) a]
genAlignedPatch = map go
  where go (n, a) = Patch a $ Pos n $ Meta Nothing $ Const ()

genNatPatch :: SeekRep s ~ Natural => [(Natural, a)] -> [Patch s (Const ()) a]
genNatPatch = map go
  where go (n, a) = Patch a $ Pos n $ Const ()

-- note that we have to be concrete about our seek representation somewhere! so
-- we do it here
align' :: Integer -> [(Integer, a)] -> Either (Error 'FwdSeek) [Patch 'FwdSeek (Const ()) a]
align' n = traverse align . map (Aligned n) . genAlignedPatch

spec :: Spec
spec = do
    it "aligns valid simple patches" $ do
      align' @String 0 [] `shouldBe` Right (genNatPatch [])
      align' 0 [(0, "XXX")] `shouldBe` Right (genNatPatch [(0, "XXX")])
      align' 5 [(0, "XXX"), ((-5), "XXX")] `shouldBe` Right (genNatPatch [(5, "XXX"), (0, "XXX")])
