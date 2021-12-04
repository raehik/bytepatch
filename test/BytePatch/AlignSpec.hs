module BytePatch.AlignSpec ( spec ) where

import           BytePatch.Align
import           Test.Hspec
import           Util

import           BytePatch.Core
import           Data.Functor.Const
import           GHC.Natural

genAlignedPatch :: [(Integer, a)] -> [Patch 'CursorSeek (Meta (Const ())) a]
genAlignedPatch = map go
  where go (n, a) = Patch a $ Pos n $ Meta Nothing $ Const ()

genAbsPatch :: [(Natural, a)] -> [Patch 'AbsSeek (Const ()) a]
genAbsPatch = map go
  where go (n, a) = Patch a $ Pos n $ Const ()

align' :: Integer -> [(Integer, a)] -> Either Error [Patch 'AbsSeek (Const ()) a]
align' n = traverse align . map (Aligned n) . genAlignedPatch

spec :: Spec
spec = do
    it "aligns valid simple patches" $ do
      align' @String 0 [] `shouldBe` Right (genAbsPatch [])
      align' 0 [(0, "XXX")] `shouldBe` Right (genAbsPatch [(0, "XXX")])
      align' 5 [(0, "XXX"), ((-5), "XXX")] `shouldBe` Right (genAbsPatch [(5, "XXX"), (0, "XXX")])
