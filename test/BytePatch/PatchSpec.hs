module BytePatch.PatchSpec ( spec ) where

import           BytePatch.Patch
import           Test.Hspec
import           Util

import           BytePatch.Core
import           Data.Functor.Const
import           GHC.Natural

import qualified Data.ByteString                as BS

genPatch :: [(Natural, a)] -> [Patch 'FwdSeek (Const ()) a]
genPatch = map go
  where go (n, a) = Patch a $ Pos n $ Const ()

spec :: Spec
spec = do
    let patch ps = patchListPure $ genPatch ps
    it "applies valid simple forward in-place patches" $ do
      patch []           "1234567890" `shouldBe` "1234567890"
      patch [(0, "XXX")] "1234567890" `shouldBe` "XXX4567890"
      patch [(1, "XXX")] "1234567890" `shouldBe` "1XXX567890"
      patch [(7, "XXX")] "1234567890" `shouldBe` "1234567XXX"
