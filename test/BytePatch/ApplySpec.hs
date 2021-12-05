{-# OPTIONS_GHC -fno-warn-orphans #-}

module BytePatch.ApplySpec {- ( spec ) -} where

import           BytePatch.Apply
import           Test.Hspec
import           Test.Hspec.QuickCheck
import           Test.QuickCheck
import           Util

import           BytePatch.Patch
import           Data.Functor.Const
import           GHC.Natural

makePatch :: [(Natural, a)] -> [Patch 'FwdSeek (Const ()) a]
makePatch = map go
  where go (n, a) = Patch a $ Pos n $ Const ()

spec :: Spec
spec = do
    let patch' ps = patchListPure $ makePatch ps
    it "applies valid simple forward in-place patches" $ do
      patch' []           "1234567890" `shouldBe` "1234567890"
      patch' [(0, "XXX")] "1234567890" `shouldBe` "XXX4567890"
      patch' [(1, "XXX")] "1234567890" `shouldBe` "1XXX567890"
      patch' [(7, "XXX")] "1234567890" `shouldBe` "1234567XXX"
    prop "applies valid generated simple forward in-place patches" $ do
      \n -> forAll (genPatchList @Char n) $
        \ps -> forAll (genBoundList @Char n n) $
          \a -> pendingWith "cba to reimplement patcher"
                -- patchListPure ps a `shouldBe` manualPatch ps a

-- | "Verified" implementation of a simple patch algorithm.
--
-- nah I cba lol just trust me
--manualPatch :: [Patch 'FwdSeek (Const ()) [a]] -> [a] -> [a]

genPatchList :: Arbitrary a => Natural -> Gen [Patch 'FwdSeek (Const ()) [a]]
genPatchList = skip []
  where
    skip ps n = do
        -- why no Random Natural...
        nSkip <- choose (0, naturalToInteger n)
        gen ps (naturalFromInteger nSkip) $ n - (fromIntegral nSkip)
    gen ps _     0 = return ps
    gen ps nSkip n = do
        a <- genBoundList n 1
        let p = Patch a $ Pos nSkip $ Const ()
            n' = n - (fromIntegral (length a))
        skip (p:ps) n'

genBoundList :: Arbitrary a => Natural -> Natural -> Gen [a]
genBoundList x y =
    arbitrary `suchThat` \l -> fromIntegral (length l) <= x && fromIntegral (length l) >= y

-- TODO this is in quickcheck-instances
instance Arbitrary Natural where
  arbitrary = arbitrarySizedNatural
  shrink    = shrinkIntegral
