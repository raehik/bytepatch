-- | Various combined patch tests (using multiple layers).

module BytePatch.Patch.CombinedSpec ( spec ) where

import           BytePatch.Patch.Linearize
import           BytePatch.Patch.Binary
import           Test.Hspec
import           Util

import           BytePatch.Patch
import           Data.Functor.Const
import           GHC.Natural

{-
TODO. Aim is to present how linearization and binary conversion can be done in
either order, but that they mean different things.
-}

spec :: Spec
spec = do
    it "does sth" $
      pending
