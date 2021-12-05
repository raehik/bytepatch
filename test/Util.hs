{-# OPTIONS_GHC -fno-warn-orphans #-}

module Util where

import           Text.Megaparsec
import           Data.Void
import           GHC.Natural
import           Test.QuickCheck

--parseFromCharStream :: (MonadParsec e s m, Token s ~ Char) => m a -> s -> Maybe a
parseFromCharStream :: (Stream s, Token s ~ Char) => Parsec Void s a -> s -> Maybe a
parseFromCharStream parser text = parseMaybe parser text

-- TODO this is in quickcheck-instances
instance Arbitrary Natural where
  arbitrary = arbitrarySizedNatural
  shrink    = shrinkIntegral
