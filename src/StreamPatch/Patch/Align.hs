module StreamPatch.Patch.Align where

import StreamPatch.Patch
import StreamPatch.HFunctorList ( hflStrip )

import GHC.Generics ( Generic )
import Data.Vinyl
import Data.Vinyl.TypeLevel
import Data.Functor.Const

data Meta st = Meta
  { mExpected :: Maybe st
  -- ^ Absolute stream offset for edit. Used for checking against actual offset.
  } deriving (Generic, Show, Eq)

data Error st
  = ErrorAlignedToNegative Integer -- guaranteed negative
  | ErrorDoesntMatchExpected st st
    deriving (Generic, Show, Eq)

-- | Attempt to align the given patch to 0 using the given base.
--
-- The resulting seek is guaranteed to be non-negative, so you may use
-- natural-like types safely.
--
-- TODO Complicated.
align
    :: forall sf st a ss is r rs
    .  ( Integral sf, Num st, Eq st
       , r ~ Const (Meta st)
       , rs ~ RDelete r ss
       , RElem r ss (RIndex r ss)
       , RSubset rs ss is )
    => Integer
    -> Patch sf ss a
    -> Either (Error st) (Patch st rs a)
align sBase (Patch a s ms)
  | s' < 0 = Left $ ErrorAlignedToNegative s'
  | otherwise =
      case mExpected m of
        Nothing        -> Right $ Patch a s'' ms'
        Just sExpected ->
          if   sExpected == s''
          then Right $ Patch a s'' ms'
          else Left $ ErrorDoesntMatchExpected sExpected s''
  where
    s' = sBase + toInteger s
    s'' = fromInteger s'
    (m, ms') = hflStrip getConst ms
