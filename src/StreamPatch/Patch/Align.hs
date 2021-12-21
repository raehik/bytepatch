module StreamPatch.Patch.Align where

import           StreamPatch.Patch

import           GHC.Generics ( Generic )
import           Numeric.Natural
import           GHC.Natural ( naturalFromInteger )
import           Data.Vinyl
import           Data.Vinyl.TypeLevel
import           Data.Functor.Const

data Meta s = Meta
  { mExpected :: Maybe (SeekRep s)
  -- ^ Absolute stream offset for edit. Used for checking against actual offset.
  } deriving (Generic)

deriving instance (Eq   (SeekRep s)) => Eq   (Meta s)
deriving instance (Show (SeekRep s)) => Show (Meta s)

data Error s
  = ErrorSeekBelow0 (SeekRep 'RelSeek)
  | ErrorDoesntMatchExpected (SeekRep s) (SeekRep s) -- expected, then actual
    deriving (Generic)

deriving instance (Eq   (SeekRep s)) => Eq   (Error s)
deriving instance (Show (SeekRep s)) => Show (Error s)

-- | Attempt to align the given patch to 0 using the given base.
align
    :: forall s a ss rs is i r
    .  ( SeekRep s ~ Natural
       , r ~ Const (Meta s)
       , rs ~ RDelete r ss
       , RElem r ss i
       , RSubset rs ss is )
    => SeekRep 'RelSeek
    -> Patch 'RelSeek ss a
    -> Either (Error s) (Patch s rs a)
align sBase (Patch a s ms) =
    case tryIntegerToNatural sAligned of
      Nothing          -> Left $ ErrorSeekBelow0 sAligned
      Just sAlignedNat ->
        case mExpected m of
          Nothing        -> reform sAlignedNat
          Just sExpected ->
            if   sExpected == sAlignedNat
            then reform sAlignedNat
            else Left $ ErrorDoesntMatchExpected sExpected sAlignedNat
  where
    sAligned = sBase + s
    -- TODO require a visible type application here, unsure why. even without
    -- the SeekRep indirection
    m = getConst @(Meta s) $ getFlap $ rget $ getFunctorRec ms
    ms' = FunctorRec $ rcast @rs $ getFunctorRec ms
    reform s' = Right $ Patch a s' ms'

tryIntegerToNatural :: Integer -> Maybe Natural
tryIntegerToNatural n | n < 0     = Nothing
                      | otherwise = Just $ naturalFromInteger n