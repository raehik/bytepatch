{-| Relative-seeking patches that can be re-aligned to a new 0.

Essentially a layer on top of a patch that can be stripped off with 'align' (or
'alignList').
-}

module BytePatch.Patch.Align where

import           BytePatch.Patch

import           GHC.Natural
import           GHC.Generics               ( Generic )
import           Optics
import           Data.Generics.Product.Any

data MetaPos s pd a = Meta
  { mpExpected :: Maybe (SeekRep s)
  -- ^ Absolute stream offset for edit. Used for checking against actual offset.

  , mpInner    :: pd a
  } deriving (Generic, Functor, Foldable, Traversable)

deriving instance (Eq   (SeekRep s), Eq   (pd a), Eq   a) => Eq   (MetaPos s pd a)
deriving instance (Show (SeekRep s), Show (pd a), Show a) => Show (MetaPos s pd a)

data Error s
  = ErrorSeekBelow0 (SeekRep 'RelSeek)
  | ErrorDoesntMatchExpected (SeekRep s) (SeekRep s) -- expected, then actual
    deriving (Generic)

deriving instance (Eq   (SeekRep s)) => Eq   (Error s)
deriving instance (Show (SeekRep s)) => Show (Error s)

-- | Attempt to align the given patch to 0 using the given base.
align
    :: SeekRep s ~ Natural
    => SeekRep 'RelSeek
    -> Patch 'RelSeek dd (MetaPos s pd) a
    -> Either (Error s) (Patch s dd pd a)
align a = traverseOf (the @"patchPos") (alignPos a)

-- | Attempt to "align to 0" a relative 'Pos' to any seek represented by a
--   'Natural'.
alignPos
    :: (SeekRep s ~ Natural)
    => SeekRep 'RelSeek
    -> Pos 'RelSeek (MetaPos s pd) a
    -> Either (Error s) (Pos s pd a)
alignPos sBase (Pos s meta) =
    case tryIntegerToNatural sAligned of
      Nothing          -> Left $ ErrorSeekBelow0 sAligned
      Just sAlignedNat ->
        case mpExpected meta of
          Nothing -> reform sAlignedNat
          Just sExpected ->
            if   sExpected == sAlignedNat
            then reform sAlignedNat
            else Left $ ErrorDoesntMatchExpected sExpected sAlignedNat
  where
    sAligned = sBase + s
    reform s' = Right $ Pos s' $ mpInner meta

tryIntegerToNatural :: Integer -> Maybe Natural
tryIntegerToNatural n | n < 0     = Nothing
                      | otherwise = Just $ naturalFromInteger n
