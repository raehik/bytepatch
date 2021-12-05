module BytePatch.Align where

import           BytePatch.Patch

import           GHC.Generics               ( Generic )
import           GHC.Natural
import           Optics
import           Data.Generics.Product.Any

class Alignable t p d a where
    align
        :: SeekRep s ~ Natural
        => t (p 'CursorSeek (Meta s d) a)
        -> Either (Error s) (p s d a)

-- | Data relative to a given point. Aligned as in "with information on how to
--   align".
data Aligned a = Aligned
  { alignedAlign :: SeekRep 'CursorSeek
  , alignedData  :: a
  } deriving (Eq, Show, Generic, Functor, Foldable, Traversable)

data Meta s d a = Meta
  { mExpected :: Maybe (SeekRep s)
  -- ^ Absolute stream offset for edit. Used for checking against actual offset.

  , mInner     :: d a
  } deriving (Generic, Functor, Foldable, Traversable)

deriving instance (Eq (SeekRep s), Eq (d a), Eq a) => Eq (Meta s d a)
deriving instance (Show (SeekRep s), Show (d a), Show a) => Show (Meta s d a)

data Error s
  = ErrorSeekBelow0 (SeekRep 'CursorSeek)
  | ErrorDoesntMatchExpected (SeekRep s) (SeekRep s) -- expected, then actual
    deriving (Generic)

deriving instance (Eq (SeekRep s)) => Eq (Error s)
deriving instance (Show (SeekRep s)) => Show (Error s)

instance Alignable Aligned Patch d a where
    align p = traverseOf (the @"patchPos") (alignPos (alignedAlign p)) (alignedData p)

instance Alignable Aligned MultiPatch d a where
    align p = traverseOf (the @"multiPatchPos") (traverse (alignPos (alignedAlign p))) (alignedData p)

-- The signature here states "given a cursor seek and a cursor position, I can
-- attempt to align the position to any Nat-like seek type". Indeed, the
-- aligning process doesn't care what the type it writes into *means*, it just
-- matters that it's stored as a Nat. Previously it used 'FwdSeek internally,
-- and I would've have issues trying to align cursors positions to absolute
-- positions (a valid request). Now, that's perfectly fine!
alignPos
    :: (SeekRep s ~ Natural)
    => SeekRep 'CursorSeek
    -> Pos 'CursorSeek (Meta s d a)
    -> Either (Error s) (Pos s (d a))
alignPos sBase (Pos s meta) =
    case tryIntegerToNatural sAligned of
      Nothing          -> Left $ ErrorSeekBelow0 sAligned
      Just sAlignedNat ->
        case mExpected meta of
          Nothing -> reform sAlignedNat
          Just sExpected ->
            if   sExpected == sAlignedNat
            then reform sAlignedNat
            else Left $ ErrorDoesntMatchExpected sExpected sAlignedNat
  where
    sAligned = sBase + s
    reform s' = Right $ Pos s' $ mInner meta

tryIntegerToNatural :: Integer -> Maybe Natural
tryIntegerToNatural n | n < 0     = Nothing
                      | otherwise = Just $ naturalFromInteger n
