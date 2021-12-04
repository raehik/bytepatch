module BytePatch.Align where

import           BytePatch.Core

import           GHC.Generics               ( Generic )
import           GHC.Natural
import           Optics
import           Data.Generics.Product.Any

class Alignable t p d a where
    align
        :: t (p 'CursorSeek (Meta d) a)
        -> Either Error (p 'AbsSeek d a)

-- | Data relative to a given point. Aligned as in "with information on how to
--   align".
data Aligned a = Aligned
  { alignedData  :: a
  , alignedAlign :: SeekRep 'CursorSeek
  } deriving (Eq, Show, Generic, Functor, Foldable, Traversable)

data Meta d a = Meta
  { mExpected :: Maybe (SeekRep 'AbsSeek)
  -- ^ Absolute stream offset for edit. Used for checking against actual offset.

  , mInner     :: d a
  } deriving (Eq, Show, Generic, Functor, Foldable, Traversable)

data Error
  = ErrorSeekBelow0 (SeekRep 'CursorSeek)
  | ErrorDoesntMatchExpected (SeekRep 'AbsSeek) (SeekRep 'AbsSeek) -- expected, then actual
    deriving (Eq, Show, Generic)

instance Alignable Aligned Patch d a where
    align p = traverseOf (the @"patchPos") (alignPos (alignedAlign p)) (alignedData p)

instance Alignable Aligned MultiPatch d a where
    align p = traverseOf (the @"multiPatchPos") (traverse (alignPos (alignedAlign p))) (alignedData p)

alignPos
    :: SeekRep 'CursorSeek
    -> Pos 'CursorSeek (Meta d a)
    -> Either Error (Pos 'AbsSeek (d a))
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
