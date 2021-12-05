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

-- | Data relative to a given point. Aligned as in "with information on how to
--   align".
data Aligned a = Aligned
  { alignedAlign :: SeekRep 'RelSeek
  , alignedData  :: a
  } deriving (Eq, Show, Generic, Functor, Foldable, Traversable)

-- | Extends a metadata type with optional alignment metadata.
data Meta s d a = Meta
  { mExpected :: Maybe (SeekRep s)
  -- ^ Absolute stream offset for edit. Used for checking against actual offset.

  , mInner     :: d a
  } deriving (Generic, Functor, Foldable, Traversable)

deriving instance (Eq (SeekRep s), Eq (d a), Eq a) => Eq (Meta s d a)
deriving instance (Show (SeekRep s), Show (d a), Show a) => Show (Meta s d a)

data Error s
  = ErrorSeekBelow0 (SeekRep 'RelSeek)
  | ErrorDoesntMatchExpected (SeekRep s) (SeekRep s) -- expected, then actual
    deriving (Generic)

deriving instance (Eq (SeekRep s)) => Eq (Error s)
deriving instance (Show (SeekRep s)) => Show (Error s)

-- | A patch-like that can be aligned, given a relative seek.
class Alignable p d a where
    -- | Attempt to align to 0 using the given base. As such, this works with
    --   any patch-like that stores seeks as 'Natural's.
    --
    -- Note that aligning strips the alignment metadata layer.
    alignTo
        :: SeekRep s ~ Natural
        => SeekRep 'RelSeek
        -> p 'RelSeek (Meta s d) a
        -> Either (Error s) (p s d a)

instance Alignable Patch d a where
    alignTo a = traverseOf (the @"patchPos") (alignPos a)

instance Alignable MultiPatch d a where
    alignTo a = traverseOf (the @"multiPatchPos") (traverse (alignPos a))

-- | Attempt to align a patch to its stored alignment data.
align
    :: (Alignable p d a, SeekRep s ~ Natural)
    => Aligned (p 'RelSeek (Meta s d) a)
    -> Either (Error s) (p s d a)
align (Aligned a d) = alignTo a d

-- | Attempt to align a list of patches to the list's stored alignment data.
alignList
    :: (Alignable p d a, SeekRep s ~ Natural)
    => Aligned [p 'RelSeek (Meta s d) a]
    -> Either (Error s) [p s d a]
alignList (Aligned a ds) = traverse (alignTo a) ds

-- | Attempt to "align to 0" a relative 'Pos' to any seek represented by a
--   'Natural'.
alignPos
    :: (SeekRep s ~ Natural)
    => SeekRep 'RelSeek
    -> Pos 'RelSeek (Meta s d a)
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
