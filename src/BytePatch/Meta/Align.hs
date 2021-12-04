module BytePatch.Meta.Align where

import           BytePatch.Core

import           GHC.Generics               ( Generic )
import           GHC.Natural
import           Data.Kind
import           Data.Functor.Identity
import           Optics
import           Data.Generics.Product.Any

class Alignable p d a where
    align
        :: SeekRep 'CursorSeek
        -> p 'CursorSeek (Align d) a
        -> Either Error (p 'AbsSeek d a)

data Align d a = Align
  { alignExpected :: Maybe (SeekRep 'AbsSeek)
  -- ^ Absolute stream offset for edit. Used for checking against actual offset.

  , alignInner     :: d a
  } deriving (Eq, Show, Generic, Functor, Foldable, Traversable)

data Error
  = ErrorSeekBelow0 (SeekRep 'CursorSeek)
  | ErrorDoesntMatchExpected (SeekRep 'AbsSeek) (SeekRep 'AbsSeek) -- expected, then actual
    deriving (Eq, Show, Generic)

instance Alignable Patch d a where
    align sBase = traverseOf (the @"patchPos") (alignPos sBase)

alignPos
    :: SeekRep 'CursorSeek
    -> Pos 'CursorSeek (Align d a)
    -> Either Error (Pos 'AbsSeek (d a))
alignPos sBase (Pos s meta) =
    case tryIntegerToNatural sAligned of
      Nothing          -> Left $ ErrorSeekBelow0 sAligned
      Just sAlignedNat ->
        case alignExpected meta of
          Nothing -> reform sAlignedNat
          Just sExpected ->
            if   sExpected == sAlignedNat
            then reform sAlignedNat
            else Left $ ErrorDoesntMatchExpected sExpected sAlignedNat
  where
    sAligned = sBase + s
    reform s' = Right $ Pos s' $ alignInner meta

tryIntegerToNatural :: Integer -> Maybe Natural
tryIntegerToNatural n | n < 0     = Nothing
                      | otherwise = Just $ naturalFromInteger n
