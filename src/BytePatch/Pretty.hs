{-|
Convenience interface to enable defining edits at offsets with some optional
safety checks.

Redefines some types to enable us to easily leverage Aeson's generic JSON schema
deriving. That sadly means we can't use some of the interesting offset plumbing.
-}

module BytePatch.Pretty
  (
  -- * Core types
    CommonMultiEdits(..)
  , MultiEdit(..)
  , EditOffset(..)

  -- * Convenience functions
  , normalizeSimple

  -- * Low-level interface
  , applyBaseOffset
  , listAlgebraConcatEtc
  , normalize
  ) where

import           BytePatch.Core
import           BytePatch.Linear.Core
import           BytePatch.Pretty.PatchRep

import qualified Data.ByteString            as BS
import           Data.Maybe                 ( fromMaybe )
import           GHC.Generics               ( Generic )

type Bytes = BS.ByteString

-- | A list of 'MultiEdit's with some common configuration.
data CommonMultiEdits a = CommonMultiEdits
  { cmesBaseOffset :: Maybe Int
  -- ^
  -- The base offset from which all offsets are located. An actual offset is
  -- calculated by adding the base offset to an offset. Actual offsets below 0
  -- are invalid, meaning for an offset @o@ with base offset @bo@, the actual
  -- offset is only valid when @o >= bo@. Negative base offsets are allowed.

  , cmesEdits :: [MultiEdit a]
  } deriving (Eq, Show, Generic, Functor, Foldable, Traversable)

-- | A single edit to be applied at a list of offsets.
data MultiEdit a = MultiEdit
  { meData :: a              -- ^ The value (e.g. bytes, text) to add.
  , meAt   :: [EditOffset a] -- ^ Offsets to apply edit at.
  } deriving (Eq, Show, Generic, Functor, Foldable, Traversable)

-- | An edit offset, with metadata to use for preparing and applying the edit.
data EditOffset a = EditOffset
  { eoOffset    :: Int
  -- ^ Stream offset for edit.

  , eoAbsOffset :: Maybe Int
  -- ^ Absolute stream offset for edit. Used for checking against actual offset.

  , eoMaxLength :: Maybe Int
  -- ^ Maximum number of bytes allowed to write at this offset.

  , eoEditMeta  :: Maybe (EditMeta a)
  -- ^ Optional apply time metadata for the edit at this offset.

  } deriving (Eq, Show, Generic, Functor, Foldable, Traversable)

-- | Normalize a list of 'CommonMultiEdits's, discarding everything on error.
normalizeSimple :: PatchRep a => [CommonMultiEdits a] -> Maybe [Patch Bytes]
normalizeSimple cmess =
    let (p, errs) = listAlgebraConcatEtc . map applyBaseOffset $ cmess
     in case errs of
          _:_ -> Nothing
          []  -> normalize p

-- Drops no info, not easy to consume.
applyBaseOffset :: CommonMultiEdits a -> (Int, [(MultiEdit a, [EditOffset a])])
applyBaseOffset cmes =
    (baseOffset, recalculateMultiPatchOffsets baseOffset (cmesEdits cmes))
      where baseOffset = fromMaybe 0 (cmesBaseOffset cmes)

-- lmao this sucks. generalisation bad
listAlgebraConcatEtc :: [(a, [(b, [c])])] -> ([b], [(c, a)])
listAlgebraConcatEtc = mconcat . map go
  where
    go (baseOffset, inps) = tuplemconcat (map (go' baseOffset) inps)
    go' x (mp, offs) = (mp, map (\o -> (o, x)) offs)
    tuplemconcat = foldr (\(a, bs) (as, bs') -> (a:as, bs <> bs')) ([], mempty)

recalculateMultiPatchOffsets :: Int -> [MultiEdit a] -> [(MultiEdit a, [EditOffset a])]
recalculateMultiPatchOffsets baseOffset = map go
  where
    go :: MultiEdit a -> (MultiEdit a, [EditOffset a])
    go me =
        let (osRecalculated, osInvalid) = recalculateOffsets baseOffset (meAt me)
         in (me { meAt = osRecalculated }, osInvalid)

recalculateOffsets :: Int -> [EditOffset a] -> ([EditOffset a], [EditOffset a])
recalculateOffsets baseOffset = partitionMaybe go
  where
    go o = if actualOffset >= 0 then Just (o { eoOffset = actualOffset }) else Nothing
      where actualOffset = baseOffset + eoOffset o

normalize :: PatchRep a => [MultiEdit a] -> Maybe [Patch Bytes]
normalize xs = concat <$> mapM go xs
  where go (MultiEdit contents os) = mapM (tryMakeSingleReplace contents) os

-- TODO now can error with "[expected] content has no valid patch rep"
tryMakeSingleReplace :: PatchRep a => a -> EditOffset a -> Maybe (Patch Bytes)
tryMakeSingleReplace contents (EditOffset os maos maxLen mMeta) =
    case toPatchRep contents of
      Left errStr -> error errStr -- TODO
      Right bs ->
        if   offsetIsCorrect
        then case maxLen of
               Just len -> if BS.length bs > len then Nothing else overwrite bs
               Nothing  -> overwrite bs
        else Nothing
  where
    overwrite bs = case traverse toPatchRep meta of
                     Left errStr -> error errStr -- TODO
                     Right meta' ->
                         Just $ WithOffset os $ Edit { editData = bs
                                                     , editMeta = meta' }
    meta = fromMaybe (EditMeta Nothing Nothing) mMeta
    offsetIsCorrect = case maos of Nothing  -> True
                                   Just aos -> os == aos

--------------------------------------------------------------------------------

-- | Map a failable function over a list, retaining "failed" 'Nothing' results.
partitionMaybe :: (a -> Maybe b) -> [a] -> ([b], [a])
partitionMaybe f =
    foldr (\x -> maybe (mapSnd (x:)) (\y -> mapFst (y:)) (f x)) ([], [])

mapFst :: (a -> c) -> (a, b) -> (c, b)
mapFst f (a, b) = (f a, b)

mapSnd :: (b -> c) -> (a, b) -> (a, c)
mapSnd f (a, b) = (a, f b)
