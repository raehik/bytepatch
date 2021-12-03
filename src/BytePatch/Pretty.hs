{-# LANGUAGE DataKinds, TypeFamilies, UndecidableInstances #-}

{-|
Convenience interface to enable defining edits at offsets with some optional
safety checks.

Redefines some types to enable us to easily leverage Aeson's generic JSON schema
deriving. That sadly means we can't use some of the interesting offset plumbing.

TODO I should definitely bite the bullet and use the plumbing now that it's so
cool.
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
import           BytePatch.Pretty.PatchRep

import qualified Data.ByteString            as BS
import           Data.Maybe                 ( fromMaybe )
import           GHC.Generics               ( Generic )
import           GHC.Natural

type Bytes = BS.ByteString

-- | A list of 'MultiEdit's with some common configuration.
data CommonMultiEdits a = CommonMultiEdits
  { cmesBaseOffset :: Maybe (SeekRep 'CursorSeek)
  -- ^
  -- The base offset from which all offsets are located. An actual offset is
  -- calculated by adding the base offset to an offset. Actual offsets below 0
  -- are invalid, meaning for an offset @o@ with base offset @bo@, the actual
  -- offset is only valid when @o >= bo@. Negative base offsets are allowed.

  , cmesEdits :: [MultiEdit 'CursorSeek a]
  } deriving (Eq, Show, Generic, Functor, Foldable, Traversable)

-- | A single edit to be applied at a list of offsets.
data MultiEdit (s :: SeekKind) a = MultiEdit
  { meData :: a              -- ^ The value (e.g. bytes, text) to add.
  , meAt   :: [EditOffset s a] -- ^ Offsets to apply edit at.
  } deriving (Generic, Functor, Foldable, Traversable)

deriving instance (Eq (SeekRep s), Eq a) => Eq (MultiEdit s a)
deriving instance (Show (SeekRep s), Show a) => Show (MultiEdit s a)

-- | An edit offset, with metadata to use for preparing and applying the edit.
data EditOffset (s :: SeekKind) a = EditOffset
  { eoOffset    :: SeekRep s
  -- ^ Stream offset for edit.

  , eoAbsOffset :: Maybe (SeekRep 'AbsSeek)
  -- ^ Absolute stream offset for edit. Used for checking against actual offset.

  , eoMaxLength :: Maybe Natural
  -- ^ Maximum number of bytes allowed to write at this offset.

  , eoEditMeta  :: Maybe (EditMeta a)
  -- ^ Optional apply time metadata for the edit at this offset.

  } deriving (Generic, Functor, Foldable, Traversable)

deriving instance (Eq (SeekRep s), Eq a) => Eq (EditOffset s a)
deriving instance (Show (SeekRep s), Show a) => Show (EditOffset s a)

-- | Normalize a list of 'CommonMultiEdits's, discarding everything on error.
normalizeSimple :: PatchRep a => [CommonMultiEdits a] -> Maybe [Patch 'AbsSeek Bytes]
normalizeSimple cmess =
    let (p, errs) = listAlgebraConcatEtc . map applyBaseOffset $ cmess
     in case errs of
          _:_ -> Nothing
          []  -> normalize p

-- Drops no info, not easy to consume.
applyBaseOffset
    :: CommonMultiEdits a
    -> (Integer, [(MultiEdit 'AbsSeek a, [EditOffset 'CursorSeek a])])
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

recalculateMultiPatchOffsets
    :: Integer
    -> [MultiEdit 'CursorSeek a]
    -> [(MultiEdit 'AbsSeek a, [EditOffset 'CursorSeek a])]
recalculateMultiPatchOffsets baseOffset = map go
  where
    go :: MultiEdit 'CursorSeek a -> (MultiEdit 'AbsSeek a, [EditOffset 'CursorSeek a])
    go me =
        let (osRecalculated, osInvalid) = recalculateOffsets baseOffset (meAt me)
         in (me { meAt = osRecalculated }, osInvalid)

recalculateOffsets
    :: Integer
    -> [EditOffset 'CursorSeek a]
    -> ([EditOffset 'AbsSeek a], [EditOffset 'CursorSeek a])
recalculateOffsets baseOffset = partitionMaybe go
  where
    go o = let actualOffset = baseOffset + eoOffset o
            in case tryIntegerToNatural actualOffset of
                 Nothing -> Nothing
                 Just actualOffset' -> Just $ o { eoOffset = actualOffset' }

tryIntegerToNatural :: Integer -> Maybe Natural
tryIntegerToNatural n | n < 0     = Nothing
                      | otherwise = Just $ naturalFromInteger n

normalize :: PatchRep a => [MultiEdit 'AbsSeek a] -> Maybe [Patch 'AbsSeek Bytes]
normalize xs = concat <$> mapM go xs
  where go (MultiEdit contents os) = mapM (tryMakeSingleReplace contents) os

-- TODO now can error with "[expected] content has no valid patch rep"
tryMakeSingleReplace :: PatchRep a => a -> EditOffset 'AbsSeek a -> Maybe (Patch 'AbsSeek Bytes)
tryMakeSingleReplace contents (EditOffset os maos mMaxLen mMeta) =
    case toPatchRep contents of
      Left errStr -> error errStr -- TODO
      Right bs ->
        if   offsetIsCorrect
        then case mMaxLen of
               Just maxLen -> if BS.length bs > fromIntegral maxLen then Nothing else overwrite bs
               Nothing     -> overwrite bs
        else Nothing
  where
    overwrite bs = case traverse toPatchRep meta of
                     Left errStr -> error errStr -- TODO
                     Right meta' ->
                         Just $ Patch os $ Edit { editData = bs
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
