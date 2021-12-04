{-# LANGUAGE DataKinds, TypeFamilies, UndecidableInstances #-}

module BytePatch.Pretty
  (
  -- * Core types
    BinMultiPatches(..)
  , Meta(..)

  -- * Convenience functions
  , normalizeSimple

  -- * Low-level interface
  , applyBaseOffset
  , listAlgebraConcatEtc
  , normalize
  ) where

import           BytePatch.Core
import qualified BytePatch.Patch.Binary     as Bin
import           BytePatch.Patch.Binary     ( BinRep(..) )

import qualified Data.ByteString            as BS
import           Data.Maybe                 ( fromMaybe )
import           GHC.Generics               ( Generic )
import           GHC.Natural

type Bytes = BS.ByteString

data BinMultiPatches a = BinMultiPatches
  { bmpsBaseOffset :: Maybe (SeekRep 'CursorSeek)
  -- ^
  -- The base offset from which all offsets are located. An actual offset is
  -- calculated by adding the base offset to an offset. Actual offsets below 0
  -- are invalid, meaning for an offset @o@ with base offset @bo@, the actual
  -- offset is only valid when @o >= bo@. Negative base offsets are allowed.

  , bmpsEdits :: [MultiPatch 'CursorSeek Meta a]
  } deriving (Eq, Show, Generic, Functor, Foldable, Traversable)

data Meta a = Meta
  { mAbsOffset :: Maybe (SeekRep 'AbsSeek)
  -- ^ Absolute stream offset for edit. Used for checking against actual offset.

  , mMaxLength :: Maybe Natural
  -- ^ Maximum number of bytes allowed to write at this offset.

  , mBinMeta   :: Maybe (Bin.Meta a)
  -- ^ Optional apply time metadata for the edit at this offset.

  } deriving (Eq, Show, Generic, Functor, Foldable, Traversable)

-- | Normalize a list of 'CommonMultiEdits's, discarding everything on error.
normalizeSimple :: BinRep a => [BinMultiPatches a] -> Maybe [Patch 'AbsSeek Bin.Meta Bytes]
normalizeSimple cmess =
    let (p, errs) = listAlgebraConcatEtc . map applyBaseOffset $ cmess
     in case errs of
          _:_ -> Nothing
          []  -> normalize p

-- Drops no info, not easy to consume.
applyBaseOffset
    :: BinMultiPatches a
    -> (Integer, [(MultiPatch 'AbsSeek Meta a, [Pos 'CursorSeek (Meta a)])])
applyBaseOffset bmps =
    (baseOffset, recalculateMultiPatchOffsets baseOffset (bmpsEdits bmps))
      where baseOffset = fromMaybe 0 (bmpsBaseOffset bmps)

-- lmao this sucks. generalisation bad
listAlgebraConcatEtc :: [(a, [(b, [c])])] -> ([b], [(c, a)])
listAlgebraConcatEtc = mconcat . map go
  where
    go (baseOffset, inps) = tuplemconcat (map (go' baseOffset) inps)
    go' x (mp, offs) = (mp, map (\o -> (o, x)) offs)
    tuplemconcat = foldr (\(a, bs) (as, bs') -> (a:as, bs <> bs')) ([], mempty)

recalculateMultiPatchOffsets
    :: Integer
    -> [MultiPatch 'CursorSeek Meta a]
    -> [(MultiPatch 'AbsSeek Meta a, [Pos 'CursorSeek (Meta a)])]
recalculateMultiPatchOffsets baseOffset = map go
  where
    go :: MultiPatch 'CursorSeek Meta a -> (MultiPatch 'AbsSeek Meta a, [Pos 'CursorSeek (Meta a)])
    go mp =
        let (osRecalculated, osInvalid) = recalculateOffsets baseOffset (multiPatchPos mp)
         in (mp { multiPatchPos = osRecalculated }, osInvalid)

recalculateOffsets
    :: Integer
    -> [Pos 'CursorSeek (Meta a)]
    -> ([Pos 'AbsSeek (Meta a)], [Pos 'CursorSeek (Meta a)])
recalculateOffsets baseOffset = partitionMaybe go
  where
    go p = let actualOffset = baseOffset + posSeek p
            in case tryIntegerToNatural actualOffset of
                 Nothing -> Nothing
                 Just actualOffset' -> Just p { posSeek = actualOffset' }

tryIntegerToNatural :: Integer -> Maybe Natural
tryIntegerToNatural n | n < 0     = Nothing
                      | otherwise = Just $ naturalFromInteger n

normalize :: BinRep a => [MultiPatch 'AbsSeek Meta a] -> Maybe [Patch 'AbsSeek Bin.Meta Bytes]
normalize xs = concat <$> mapM go xs
  where go (MultiPatch d ps) = mapM (\p -> tryMakeSingleReplace (Patch d p)) ps

-- TODO now can error with "[expected] content has no valid patch rep"
tryMakeSingleReplace :: BinRep a => Patch 'AbsSeek Meta a -> Maybe (Patch 'AbsSeek Bin.Meta Bytes)
tryMakeSingleReplace (Patch d (Pos os (Meta maos mMaxLen mBinMeta))) =
    case toBinRep d of
      Left errStr -> error errStr -- TODO
      Right bs ->
        if   offsetIsCorrect
        then case mMaxLen of
               Just maxLen -> if BS.length bs > fromIntegral maxLen then Nothing else overwrite bs
               Nothing     -> overwrite bs
        else Nothing
  where
    overwrite bs = case traverse toBinRep binMeta of
                     Left  errStr   -> error errStr -- TODO
                     Right binMeta' -> Just $ Patch bs (Pos os binMeta')
    binMeta = fromMaybe (Bin.Meta Nothing Nothing) mBinMeta
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
