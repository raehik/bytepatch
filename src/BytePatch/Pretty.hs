-- | Convenience interface to enable defining edits at offsets with some
--   optional safety checks.

module BytePatch.Pretty
  (
  -- * Core types
    MultiPatches(..)
  , MultiPatch(..)
  , Offset(..)

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

-- | Normalize a set of 'MultiPatches', discarding everything on error.
normalizeSimple :: PatchRep a => [MultiPatches a] -> Maybe [Patch Bytes]
normalizeSimple mps =
    let (p, errs) = listAlgebraConcatEtc . map applyBaseOffset $ mps
     in case errs of
          _:_ -> Nothing
          []  -> normalize p

-- | A list of patches sharing a configuration, each applied at a list of
--   offsets, abstracted over patch type.
data MultiPatches a = MultiPatches
  { mpsBaseOffset :: Maybe Int
  -- ^ The base offset from which all offsets are located. Subtracted from each
  --   offset value to obtain the actual offset. Any offset located before the
  --   base offset (x where x < base) is discarded as erroneous.
  --
  -- This feature enables us to allow negative offsets. For example, say you set
  -- the base offset to @-10@. This is equivalent to stating that every offset
  -- in the list is to be shifted +10 bytes. Thus, all offsets x where x >= -10
  -- are now valid.
  --
  -- The original rationale behind this feature was to ease assembly patches on
  -- ELFs. Decompilers focus on virtual addresses, and apparently (in my
  -- experience) don't like to divulge physical file offsets. However, we can
  -- recover the physical offset of any virtual address via the following steps:
  --
  --   1. subtract the containing ELF segment's virtual address
  --   2.      add the containing ELF segment's physical offset
  --
  -- So we can prepare a base offset @elf_vaddr - elf_phys_offset@, which we can
  -- subtract from any virtual address inside that segment to retrieve its
  -- related byte offset in the ELF file. Thus, you need do that calculation
  -- manually once for every segment you patch, then you can use your
  -- decompiler's virtual addresses!
  --
  -- You can even specify absolute offsets, which are compared to the calculated
  -- actual offsets. So you get the best of both worlds!
  --
  -- Absolute offsets are only used for asserting correctness of calculated
  -- actual offsets. If you want to mix absolute and base-relative offsets...
  -- don't. I'm loath to support that, because I believe it would serve only to
  -- confuse the patch file interface. Instead, group patches into absolute
  -- (base offset = 0) and base-relative lists.

  , mpsPatches :: [MultiPatch a]
  } deriving (Eq, Show, Generic)

-- | A single patch applied at a list of offsets, parameterized by patch type.
data MultiPatch a = MultiPatch
  { mpContents    :: a
  -- ^ The value to patch in. Likely a bytestring or text for simple uses.
  , mpOffsets     :: [Offset a]
  } deriving (Eq, Show, Generic)

-- | An offset in a stream, with metadata about it to use when preparing the
--   patch and at patch time.
data Offset a = Offset
  { oOffset         :: Int
  -- ^ Stream offset to patch at.

  , oAbsoluteOffset :: Maybe Int
  -- ^ Absolute stream offset to patch at. Compared with actual offset
  --   (calculated from offset and base offset).

  , oMaxLength      :: Maybe Int
  -- ^ Maximum bytestring length allowed to patch in at this offset.
  -- TODO: use single range/span instead (default 0->x, also allow y->x)

  , oPatchMeta      :: Maybe (OverwriteMeta a)
  -- ^ Patch-time info for the overwrite at this offset.
  --
  -- Named "patch meta" instead of the more correct "overwrite meta" for more
  -- friendly JSON field naming. We wrap it in a 'Maybe' for similar reasons,
  -- plus it means the default can be inserted later on.
  } deriving (Eq, Show, Generic)

-- Drops no info, not easy to consume.
applyBaseOffset :: MultiPatches a -> (Int, [(MultiPatch a, [Offset a])])
applyBaseOffset mps =
    (baseOffset, recalculateMultiPatchOffsets baseOffset (mpsPatches mps))
      where baseOffset = fromMaybe 0 (mpsBaseOffset mps)

-- lmao this sucks. generalisation bad
listAlgebraConcatEtc :: [(a, [(b, [c])])] -> ([b], [(c, a)])
listAlgebraConcatEtc = mconcat . map go
  where
    go (baseOffset, inps) = tuplemconcat (map (go' baseOffset) inps)
    go' x (mp, offs) = (mp, map (\o -> (o, x)) offs)
    tuplemconcat = foldr (\(a, bs) (as, bs') -> (a:as, bs <> bs')) ([], mempty)

recalculateMultiPatchOffsets :: Int -> [MultiPatch a] -> [(MultiPatch a, [Offset a])]
recalculateMultiPatchOffsets baseOffset = map go
  where
    go :: MultiPatch a -> (MultiPatch a, [Offset a])
    go mp =
        let (osRecalculated, osInvalid) = recalculateOffsets baseOffset (mpOffsets mp)
         in (mp { mpOffsets = osRecalculated }, osInvalid)

recalculateOffsets :: Int -> [Offset a] -> ([Offset a], [Offset a])
recalculateOffsets baseOffset = partitionMaybe go
  where
    go o = if actualOffset >= 0 then Just (o { oOffset = actualOffset }) else Nothing
      where actualOffset = oOffset o - baseOffset

normalize :: PatchRep a => [MultiPatch a] -> Maybe [Patch Bytes]
normalize xs = concat <$> mapM go xs
  where go (MultiPatch contents os) = mapM (tryMakeSingleReplace contents) os

-- TODO now can error with "[expected] content has no valid patch rep"
tryMakeSingleReplace :: PatchRep a => a -> Offset a -> Maybe (Patch Bytes)
tryMakeSingleReplace contents (Offset os maos maxLen mMeta) =
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
                     Right meta' -> Just $ Patch { patchContents = bs
                                                 , patchOffset   = os
                                                 , patchMeta     = meta' }
    meta = fromMaybe (OverwriteMeta Nothing Nothing) mMeta
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
