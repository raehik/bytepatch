-- | Small interface on top of the linear binary patch algorithm.
module BytePatch.Pretty
  ( MultiPatches(..)
  , MultiPatch(..)
  , Offset(..)
  , Cfg(..)
  , PatchType(..)
  , applyBaseOffset
  , listAlgebraConcatEtc
  , normalize
  , ToBinPatch(..)

  , BP.OverwriteMeta(..)
  ) where

import qualified BytePatch.Patch            as BP
import           BytePatch.HexByteString

import qualified Data.ByteString            as BS
import qualified Data.Text.Encoding         as Text
import           Data.Text                  ( Text )
import           Data.Maybe                 ( fromMaybe )
import           GHC.Generics               ( Generic )

type Bytes = BS.ByteString

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

  , oPatchMeta      :: Maybe (BP.OverwriteMeta a)
  -- ^ Patch-time info for the overwrite at this offset.
  --
  -- Named "patch meta" instead of the more correct "overwrite meta" for more
  -- friendly JSON field naming. We wrap it in a 'Maybe' for similar reasons,
  -- plus it means the default can be inserted later on.
  } deriving (Eq, Show, Generic)

data Cfg = Cfg
  { cfgPatchType :: PatchType
  } deriving (Eq, Show)

-- | Indicates the type of patch being done. Both binary and text patches are
--   "compiled" down to simpler binary patches, but by remembering the original
--   format, we can attempt to provide better error behaviour.
data PatchType
  = PatchTypeBinary
  -- ^ Binary patch: show errors etc. using binary-friendly hex bytes
  | PatchTypeUTF8
  -- ^ Text (UTF-8) patch: show errors etc. using text (UTF-8)
    deriving (Eq, Show)

-- Drops no info, not easy to consume.
applyBaseOffset :: [MultiPatches a] -> [(Int, [(MultiPatch a, [Offset a])])]
applyBaseOffset = map go
  where
    go :: MultiPatches a -> (Int, [(MultiPatch a, [Offset a])])
    go mps = (baseOffset, recalculateMultiPatchOffsets baseOffset (mpsPatches mps))
      where
        baseOffset = fromMaybe 0 (mpsBaseOffset mps)

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

normalize :: ToBinPatch a => [MultiPatch a] -> Maybe [BP.Patch Bytes]
normalize xs = concat <$> mapM go xs
  where
    go (MultiPatch contents os) = mapM (tryMakeSingleReplace contents) os

tryMakeSingleReplace :: ToBinPatch a => a -> Offset a -> Maybe (BP.Patch Bytes)
tryMakeSingleReplace contents (Offset os maos maxLen mMeta) =
    if offsetIsCorrect
    then case maxLen of
           Just len -> if BS.length bs > len then Nothing else go
           Nothing  -> go
    else Nothing
  where
    go = Just (BP.Patch bs os metaBin)
    metaBin = fmap toBinPatch meta
    bs = toBinPatch contents
    meta = fromMaybe metaDefault mMeta
    metaDefault = BP.OverwriteMeta Nothing Nothing
    offsetIsCorrect = case maos of
      Nothing -> True
      Just aos -> os == aos

-- | How to turn a given type into a binary patch.
class ToBinPatch a where
    toBinPatch :: a -> BS.ByteString

-- | Bytestrings are copied as-is.
instance ToBinPatch BS.ByteString where
    toBinPatch = id

-- | Text is converted to UTF-8 bytes and null-terminated (!).
instance ToBinPatch Text where
    -- TODO sucks I gotta do a snoc here >:(
    toBinPatch t = BS.snoc (Text.encodeUtf8 t) 0x00

-- | Bytestring wrapper for Aeson.
instance ToBinPatch HexByteString where
    toBinPatch = unHexByteString

--------------------------------------------------------------------------------

partitionMaybe :: (a -> Maybe b) -> [a] -> ([b], [a])
partitionMaybe f =
    foldr
        (\x -> maybe (mapSnd (x:)) (\y -> mapFst (y:)) (f x))
        ([], [])

mapFst :: (a -> c) -> (a, b) -> (c, b)
mapFst f (a, b) = (f a, b)

mapSnd :: (b -> c) -> (a, b) -> (a, c)
mapSnd f (a, b) = (a, f b)
