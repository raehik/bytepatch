module BytePatch.Meta.Normalize where

import           BytePatch.Core

import           GHC.Generics               ( Generic )
import           GHC.Natural
import           Data.Kind
import           Optics
import           Data.Generics.Product.Any

data NormalizedMultiPatches (d :: Type -> Type) a = NormalizedMultiPatches
  { nmpsBaseOffset :: SeekRep 'CursorSeek
  -- ^
  -- The base offset from which all offsets are located. An actual offset is
  -- calculated by adding the base offset to an offset. Actual offsets below 0
  -- are invalid, meaning for an offset @o@ with base offset @bo@, the actual
  -- offset is only valid when @o >= bo@. Negative base offsets are allowed.

  , nmpsMultiPatches :: [MultiPatch 'CursorSeek (Normalized d) a]
  } deriving (Eq, Show, Generic, Functor, Foldable, Traversable)

data Normalized (d :: Type -> Type) a = Normalized
  { normalizedAbsOffset :: Maybe (SeekRep 'AbsSeek)
  -- ^ Absolute stream offset for edit. Used for checking against actual offset.

  , normalizedInner     :: d a
  } deriving (Eq, Show, Generic, Functor, Foldable, Traversable)

data Error
  = ErrorNormalizedSeekBelow0 Integer
  | ErrorNormalizedDoesntMatchExpected Natural Natural -- expected, then actual
    deriving (Eq, Show, Generic)

normalize
    :: [NormalizedMultiPatches d a]
    -> Either Error [MultiPatch 'AbsSeek d a]
normalize xs = concat <$> traverse normalize1 xs

normalize1
    :: NormalizedMultiPatches d a
    -> Either Error [MultiPatch 'AbsSeek d a]
normalize1 (NormalizedMultiPatches bo mps) = traverse (normalize2 bo) mps

{-
Holy shit. Cool. To be clear, this is what this is doing:

    normalize2 bo x = case normalize3 bo (multiPatchPos x) of
                        Left err -> Left err
                        Right p  -> Right x { multiPatchPos = p }

OK.
-}
normalize2
    :: SeekRep 'CursorSeek
    -> MultiPatch 'CursorSeek (Normalized d) a
    -> Either Error (MultiPatch 'AbsSeek d a)
normalize2 bo = traverseOf (the @"multiPatchPos") (normalize3 bo)

normalize3
    :: SeekRep 'CursorSeek
    -> [Pos 'CursorSeek (Normalized d a)]
    -> Either Error [Pos 'AbsSeek (d a)]
normalize3 bo = traverse (normalize4 bo)

normalize4
    :: SeekRep 'CursorSeek
    -> Pos 'CursorSeek (Normalized d a)
    -> Either Error (Pos 'AbsSeek (d a))
normalize4 bo (Pos seek meta) =
    let seekNorm = bo + seek
     in case tryIntegerToNatural seekNorm of
          Nothing -> Left $ ErrorNormalizedSeekBelow0 seekNorm
          Just seekNormNat ->
            case normalizedAbsOffset meta of
              Nothing -> reform seekNormNat
              Just seekAbs ->
                if   seekAbs == seekNormNat
                then reform seekNormNat
                else Left $ ErrorNormalizedDoesntMatchExpected seekAbs seekNormNat
  where reform seek' = Right $ Pos seek' $ normalizedInner meta

tryIntegerToNatural :: Integer -> Maybe Natural
tryIntegerToNatural n | n < 0     = Nothing
                      | otherwise = Just $ naturalFromInteger n

{-

-- Drops no info, not easy to consume.
applyBaseOffset
    :: BinMultiPatches a
    -> (Integer, [(MultiPatch 'AbsSeek Meta a, [Pos 'CursorSeek (Meta a)])])
applyBaseOffset nmps =
    (baseOffset, recalculateMultiPatchOffsets baseOffset (nmpsEdits nmps))
      where baseOffset = fromMaybe 0 (nmpsBaseOffset nmps)

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
    binMeta = fromMaybe (Bin.Meta Nothing Nothing Nothing) mBinMeta
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

-}
