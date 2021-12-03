{-# LANGUAGE DataKinds, TypeFamilies, UndecidableInstances #-}

module BytePatch.Linear.Gen (gen, Error(..)) where

import           BytePatch.Core

import qualified Data.ByteString        as BS
import           Control.Monad.State
import qualified Data.List              as List
import           GHC.Natural

type Bytes = BS.ByteString

-- | Error encountered during linear patchscript generation.
data Error s a
  = ErrorOverlap (Patch s a) (Patch s a)
  -- ^ Two edits wrote to the same offset.

deriving instance (Eq (SeekRep s), Eq a) => Eq (Error s a)
deriving instance (Show (SeekRep s), Show a) => Show (Error s a)

-- | Process a list of patches into a linear patch script.
--
-- Errors are reported, but do not interrupt patch generation. The user could
-- discard patchscripts that errored, or perhaps attempt to recover them. This
-- is what we do for errors:
--
--   * overlapping edit: later edit is skipped & overlapping edits reported
gen
    :: [Patch 'AbsSeek Bytes]
    -> ([Patch 'FwdSeek Bytes], [Error 'AbsSeek Bytes])
gen pList =
    let pList'                 = List.sortBy comparePatchOffsets pList
        (_, script, errors, _) = execState (go pList') (0, [], [], undefined)
        -- I believe the undefined is inaccessible providing the first patch has
        -- a non-negative offset (negative offsets are forbidden)
     in (reverse script, reverse errors)
  where
    comparePatchOffsets (Patch o1 _) (Patch o2 _) = compare o1 o2
    go
        :: (MonadState (Natural, [Patch 'FwdSeek Bytes], [Error 'AbsSeek Bytes], Patch 'AbsSeek Bytes) m)
        => [Patch 'AbsSeek Bytes]
        -> m ()
    go [] = return ()
    go (p@(Patch offset edit) : ps) = do
        (cursor, script, errors, prevPatch) <- get
        case offset `minusNaturalMaybe` cursor of
          -- next offset is behind current cursor: overlapping patches
          -- record error, recover via dropping patch
          Nothing -> do
            let e = ErrorOverlap p prevPatch
            let errors' = e : errors
            put (cursor, script, errors', p)
            go ps
          Just skip -> do
            let dataLen = fromIntegral $ BS.length $ editData edit
            let cursor' = cursor + skip + dataLen
            put (cursor', Patch skip edit : script, errors, p)
            go ps
