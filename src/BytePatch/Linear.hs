module BytePatch.Linear (gen, Error(..)) where

import           BytePatch.Core

import qualified Data.ByteString        as BS
import           Control.Monad.State
import qualified Data.List              as List
import           GHC.Natural

type Bytes = BS.ByteString

-- | Error encountered during linear patchscript generation.
data Error s m a
  = ErrorOverlap (Patch s m a) (Patch s m a)
  -- ^ Two edits wrote to the same offset.

deriving instance (Eq (SeekRep s), Eq (m a), Eq a) => Eq (Error s m a)
deriving instance (Show (SeekRep s), Show (m a), Show a) => Show (Error s m a)

f :: Patch 'AbsSeek d a -> Patch 'AbsSeek d a -> Ordering
f p1 p2 = compare (posSeek (patchPos p1)) (posSeek (patchPos p2))

-- | Process a list of patches into a linear patch script.
--
-- Errors are reported, but do not interrupt patch generation. The user could
-- discard patchscripts that errored, or perhaps attempt to recover them. This
-- is what we do for errors:
--
--   * overlapping edit: later edit is skipped & overlapping edits reported
gen
    :: [Patch 'AbsSeek m Bytes]
    -> ([Patch 'FwdSeek m Bytes], [Error 'AbsSeek m Bytes])
gen pList =
    let pList'                 = List.sortBy f pList
        (_, script, errors, _) = execState (go pList') (0, [], [], undefined)
        -- I believe the undefined is inaccessible providing the first patch has
        -- a non-negative offset (negative offsets are forbidden)
     in (reverse script, reverse errors)
  where
    go [] = return ()
    go (p:ps) = do
        (cursor, script, errors, prevPatch) <- get
        case posSeek (patchPos p) `minusNaturalMaybe` cursor of
          -- next offset is behind current cursor: overlapping patches
          -- record error, recover via dropping patch
          Nothing -> do
            let e = ErrorOverlap p prevPatch
            let errors' = e : errors
            put (cursor, script, errors', p)
            go ps
          Just skip -> do
            let dataLen = fromIntegral $ BS.length $ patchData p
            let cursor' = cursor + skip + dataLen
            let p' = p { patchPos = (patchPos p) { posSeek = cursor' } } -- TODO do via optics
            put (cursor', p' : script, errors, p)
            go ps
