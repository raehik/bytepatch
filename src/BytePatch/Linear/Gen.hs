module BytePatch.Linear.Gen (gen, Error(..)) where

import           BytePatch.Core
import           BytePatch.Linear.Core

import qualified Data.ByteString        as BS
import           Control.Monad.State
import qualified Data.List              as List

type Bytes = BS.ByteString

-- | Error encountered during linear patchscript generation.
data Error a
  = ErrorOverlap (Patch a) (Patch a)
  -- ^ Two edits wrote to the same offset.
    deriving (Eq, Show)

-- | Process an offset patchscript into a linear patchscript.
--
-- Errors are reported, but do not interrupt patch generation. The user could
-- discard patchscripts that errored, or perhaps attempt to recover them. This
-- is what we do for errors:
--
--   * overlapping edit: later edit is skipped & overlapping edits reported
gen :: [Patch Bytes] -> (Patchscript Bytes, [Error Bytes])
gen pList =
    let pList'                 = List.sortBy comparePatchOffsets pList
        (_, script, errors, _) = execState (go pList') (0, [], [], undefined)
        -- I believe the undefined is inaccessible providing the first patch has
        -- a non-negative offset (negative offsets are forbidden)
     in (reverse script, reverse errors)
  where
    comparePatchOffsets (Patch _ o1 _) (Patch _ o2 _) = compare o1 o2
    go :: (MonadState (Int, Patchscript Bytes, [Error Bytes], Patch Bytes) m) => [Patch Bytes] -> m ()
    go [] = return ()
    go (p@(Patch bs offset meta):ps) = do
        (cursor, script, errors, prevPatch) <- get
        case trySkipTo offset cursor of
          -- next offset is behind current cursor: overlapping patches
          -- record error, recover via dropping patch
          Left _ -> do
            let e = ErrorOverlap p prevPatch
            let errors' = e : errors
            put (cursor, script, errors', p)
            go ps
          Right skip -> do
            let cursor' = cursor + skip + BS.length bs
                o       = Overwrite bs meta
            put (cursor', (skip, o):script, errors, p)
            go ps
    trySkipTo to from =
        let diff = to - from in if diff >= 0 then Right diff else Left (-diff)
