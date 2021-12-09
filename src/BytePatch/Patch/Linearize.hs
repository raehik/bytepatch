-- | Attempt to linearize absolute-seeking patches.

module BytePatch.Patch.Linearize where

import           BytePatch.Patch
import           Util                   ( traverseM )

import           Control.Monad.State
import qualified Data.List              as List
import           GHC.Natural
import           GHC.Generics
import qualified Data.ByteString        as BS
import qualified Data.Text              as Text
import           Data.Text              ( Text )

class HasLength a where
    getLength :: a -> Natural

instance HasLength BS.ByteString where
    getLength = fromIntegral . BS.length
instance HasLength Text where
    getLength = fromIntegral . Text.length
instance HasLength String where
    getLength = fromIntegral . List.length

-- traverseM is some *shit*. Good luck if you wanna write this again or sth lol
--
-- The 'undefined' usage is safe: the cursor begins at 0, and @n - 0 >= 0@ for
-- any natural @n@.
linearize
    :: HasLength a
    => [Patch 'AbsSeek dd pd a]
    -> Either (Error dd pd a) [Patch 'FwdSeek dd pd a]
linearize ps = evalState (traverseM go (List.sortBy comparePatchSeeks ps)) (0, undefined)
  where
    go p = do
        (cursor, pPrev) <- get
        let pos = patchPos p
        case posSeek pos `minusNaturalMaybe` cursor of
          -- next absolute seek is before cursor: current patch overlaps prev
          Nothing -> return $ Left $ ErrorOverlap cursor p pPrev
          Just skip -> do
            let cursor' = cursor + skip + getLength (patchData p)
                p' = p { patchPos = pos { posSeek = skip } }
            put (cursor', p)
            return $ Right p'

data Error dd pd a
  = ErrorOverlap
        (SeekRep 'AbsSeek)      -- ^ absolute position in stream
        (Patch 'AbsSeek dd pd a)    -- ^ overlapping patch
        (Patch 'AbsSeek dd pd a)    -- ^ previous patch
  -- ^ Two edits wrote to the same offset.
    deriving (Eq, Show, Generic, Functor, Foldable, Traversable)

comparePatchSeeks :: Ord (SeekRep s) => Patch s dd pd a -> Patch s dd pd a -> Ordering
comparePatchSeeks p1 p2 = compare (g p1) (g p2)
  where g = posSeek . patchPos
