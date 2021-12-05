-- | Attempt to linearize an absolute-seeking patch.

module BytePatch.Patch.Linearize where

import           BytePatch.Patch

import           Control.Monad.State
import qualified Data.List              as List
import           GHC.Natural
import           GHC.Generics
import qualified Data.ByteString        as BS
import qualified Data.Text              as Text
import           Data.Text              ( Text )

class HasLength a where
    getLength' :: a -> Natural

instance HasLength BS.ByteString where
    getLength' = fromIntegral . BS.length
instance HasLength Text where
    getLength' = fromIntegral . Text.length
instance HasLength String where
    getLength' = fromIntegral . List.length

-- TODO do we gotta reverse lol
linearize
    :: HasLength a
    => [Patch 'AbsSeek m a]
    -> Either (Error m a) [Patch 'FwdSeek m a]
linearize ps = evalState (traverseM go (List.sortBy comparePatchSeeks ps)) 0
  where
    go p = do
        cursor <- get
        case posSeek (patchPos p) `minusNaturalMaybe` cursor of
          -- next offset is behind current cursor: current patch overlaps prev
          Nothing -> return $ Left $ ErrorOverlap' cursor p
          Just skip -> do
            let dataLen = getLength' $ patchData p
                cursor' = cursor + skip + dataLen
                -- can't do via optics due to pos type changing!!
                p' = p { patchPos = (patchPos p) { posSeek = cursor' } }
            put cursor'
            return $ Right p'

data Error m a
  = ErrorOverlap' (SeekRep 'AbsSeek) (Patch 'AbsSeek m a)
  -- ^ Two edits wrote to the same offset.
    deriving (Eq, Show, Generic, Functor, Foldable, Traversable)

comparePatchSeeks :: Ord (SeekRep s) => Patch s d a -> Patch s d a -> Ordering
comparePatchSeeks p1 p2 = compare (g p1) (g p2)
  where g = posSeek . patchPos

-- lol. ty hw-kafka-client
traverseM
    :: (Traversable t, Applicative f, Monad m)
    => (v -> m (f v'))
    -> t v
    -> m (f (t v'))
traverseM f xs = sequenceA <$> traverse f xs
