module StreamPatch.Patch.Linearize.InPlace where

import StreamPatch.Patch
import StreamPatch.HFunctorList
import StreamPatch.Patch.Linearize.Common

import GHC.Generics ( Generic )
import Numeric.Natural
import GHC.Natural ( minusNaturalMaybe )
import Data.Vinyl

import Control.Monad.State
import qualified Data.List              as List
import qualified Data.ByteString        as BS
import qualified Data.Text              as Text
import Data.Text              ( Text )
import StreamPatch.Util       ( traverseM )

class HasLength a where
    getLength :: a -> Natural

instance HasLength BS.ByteString where
    getLength = fromIntegral . BS.length
instance HasLength Text where
    getLength = fromIntegral . Text.length
instance HasLength String where
    getLength = fromIntegral . List.length

data Error fs a
  = ErrorOverlap -- ^ Two edits wrote to the same offset.
        Natural -- ^ absolute position in stream
        (Patch Natural fs a) -- ^ overlapping patch
        (Patch Natural fs a) -- ^ previous patch
    deriving (Generic)

deriving instance (Eq a, Eq (Rec (Flap a) fs)) => Eq (Error fs a)
deriving instance (Show a, ReifyConstraint Show (Flap a) fs, RMap fs, RecordToList fs) => Show (Error fs a)
deriving instance Functor     (HFunctorList fs) => Functor     (Error fs)
deriving instance Foldable    (HFunctorList fs) => Foldable    (Error fs)
deriving instance Traversable (HFunctorList fs) => Traversable (Error fs)

linearizeInPlace
    :: forall a fs. HasLength a
    => [Patch Natural fs a]
    -> Either (Error fs a) [Patch Natural fs a]
linearizeInPlace ps = evalState (traverseM go (List.sortBy comparePatchSeeks ps)) (0, undefined)
  where
    go p@(Patch a s _)  = do
        (cursor, pPrev) <- get
        case s `minusNaturalMaybe` cursor of
          -- next absolute seek is before cursor: current patch overlaps prev
          Nothing -> return $ Left $ ErrorOverlap cursor p pPrev
          Just skip -> do
            let cursor' = cursor + skip + getLength a
                p' = p { patchSeek = skip }
            put (cursor', p)
            return $ Right p'
