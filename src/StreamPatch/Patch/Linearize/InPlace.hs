module StreamPatch.Patch.Linearize.InPlace where

import StreamPatch.Patch
import StreamPatch.HFunctorList
import StreamPatch.Patch.Linearize.Common

import GHC.Generics ( Generic )
import Data.Vinyl

import Control.Monad.State
import qualified Data.List              as List
import qualified Data.ByteString        as BS
import qualified Data.Text              as Text
import Data.Text              ( Text )
import StreamPatch.Util       ( traverseM )

type Len = Int

class HasLength a where
    -- | Returns non-negative values only.
    getLength :: a -> Len

instance HasLength BS.ByteString where getLength = BS.length
instance HasLength Text          where getLength = Text.length
instance HasLength String        where getLength = List.length

data Error fs a
  = ErrorOverlap -- ^ Two edits wrote to the same offset.
        Len -- ^ absolute position in stream
        (Patch Len fs a) -- ^ overlapping patch
        (Patch Len fs a) -- ^ previous patch
    deriving (Generic)

deriving instance (Eq a, Eq (Rec (Flap a) fs)) => Eq (Error fs a)
deriving instance (Show a, ReifyConstraint Show (Flap a) fs, RMap fs, RecordToList fs) => Show (Error fs a)
deriving instance Functor     (HFunctorList fs) => Functor     (Error fs)
deriving instance Foldable    (HFunctorList fs) => Foldable    (Error fs)
deriving instance Traversable (HFunctorList fs) => Traversable (Error fs)

linearizeInPlace
    :: forall a fs. HasLength a
    => [Patch Len fs a]
    -> Either (Error fs a) [Patch Len fs a]
linearizeInPlace ps = evalState (traverseM go (List.sortBy comparePatchSeeks ps)) (0, undefined)
  where
    go p@(Patch a s _)  = do
        (cursor, pPrev) <- get
        let skip = s - cursor
        if skip < 0 then do
            -- next absolute seek is before cursor: current patch overlaps prev
            return $ Left $ ErrorOverlap cursor p pPrev
        else do
            let cursor' = cursor + skip + getLength a
                p' = p { patchSeek = skip }
            put (cursor', p)
            return $ Right p'
