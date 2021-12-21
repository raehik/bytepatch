-- | Core patch type definitions: patches, seeks, metadata.

module StreamPatch.Patch where

import           Data.Kind
import           GHC.Generics ( Generic )
import           GHC.Natural
import           Data.Vinyl
import           Control.Applicative ( liftA2 )

type Patch :: SeekKind -> [Type -> Type] -> Type -> Type
data Patch s fs a = Patch
  { patchData :: a
  , patchSeek :: SeekRep s
  , patchMeta :: FunctorRec fs a
  } deriving (Generic)

deriving instance (Eq   a, Eq   (SeekRep s), Eq (Rec (Flap a) fs))  => Eq (Patch s fs a)
deriving instance (Show a, Show (SeekRep s), ReifyConstraint Show (Flap a) fs, RMap fs, RecordToList fs) => Show (Patch s fs a)
deriving instance (Functor (FunctorRec fs)) => Functor (Patch s fs)

-- Taken from vinyl-plus. Functor and Applicative instances were provided.
newtype Flap a f = Flap { getFlap :: f a } deriving (Eq, Show, Generic)
newtype FunctorRec fs a = FunctorRec { getFunctorRec :: Rec (Flap a) fs } deriving (Generic)
deriving instance (ReifyConstraint Show (Flap a) fs, RMap fs, RecordToList fs) => Show (FunctorRec fs a)
deriving instance Eq (Rec (Flap a) fs) => Eq (FunctorRec fs a)

instance Functor (FunctorRec '[]) where
  fmap _ (FunctorRec RNil) = FunctorRec RNil
instance (Functor r, Functor (FunctorRec rs)) => Functor (FunctorRec (r ': rs)) where
  fmap f (FunctorRec (Flap r :& rs)) =
    FunctorRec (Flap (fmap f r) :& getFunctorRec (fmap f (FunctorRec rs)))

instance Applicative (FunctorRec '[]) where
  pure _ = FunctorRec RNil
  FunctorRec RNil <*> FunctorRec RNil = FunctorRec RNil
instance (Applicative r, Applicative (FunctorRec rs)) => Applicative (FunctorRec (r ': rs)) where
  pure a = FunctorRec (Flap (pure a) :& getFunctorRec (pure a))
  FunctorRec (Flap f :& fs) <*> FunctorRec (Flap a :& as) =
    FunctorRec (Flap (f <*> a) :& getFunctorRec (FunctorRec fs <*> FunctorRec as))

instance Foldable (FunctorRec '[]) where
  foldr _ z (FunctorRec RNil) = z
instance (Foldable r, Foldable (FunctorRec rs)) => Foldable (FunctorRec (r ': rs)) where
    -- TODO foldr is harder lol
    foldMap f (FunctorRec (Flap r :& rs)) = foldMap f r <> foldMap f (FunctorRec rs)

-- I am shit at this LOL
instance Traversable (FunctorRec '[]) where
  traverse _ (FunctorRec RNil) = pure (FunctorRec RNil)
instance (Traversable r, Traversable (FunctorRec rs)) => Traversable (FunctorRec (r ': rs)) where
  traverse
      :: forall f a b. Applicative f
      => (a -> f b)
      -> (FunctorRec (r ': rs)) a
      -> f (FunctorRec (r ': rs) b)
  traverse f (FunctorRec (Flap (r :: r a) :& rs)) =
      FunctorRec <$> rBoth
    where
      rBoth :: f (Rec (Flap b) (r ': rs))
      rBoth = liftA2 (:&) rHead rTail
      rHead :: f (Flap b r)
      rHead = Flap <$> traverse f r
      rTail :: f (Rec (Flap b) rs)
      rTail = getFunctorRec <$> traverse f (FunctorRec rs)

-- | What a patch seek value means.
data SeekKind
  = FwdSeek -- ^ seeks only move cursor forward
  | RelSeek -- ^ seeks are relative e.g. to a universal base, or a stream cursor
  | AbsSeek -- ^ seeks specify an exact offset in stream
    deriving (Eq, Show, Generic)

-- | Get the representation for a 'SeekKind'. Allows us a bit more safety.
type family SeekRep (s :: SeekKind) where
    SeekRep 'FwdSeek = Natural
    SeekRep 'RelSeek = Integer
    SeekRep 'AbsSeek = Natural
