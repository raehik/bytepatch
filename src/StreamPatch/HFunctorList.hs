module StreamPatch.HFunctorList where

import           Data.Vinyl
import           Data.Vinyl.TypeLevel ( RDelete )
import           Control.Applicative  ( liftA2 )
import           GHC.Generics         ( Generic )
import           Foreign.Storable     ( Storable )

-- | A heterogeneous list of functors over 'a'. Each type in the list
--   corresponds to a single value.
--
-- Heterogeneous lists are nice, but sometimes you want one that you can map a
-- functor over. This type gives you that. We use vinyl for the representation:
-- it provides everything we need other than some newtypes to swap types around.
newtype HFunctorList fs a = HFunctorList { getHFunctorList :: Rec (Flap a) fs }
    deriving stock (Generic)

deriving instance (ReifyConstraint Show (Flap a) fs, RMap fs, RecordToList fs) => Show (HFunctorList fs a)
deriving instance Eq        (Rec (Flap a) fs) => Eq        (HFunctorList fs a)
deriving instance Ord       (Rec (Flap a) fs) => Ord       (HFunctorList fs a)
deriving instance Storable  (Rec (Flap a) fs) => Storable  (HFunctorList fs a)

instance Functor (HFunctorList '[]) where
  fmap _ (HFunctorList RNil) = HFunctorList RNil
instance (Functor r, Functor (HFunctorList rs)) => Functor (HFunctorList (r ': rs)) where
  fmap f (HFunctorList (Flap r :& rs)) =
    HFunctorList (Flap (fmap f r) :& getHFunctorList (fmap f (HFunctorList rs)))

instance Applicative (HFunctorList '[]) where
  pure _ = HFunctorList RNil
  HFunctorList RNil <*> HFunctorList RNil = HFunctorList RNil
instance (Applicative r, Applicative (HFunctorList rs)) => Applicative (HFunctorList (r ': rs)) where
  pure a = HFunctorList (Flap (pure a) :& getHFunctorList (pure a))
  HFunctorList (Flap f :& fs) <*> HFunctorList (Flap a :& as) =
    HFunctorList (Flap (f <*> a) :& getHFunctorList (HFunctorList fs <*> HFunctorList as))

instance Foldable (HFunctorList '[]) where
  foldr _ z (HFunctorList RNil) = z
instance (Foldable r, Foldable (HFunctorList rs)) => Foldable (HFunctorList (r ': rs)) where
    -- only foldmap because foldr is harder looool
    foldMap f (HFunctorList (Flap r :& rs)) = foldMap f r <> foldMap f (HFunctorList rs)

-- this took me ages because I'm stupid T_T
instance Traversable (HFunctorList '[]) where
  traverse _ (HFunctorList RNil) = pure (HFunctorList RNil)
instance (Traversable r, Traversable (HFunctorList rs)) => Traversable (HFunctorList (r ': rs)) where
  traverse
      :: forall f a b. Applicative f
      => (a -> f b)
      -> (HFunctorList (r ': rs)) a
      -> f (HFunctorList (r ': rs) b)
  traverse f (HFunctorList (Flap (r :: r a) :& rs)) =
      HFunctorList <$> rBoth
    where
      rBoth :: f (Rec (Flap b) (r ': rs))
      rBoth = liftA2 (:&) rHead rTail
      rHead :: f (Flap b r)
      rHead = Flap <$> traverse f r
      rTail :: f (Rec (Flap b) rs)
      rTail = getHFunctorList <$> traverse f (HFunctorList rs)

-- | Flipped apply: a single value at 'f a', but with "flipped" type arguments.
--   Very useless - has no Functor nor Contravariant nor HFunctor instance.
newtype Flap a f = Flap { getFlap :: f a }
    deriving stock   (Eq, Show, Ord, Generic)
    deriving newtype (Storable)

--------------------------------------------------------------------------------

-- | Get the value at a type in an HFunctorList.
hflGet
    :: forall f fs a i
    .  RElem f fs i
    => HFunctorList fs a
    -> f a
hflGet = getFlap . rget . getHFunctorList

-- | Use the value at a type in an HFunctorList, and remove it from the list.
hflStrip
    :: forall f fs a fs' b i is
    .  ( RElem f fs i
       , fs' ~ RDelete f fs
       , RSubset fs' fs is )
    => (f a -> b)
    -> HFunctorList fs a
    -> (b, HFunctorList fs' a)
hflStrip f hfl =
    let hfl' = HFunctorList $ rcast $ getHFunctorList hfl
     in (f (hflGet hfl), hfl')
