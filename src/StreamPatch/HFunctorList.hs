{-# LANGUAGE AllowAmbiguousTypes #-}

module StreamPatch.HFunctorList where

import Data.Vinyl
import Data.Vinyl.TypeLevel ( RDelete, RIndex )
import Control.Applicative  ( liftA2 )
import GHC.Generics         ( Generic )
import Foreign.Storable     ( Storable )

import Optics

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

-- Right. I only partly get this. As I understand, I'm leveraging deriving via
-- to generate the instance bodies, since they look the same as Rec but with a
-- set functor. So I just have to assure it that you can make it Storable in the
-- same way, given that @Flap a@ is storable (which is handled similarly at its
-- own definition).
deriving via (Rec (Flap a) '[])       instance Storable (HFunctorList '[] a)
deriving via (Rec (Flap a) (f ': fs)) instance (Storable (f a), Storable (Rec (Flap a) fs)) => Storable (HFunctorList (f ': fs) a)

-- It appears we can't do the same for 'Functor' etc., because the @a@ type
-- variable isn't bound, but must be for us to say what type to derive via. I
-- wonder if there is a workaround, but I can't figure it out.
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
    deriving Storable via (f a)

--------------------------------------------------------------------------------

-- | Get the value at a type in an HFunctorList.
hflGet
    :: forall f fs a i
    .  RElem f fs i
    => HFunctorList fs a
    -> f a
hflGet = getFlap . rget . getHFunctorList

-- | Put a value at a type in an HFunctorList.
hflPut
    :: forall f f' fs fs' a
    .  RecElem Rec f f' fs fs' (RIndex f fs)
    => f' a
    -> HFunctorList fs a
    -> HFunctorList fs' a
hflPut x = HFunctorList . rput' @_ @f (Flap x) . getHFunctorList

-- | Get a lens to the value at a type in an HFunctorList.
hflLens
    :: forall f f' fs fs' a s t
    .  ( RecElem Rec f f' fs fs' (RIndex f fs)
       , RElem f fs (RIndex f fs)
       , s ~ HFunctorList fs  a
       , t ~ HFunctorList fs' a )
    => Lens s t (f a) (f' a)
hflLens = lens hflGet (\hfl x -> hflPut @f x hfl)

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
