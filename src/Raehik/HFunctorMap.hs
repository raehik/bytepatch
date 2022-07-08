{-# LANGUAGE AllowAmbiguousTypes, UndecidableInstances #-}

module Raehik.HFunctorMap where

import Data.Kind
import GHC.TypeLits
import Data.Vinyl
import Data.Vinyl.TypeLevel
import GHC.Generics ( Generic )
import Data.Aeson
import GHC.Exts
import Data.Aeson.KeyMap qualified as KM
import Data.Aeson.Key qualified as K

-- labelled flipped application
type LFlap :: k -> (Symbol, k -> Type) -> Type
newtype LFlap a fl = LFlap { getLFlap :: (Snd fl) a }
    deriving stock (Generic)
deriving stock    instance Show   (Snd fl a) => Show   (LFlap a fl)
deriving anyclass instance ToJSON (Snd fl a) => ToJSON (LFlap a fl)

{-
newtype Flap a f = Flap { getFlap :: f a }
    deriving stock   (Generic, Show, Eq, Ord)
    deriving Storable via (f a)
    deriving (ToJSON, FromJSON) via (f a)
-}

lFlap :: forall l a f. f a -> LFlap a '(l, f)
lFlap = LFlap

-- labelled functor list
newtype LFunctorList fs a = LFunctorList { getLFunctorList :: Rec (LFlap a) fs }
    deriving stock (Generic)
deriving stock    instance
    (ReifyConstraint Show (LFlap a) fs, RMap fs, RecordToList fs)
      => Show (LFunctorList fs a)

instance ToJSON (LFunctorList '[] a) where
    toJSON (LFunctorList RNil) = Object mempty
instance (ToJSON (f a), KnownSymbol l, ToJSON (LFunctorList fs a)) => ToJSON (LFunctorList ('(l, f) ': fs) a) where
    toJSON (LFunctorList (LFlap fa :& fs)) =
        let Object os = toJSON $ LFunctorList fs
            label = symbolVal'' @l
            o = toJSON fa
        in  Object $ KM.insert (K.fromString label) o os

symbolVal'' :: forall l. KnownSymbol l => String
symbolVal'' = symbolVal' (proxy# :: Proxy# l)

-- use with visible type applications
lflgetf
    :: forall (l :: Symbol) f fs a
    .  ( HasField Rec l fs fs f f )
    => LFunctorList fs a -> f a
lflgetf = getLFlap . rget @(l ::: f) . getLFunctorList

{-

import Data.Vinyl
import Data.Vinyl.TypeLevel ( RDelete, RIndex )
import Control.Applicative  ( liftA2 )
import GHC.Generics         ( Generic, Rep )
import Foreign.Storable     ( Storable )

import Optics

import Data.Aeson

instance ( ToJSON (Flap a r), Generic (Rec (Flap a) rs)
         , GToJSON' Value Zero (Rep (Rec (Flap a) rs))
         , GToJSON' Encoding Zero (Rep (Rec (Flap a) rs))
         ) => ToJSON (Rec (Flap a) (r ': rs))

-- | A list of functors parametric over a "shared" 'a', where each functor
--   stores a single value 'f a'.
--
-- Just a wrapper on top of Vinyl with some types swap around.
newtype HFunctorList fs a = HFunctorList { getHFunctorList :: Rec (Flap a) fs }
    deriving stock (Generic)

deriving via (Rec (Flap a) fs) instance ToJSON (Rec (Flap a) fs) => ToJSON (HFunctorList fs a)

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
    deriving stock   (Generic, Show, Eq, Ord)
    deriving Storable via (f a)
    deriving (ToJSON, FromJSON) via (f a)

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

-}
