{-# LANGUAGE UndecidableInstances #-} -- for @'ToJSON' t@
module Bytepatch.Schema where

import GHC.Generics ( Generic, Generically(..) )
import Data.Aeson ( ToJSON, FromJSON )

data Patch t a = Patch
  { patchData :: a
  , patchAt   :: t (Location a)
  } deriving stock Generic

deriving via Generically (Patch t a)
    instance (ToJSON a, ToJSON (t (Location a))) => ToJSON (Patch t a)

data Location a = Location
  { locSeek  :: Int
  , locCheck :: Check a
  } deriving stock Generic
    deriving (ToJSON, FromJSON) via Generically (Location a)

data Check a = Check
  { checkData     :: a
  , checkLength   :: Int
  , checkNullterm :: Int
  } deriving stock Generic
    deriving (ToJSON, FromJSON) via Generically (Check a)
