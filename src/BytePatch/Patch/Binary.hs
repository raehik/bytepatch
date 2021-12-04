module BytePatch.Patch.Binary where

import GHC.Generics ( Generic )

data Error a
  = ErrorUnexpectedNonNull a
  | ErrorDidNotMatchExpected a a
    deriving (Eq, Show, Generic, Functor, Foldable, Traversable)

data Meta a = Meta
  { mNullTerminates :: Maybe Int
  -- ^ Stream segment should be null bytes (0x00) only from this index onwards.

  , mExpected       :: Maybe a
  -- ^ Stream segment should be this.

  } deriving (Eq, Show, Generic, Functor, Foldable, Traversable)

data Cfg = Cfg
  { cfgAllowPartialExpected :: Bool
  -- ^ If enabled, allow partial expected bytes checking. If disabled, then even
  --   if the expected bytes are a prefix of the actual, fail.
  } deriving (Eq, Show, Generic)

--f :: a -> a -> Meta a -> Either String
