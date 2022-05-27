module StreamPatch.Seek where

import Data.Kind
import GHC.Generics ( Generic )
import Data.Data ( Data )
import Data.Aeson

class Seek (s :: Type) where
    type SeekRep s :: Type
    unSeek :: s -> SeekRep s
    mkSeek :: SeekRep s -> s

-- | Seek to stream index. Relative to a base, which can either move during
--   patching (actual relative) or stay constant ("absolute"). We abstract over
--   the internal type to allow using natural types where we want to disallow
--   negatives, and possibly allow sized types for efficiency.
newtype SIx a = SIx { unSIx :: a }
    deriving stock (Generic, Data, Show, Eq, Ord)
    deriving (ToJSON, FromJSON) via a
