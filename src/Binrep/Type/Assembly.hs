module Binrep.Type.Assembly where

import GHC.Generics ( Generic )
import Data.Data ( Typeable, Data )

import Data.ByteString.Short ( ShortByteString )
import Data.ByteString ( ByteString )
import Data.Text.Short ( ShortText )
import Data.Aeson ( ToJSON, FromJSON )

data Arch
  = ArchArmV8ThumbLE
    deriving stock (Generic, Typeable, Data, Show, Eq)

newtype MachineInstr (arch :: Arch) = MachineInstr { getMachineInstr :: ShortByteString }
    deriving stock (Generic, Typeable, Data, Show, Eq)

newtype MachineCode  (arch :: Arch) = MachineCode  { getMachineCode  :: ByteString }
    deriving stock (Generic, Typeable, Data, Show, Eq)

newtype AsmInstr     (arch :: Arch) = AsmInstr     { getAsmInstr     :: ShortText }
    deriving stock (Generic, Typeable, Data, Show, Eq)
    deriving (ToJSON, FromJSON) via ShortText
