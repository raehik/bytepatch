{-| Convert between simple assembly & machine code.

Intended for single instructions and short snippets: using for larger programs
may not be as useful.
-}

module StreamPatch.Patch.Binary.Asm where

import Data.Text ( Text )
import Data.Text qualified as Text
import Data.ByteString qualified as BS
import Numeric.Natural ( Natural )
import GHC.Generics ( Generic )

import Keystone qualified

import System.IO.Unsafe ( unsafeDupablePerformIO )

import StreamPatch.Patch.Binary ( BinRep(..) )

import Data.Aeson qualified as Aeson

import Control.Monad.IO.Class
import Control.Monad.Except

data Arch
  = ArchArmV8ThumbLE
    deriving (Generic, Eq, Show)

-- | Each bytestring is like <10 length guaranteed. For ARM it's 2 or 4 I think.
--   Definitely a better representation available.
newtype MachineInstr (arch :: Arch) = MachineInstr { getMachineInstr :: BS.ByteString }
    deriving (Generic, Eq, Show)

-- | Mostly short strings. Buuuut comments allowed too. 128 or 256ch.
newtype AsmInstr     (arch :: Arch) = AsmInstr { getAsmInstr :: Text }
    deriving (Generic, Eq, Show)
    deriving Aeson.FromJSON via Text
    deriving Aeson.ToJSON   via Text

-- | Single-instruction assembly and disassembly for some architecture.
--
-- Both operations take minimal required context: nothing for assembly, address
-- for disassembly.
--
-- TODO assemble could take address. Keystone has an Maybe arg for it, and the
-- patcher will always have the address (kinda).
class Target arch where
    assembleInstr    ::            AsmInstr     arch -> Either Error (MachineInstr arch)
    disassembleInstr :: Natural -> MachineInstr arch -> Either Error (AsmInstr     arch)

data Error
  = ErrorOther String
    deriving (Generic, Eq, Show)

-- TODO should be same list size. Change to sized Vector?
assemble    :: Target arch =>            [AsmInstr     arch] -> Either Error [MachineInstr arch]
disassemble :: Target arch => Natural -> [MachineInstr arch] -> Either Error [AsmInstr     arch]

assemble    = traverse assembleInstr

-- TODO how to clean this up lol PLZ
disassemble initAddr = traverse (uncurry disassembleInstr) . go initAddr []
  where
    go _addr amis []       = reverse amis
    go addr  amis (mi:mis) = go (addr + fromIntegral (BS.length (getMachineInstr mi))) ((addr, mi) : amis) mis

-- TODO arbitrarily using unsafeDupablePerformIO
instance Target 'ArchArmV8ThumbLE where
    assembleInstr = fmap MachineInstr
                                      . unsafeDupablePerformIO
                                      . runExceptT
                                      . assemble' Keystone.ArchArm [Keystone.ModeV8, Keystone.ModeThumb, Keystone.ModeLittleEndian]
                                      . Text.unpack
                                      . getAsmInstr

-- single instr
instance Target arch => BinRep (AsmInstr arch) where
    toBinRep = mapEitherShow getMachineInstr . assembleInstr

-- multiple instrs
instance Target arch => BinRep [AsmInstr arch] where
    toBinRep = mapEitherShow (BS.concat . map getMachineInstr) . assemble

mapEitherShow :: Show a => (b -> c) -> Either a b -> Either String c
mapEitherShow f = either (Left . show) (Right . f)

-- | Helper for assembling a single instruction for some architecture &
--   configuration.
--
-- Note that you can pass more than one instruction by using @;@ or @\n@ as
-- separators. But we choose to limit that syntax, by checking that the a valid
-- result is only 1 instruction long.
assemble'
    :: (MonadIO m, MonadError Error m)
    => Keystone.Architecture -> [Keystone.Mode]
    -> String
    -> m BS.ByteString
assemble' arch modes inst = do
    let as' = Keystone.open arch modes
    liftIO (Keystone.runAssembler as') >>= \case
      Left  e  -> err $ "failed to obtain assembler: "<>show e
      Right as -> do
        let out' = Keystone.assemble as [inst] Nothing
        -- TODO have to inspect engine to find error. probably say if x=1 OK, if
        -- x>1 weird error, if x<1 check errno->strerror
        liftIO (Keystone.runAssembler out') >>= \case
          Left e -> err $ "error while assembling: "<>show e
          Right (mc, count) ->
            case count of
              1 -> return mc
              _ -> err $ "expected to assemble 1 instr, but assembled "<>show count
  where
    err = throwError . ErrorOther
