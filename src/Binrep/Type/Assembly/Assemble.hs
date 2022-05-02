{-# LANGUAGE OverloadedStrings #-}

module Binrep.Type.Assembly.Assemble where

import Binrep.Type.Assembly

import Keystone qualified
import System.IO.Unsafe ( unsafeDupablePerformIO )
import Control.Monad.IO.Class
import Data.ByteString qualified as BS
import Data.Text.Short qualified as Text.Short
import Data.Text qualified as Text
import Data.List qualified as List

class Assemble arch where
    assemble :: [AsmInstr arch] -> Either String (MachineCode arch)

assemble1
    :: forall arch. Assemble arch
    => AsmInstr arch -> Either String (MachineCode arch)
assemble1 inst = assemble [inst]

instance Assemble 'ArmV8ThumbLE where
    assemble =
          fmap MachineCode
        . unsafeDupablePerformIO
        . assemble' Keystone.ArchArm modeFlags
        . prepInstrs
      where
        modeFlags =
            [Keystone.ModeV8, Keystone.ModeThumb, Keystone.ModeLittleEndian]

-- | TODO This is stupid because the assembler takes a '[String]'. Great for
--   end-user, poor for performance. I want the option to give it a
--   'BS.ByteString' that I've already prepared (as is the interface).
prepInstrs :: forall arch. [AsmInstr arch] -> [String]
prepInstrs =
      List.singleton
    . Text.unpack
    . Text.intercalate (Text.pack ";")
    . map (Text.Short.toText . getAsmInstr)

-- | Ideally, the assembler takes a raw 'BS.ByteString'. A
--   'BS.Short.ShortByteString' isn't appropriate, because it could be quite
--   large. But this way, this function is basically "compose a bunch of short
--   bytestrings into one big one".
prepInstrs' :: forall arch. [AsmInstr arch] -> BS.ByteString
prepInstrs' =
      BS.intercalate ";"
    . map (Text.Short.toByteString . getAsmInstr)

assemble'
    :: MonadIO m
    => Keystone.Architecture -> [Keystone.Mode]
    -> [String]
    -> m (Either String BS.ByteString)
assemble' arch modes instrs = do
    let as' = Keystone.open arch modes
    liftIO (Keystone.runAssembler as') >>= \case
      Left  e  -> err $ "failed to obtain assembler: "<>show e
      Right as -> do
        let out' = Keystone.assemble as instrs Nothing
        -- TODO have to inspect engine to find error. probably say if x=1 OK, if
        -- x>1 weird error, if x<1 check errno->strerror
        liftIO (Keystone.runAssembler out') >>= \case
          Left e -> err $ "error while assembling: "<>show e
          Right (mc, _count) -> return $ Right mc
  where err = return . Left
