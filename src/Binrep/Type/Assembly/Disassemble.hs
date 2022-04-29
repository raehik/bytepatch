module Binrep.Type.Assembly.Disassemble where

import Binrep.Type.Assembly

import Numeric.Natural ( Natural )

class Disassemble arch where
    disassemble :: Natural -> (MachineCode arch) -> Either String [AsmInstr arch]
