module I2C.Types where

import CLaSH.Prelude

data I2CCommand = I2Cstart | I2Cstop | I2Cwrite | I2Cread | I2Cnop
  deriving (Eq, Ord)

type BitCtrlSig = (I2CCommand,Bit)
type BitRespSig = (Bool,Bool,Bit)

type I2CIn      = (Bit,Bit)
type I2COut     = (Bit,Bool,Bit,Bool)
