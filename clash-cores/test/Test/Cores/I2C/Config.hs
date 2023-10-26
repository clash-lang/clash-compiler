{-# LANGUAGE CPP #-}

module Test.Cores.I2C.Config where

import Clash.Prelude
import Clash.Explicit.SimIO

data ConfStateMachine = CONFena  |
                         CONFaddr | CONFaddrAck |
                         CONFreg  | CONFregAck  |
                         CONFdata | CONFdataAck |
                         CONFstop
  deriving Show

data ConfS = ConfS { i2cConfStateM :: ConfStateMachine
                   , i2cClaim      :: Bool
                   , i2cWrite      :: Bool
                   , i2cDin        :: Vec 8 Bit
                   , i2cLutIndex   :: Index 16
                   , i2cFault      :: Bool
                   }

type ConfI = (Bool,Bool,Bool,Bool,Bool)
type ConfO = (Bool,Bool,BitVector 8,Bool,Bool)

confInit :: ConfS
confInit = ConfS { i2cConfStateM = CONFena
                 , i2cClaim      = False
                 , i2cWrite      = False
                 , i2cDin        = repeat low
                 , i2cLutIndex   = 0
                 , i2cFault      = False
                 }

configT
  :: Reg ConfS
  -> ConfI
  -> SimIO ConfO
configT s0 (rst,ena,cmdAck,rxAck,al) = do
  s <- readReg s0
  let ConfS confStateM claim write din lutIndex fault = s

  let i2cSlvAddr = 0x34 :: BitVector 8


  let success = cmdAck && not al
      done    = lutIndex == 11

  let lutData = configLut lutIndex

  sNext <- if rst then pure confInit else case confStateM of
    CONFena
      | ena && not done
      -> pure s { i2cConfStateM = CONFaddr }
      | done
      -> do display "done"
            finish 0

    CONFaddr
      -> pure s { i2cConfStateM = CONFaddrAck
                , i2cClaim = True
                , i2cWrite = True
                , i2cDin   = unpack i2cSlvAddr
                }

    CONFaddrAck
      | success
      -> do display "CONFaddrAck"
            pure s { i2cConfStateM = CONFreg
                   , i2cWrite = False
                   }

    CONFreg
      -> if not rxAck then do
           display "Success CONFreg"
           pure s { i2cConfStateM = CONFregAck
                  , i2cWrite = True
                  , i2cDin   = unpack (fst lutData)
                  , i2cFault = False
                  }
         else do
           display "Failure CONFreg"
           _ <- finish 1
           pure s { i2cConfStateM = CONFena
                  , i2cFault = True
                  }

    CONFregAck
      | success
      -> do display "CONFregAck"
            pure s { i2cConfStateM = CONFdata
                   , i2cWrite = False
                   }

    CONFdata
      -> if not rxAck then do
           display "Success CONFdata"
           pure s { i2cConfStateM = CONFdataAck
                  , i2cWrite = True
                  , i2cClaim = False
                  , i2cDin = unpack (snd lutData)
                  , i2cFault = False
                  }
         else do
           display "Failure CONFdata"
           _ <- finish 1
           pure s { i2cConfStateM = CONFena
                  , i2cFault = True
                  }

    CONFdataAck
      | success
      -> do display "CONFdataAck"
            pure s { i2cConfStateM = CONFstop
                   , i2cWrite = False
                   }

    CONFstop
      -> if not rxAck then do
           display "Success CONFstop"
           pure s { i2cConfStateM = CONFena
                  , i2cLutIndex = lutIndex + 1
                  , i2cFault = False
                  }
         else do
           display "Failure CONFstop"
           _ <- finish 1
           pure s { i2cConfStateM = CONFena
                  , i2cFault = True
                  }

    _ -> pure s

  writeReg s0 sNext
  pure (claim,write,pack din,done,fault)

configLut :: Index 16 -> (BitVector  8, BitVector 8)
configLut i
  | i > 10 =
          (0x1E, 0b00000000)
  | otherwise = lut !! i
  where
    lut = (0x1E, 0b00000000) :>
          (0x00, 0b00011111) :>
          (0x02, 0b00011111) :>
          (0x04, 0b11111001) :>
          (0x06, 0b11111001) :>
          (0x08, 0b00010010) :>
          (0x0A, 0b00000110) :>
          (0x0C, 0b00000000) :>
          (0x0E, 0b01001010) :>
          (0x10, 0b00000001) :>
          (0x12, 0b00000001) :>
          Nil

{-# ANN config Synthesize { t_name = "configi2c", t_inputs = [], t_output = PortName "" } #-}
config
  :: Clock System
  -> Signal System ConfI
  -> Signal System ConfO
config clk = mealyIO clk configT (reg confInit)
-- See: https://github.com/clash-lang/clash-compiler/pull/2511
{-# CLASH_OPAQUE config #-}
