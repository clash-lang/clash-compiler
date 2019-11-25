module I2Ctest.I2CConfig where

import Clash.Prelude
import Clash.Explicit.SimIO

data ConfStateMachine = CONFena  |
                         CONFaddr | CONFaddrAck |
                         CONFreg  | CONFregAck  |
                         CONFdata | CONFdataAck |
                         CONFstop
  deriving Show

data ConfS = ConfS { confStateM :: ConfStateMachine
                   , start      :: Bool
                   , stop       :: Bool
                   , write      :: Bool
                   , din        :: Vec 8 Bit
                   , lutIndex   :: Index 16
                   , fault      :: Bool
                   }

type ConfI = (Bool,Bool,Bool,Bool,Bool)
type ConfO = (Bool,Bool,Bool,BitVector 8,Bool,Bool)

confInit :: ConfS
confInit = ConfS { confStateM = CONFena
                 , start      = False
                 , stop       = False
                 , write      = False
                 , din        = repeat low
                 , lutIndex   = 0
                 , fault      = False
                 }

configT
  :: Reg ConfS
  -> ConfI
  -> SimIO ConfO
configT s0 (rst,ena,cmdAck,rxAck,al) = do
  s <- readReg s0
  let ConfS confStateM start stop write din lutIndex fault = s

  let i2cSlvAddr = 0x34 :: BitVector 8


  let success = cmdAck && not al
      done    = lutIndex == 11

  let lutData = configLut lutIndex

  sNext <- if rst then pure confInit else case confStateM of
    CONFena
      | ena && not done
      -> pure s { confStateM = CONFaddr }
      | done
      -> do display "done"
            finish 0

    CONFaddr
      -> pure s { confStateM = CONFaddrAck
                , start = True
                , write = True
                , din   = unpack i2cSlvAddr
                }

    CONFaddrAck
      | success
      -> do display "CONFaddrAck"
            pure s { confStateM = CONFreg
                   , start = False
                   , write = False
                   }

    CONFreg
      -> if rxAck == False then do
           display "Success CONFreg"
           pure s { confStateM = CONFregAck
                  , write = True
                  , din   = unpack (fst lutData)
                  , fault = False
                  }
         else do
           display "Failure CONFreg"
           finish 1
           pure s { confStateM = CONFena
                  , fault = True
                  }

    CONFregAck
      | success
      -> do display "CONFregAck"
            pure s { confStateM = CONFdata
                   , write = False
                   }

    CONFdata
      -> if rxAck == False then do
           display "Success CONFdata"
           pure s { confStateM = CONFdataAck
                  , write = True
                  , stop = True
                  , din = unpack (snd lutData)
                  , fault = False
                  }
         else do
           display "Failure CONFdata"
           finish 1
           pure s { confStateM = CONFena
                  , fault = True
                  }

    CONFdataAck
      | success
      -> do display "CONFdataAck"
            pure s { confStateM = CONFstop
                   , stop = False
                   , write = False
                   }

    CONFstop
      -> if rxAck == False then do
           display "Success CONFstop"
           pure s { confStateM = CONFena
                  , lutIndex = lutIndex + 1
                  , fault = False
                  }
         else do
           display "Failure CONFdata"
           finish 1
           pure s { confStateM = CONFena
                  , fault = True
                  }

    _ -> pure s

  writeReg s0 sNext
  pure (start,stop,write,pack din,done,fault)

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
{-# NOINLINE config #-}
