{-# LANGUAGE CPP #-}

module Test.Cores.I2C.Config where

import Clash.Prelude
import Clash.Explicit.SimIO
import Numeric (showHex)

import Clash.Cores.I2C.ByteMaster (I2COperation(..))

data ConfStateMachine = CONFena  |
                        CONFaddr | CONFaddrAck |
                        CONFreg  | CONFregAck  |
                        CONFdata | CONFdataAck |
                        CONFstop
  deriving Show

data ConfS = ConfS { i2cConfStateM   :: ConfStateMachine
                   , i2cConfClaim    :: Bool
                   , i2cConfOp       :: Maybe I2COperation
                   , i2cConfLutIndex :: Index 16
                   , i2cConfFault    :: Bool
                   }

data ConfI = ConfI { i2cRst    :: Bool
                   , i2cEna    :: Bool
                   , i2cCmdAck :: Bool
                   , i2cRxAck  :: Bool
                   , i2cAl     :: Bool
                   }

data ConfO = ConfO { i2cClaim :: Bool
                   , i2cOp    :: Maybe I2COperation
                   , i2cDone  :: Bool
                   , i2cFault :: Bool
                   }

confInit :: ConfS
confInit = ConfS { i2cConfStateM   = CONFena
                 , i2cConfClaim    = False
                 , i2cConfOp       = Nothing
                 , i2cConfLutIndex = 0
                 , i2cConfFault    = False
                 }

configT
  :: Reg ConfS
  -> ConfI
  -> SimIO ConfO
configT s0 ConfI{i2cRst=rst,i2cEna=ena,i2cCmdAck=cmdAck,i2cRxAck=rxAck,i2cAl=al} = do
  s <- readReg s0
  let ConfS confStateM claim op lutIndex fault = s

  let i2cSlvAddr = 0x34 :: BitVector 8

  let success = cmdAck && not al
      done    = lutIndex == 11

  let lutData = configLut lutIndex

  sNext <- if rst then pure confInit else case confStateM of
    CONFena
      | ena && not done
      -> pure s { i2cConfStateM = CONFaddr
                , i2cConfClaim  = True
                }
      | done
      -> do display "done"
            pure s

    CONFaddr
      -> do
        display $ "CONFaddr, writing: " <> showHex i2cSlvAddr ""
        pure s { i2cConfStateM = CONFaddrAck
               , i2cConfOp     = Just (WriteData (unpack i2cSlvAddr))
               }

    CONFaddrAck
      | success
      -> if rxAck then do
           display "CONFaddrAck"
           pure s { i2cConfStateM = CONFreg
                  , i2cConfOp     = Nothing
                  }
         else do
            display "Failure CONFaddr"
            pure s { i2cConfStateM = CONFena
                   , i2cConfFault  = True
                   }

    CONFreg
      -> do
        display $
          "CONFreg, writing: " <> showHex (fst lutData) "" <>
          ", lutIndex: " <> show lutIndex
        pure s { i2cConfStateM = CONFregAck
               , i2cConfOp     = Just (WriteData (unpack (fst lutData)))
               }
    CONFregAck
      | success
      -> if rxAck then do
           display "Success CONFreg"
           pure s { i2cConfStateM = CONFdata
                  , i2cConfOp     = Nothing
                  }
         else do
           display "Failure CONFreg"
           pure s { i2cConfStateM = CONFena
                  , i2cConfFault  = True
                  }

    CONFdata
      -> do display $ "CONFdata, writing: " <> showHex (snd lutData) ""
            pure s { i2cConfStateM = CONFdataAck
                   , i2cConfOp     = Just (WriteData (unpack (snd lutData)))
                   }
    CONFdataAck
      | success
      -> if rxAck then do
           display "Success CONFdata"
           pure s { i2cConfStateM = CONFstop
                  , i2cConfOp     = Nothing
                  }
         else do
           display "Failure CONFdata"
           pure s { i2cConfStateM = CONFena
                  , i2cConfFault  = True
                  }

    CONFstop
      -> do
           display "Success CONFstop"
           pure s { i2cConfStateM   = CONFena
                  , i2cConfClaim    = False
                  , i2cConfLutIndex = lutIndex + 1
                  }

    _ -> pure s

  writeReg s0 sNext
  pure $ ConfO claim op done fault

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
