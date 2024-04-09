{-# LANGUAGE CPP #-}

module Test.Cores.I2C.Slave where

import Clash.Prelude
import Clash.Explicit.SimIO
import Control.Monad (when)

data ACConfTestS = ACCTS { i2cSlaveRegFile  :: Vec 16 (Unsigned 8)
                         , i2cSlaveAddr     :: Vec 8 Bit
                         , i2cSlaveCntr     :: Int
                         , i2cSlaveAtStateM :: AudioTestSM
                         , i2cSlavePrevSCL  :: Bit
                         , i2cSlavePrevSDA  :: Bit
                         , i2cSlaveSdaOut   :: Bit
                         , i2cSlaveRegAddr  :: Unsigned 8
                         , i2cSlaveDebug    :: Bool
                         }

data AudioTestSM = ATidle | ATaddr | ATaddrAck | ATreg | ATregAck | ATval | ATvalAck | ATstop
  deriving Show

type ACConfTestI = (Bit,Bit)
type ACConfTestO = (Bit,Vec 16 (Unsigned 8))

i2cSlaveInit :: ACConfTestS
i2cSlaveInit = ACCTS { i2cSlaveRegFile  = replicate d16 0x0
                     , i2cSlaveAddr     = replicate d8 0
                     , i2cSlaveCntr     = 0
                     , i2cSlaveAtStateM = ATidle
                     , i2cSlavePrevSCL  = high
                     , i2cSlavePrevSDA  = high
                     , i2cSlaveSdaOut   = high
                     , i2cSlaveRegAddr  = 0
                     , i2cSlaveDebug    = False
                     }

i2cSlaveT :: Reg ACConfTestS -> ACConfTestI -> SimIO ACConfTestO
i2cSlaveT s0 (scl,sda) = do
  s <- readReg s0

  let ACCTS regFile addr cntr atStateM prevSCL prevSDA sdaOut regAddr debug = s

  let startCondition = (prevSDA == high && sda == low) && scl == high
      stopCondition  = (prevSDA == low && sda == high) && scl == high

      sclRising    = prevSCL == low && scl == high
      validAddr    = pack addr == 0x34
      validRegAddr = (pack addr >= 0 || pack addr <= 1) && lsb addr == low

  stateMachine <- case atStateM of
    ATidle
      | startCondition -> do when debug $ display "start"
                             pure s {i2cSlaveAtStateM = ATaddr}
    ATaddr
      | cntr == 8 -> if validAddr then do
                       when debug $ display "valid addr"
                       pure s { i2cSlaveAtStateM = ATaddrAck
                              , i2cSlaveAddr     = repeat low
                              , i2cSlaveCntr     = 0
                              }
                     else do
                       when debug $ display $ "invalid addr: " <> show addr
                       pure s { i2cSlaveAtStateM = ATidle
                              , i2cSlaveAddr     = repeat low
                              , i2cSlaveCntr     = 0
                              }
      | sclRising ->   pure s { i2cSlaveAddr   = addr <<+ sda
                              , i2cSlaveCntr   = cntr + 1
                              , i2cSlaveSdaOut = high
                              }
    ATaddrAck
      | sclRising -> do when debug $ display "addrAck"
                        pure s { i2cSlaveAtStateM = ATreg
                               , i2cSlaveSdaOut   = low
                               }
    ATreg
      | cntr == 8 -> if validRegAddr then do
                       when debug $ display "valid reg addr"
                       pure s { i2cSlaveAtStateM = ATregAck
                              , i2cSlaveAddr     = repeat low
                              , i2cSlaveCntr     = 0
                              , i2cSlaveRegAddr  = shiftR (bitCoerce addr) 1
                              }
                     else do
                       when debug $ display $ "invalid reg addr: " <> show addr
                       pure s { i2cSlaveAtStateM = ATidle
                              , i2cSlaveAddr     = repeat low
                              , i2cSlaveCntr     = 0
                              }
      | sclRising -> pure s { i2cSlaveAddr   = addr <<+ sda
                            , i2cSlaveCntr   = cntr + 1
                            , i2cSlaveSdaOut = high
                            }
    ATregAck
      | sclRising -> do when debug $ display "regAck"
                        pure s { i2cSlaveAtStateM = ATval
                               , i2cSlaveSdaOut   = low
                               }
    ATval
      | cntr == 8 -> do when debug $ display "val"
                        pure s { i2cSlaveAtStateM = ATvalAck
                               , i2cSlaveAddr     = repeat low
                               , i2cSlaveCntr     = 0
                               , i2cSlaveRegFile  =
                                 replace regAddr (bitCoerce addr) regFile
                               }
      | sclRising -> pure s { i2cSlaveAddr   = addr <<+ sda
                            , i2cSlaveCntr   = cntr + 1
                            , i2cSlaveSdaOut = high
                            }
    ATvalAck
      | sclRising -> do when debug $ display "valAck"
                        pure s { i2cSlaveAtStateM = ATstop
                               , i2cSlaveSdaOut   = low
                               }
    ATstop
      | stopCondition -> do when debug $ display "stop"
                            pure s { i2cSlaveAtStateM = ATidle
                                   , i2cSlaveSdaOut   = high
                                   }
    _ -> pure s

  writeReg s0 (stateMachine {i2cSlavePrevSDA = sda, i2cSlavePrevSCL = scl})
  pure (sdaOut, regFile)

{-# ANN i2cSlave Synthesize { t_name = "slave", t_inputs = [], t_output = PortName "" } #-}
i2cSlave
  :: Clock System
  -> Signal System ACConfTestI
  -> Signal System ACConfTestO
i2cSlave clk = mealyIO clk i2cSlaveT (reg i2cSlaveInit)
-- See: https://github.com/clash-lang/clash-compiler/pull/2511
{-# CLASH_OPAQUE i2cSlave #-}
