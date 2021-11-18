module I2Ctest where

import qualified Data.List as L

import Clash.Explicit.Prelude
import I2C

import I2Ctest.I2CSlave
import I2Ctest.I2CConfig

system0 :: Clock System -> Reset System -> Signal System (Vec 16 (Unsigned 8), Bool, Bool)
system0 clk arst = bundle (regFile,done,fault)
 where
  (dout,hostAck,busy,al,ackOut,i2cO) =
    i2c clk arst rst (pure True) (pure 19) start stop (pure False) write (pure True) din i2cI

  (start,stop,write,din,done,fault) = unbundle $
    config clk (bundle (rst, fmap not rst,hostAck,ackOut,al))

  (_,sclOen,_,sdaOen) = unbundle i2cO
  scl  = fmap bitCoerce sclOen
  i2cI = bundle (scl,sdaS)

  (sdaS,regFile) = unbundle
    (i2cSlave clk (bundle (scl, bitCoerce <$> sdaOen)))

  rst = liftA2 (<) rstCounter 500
  rstCounter = register clk arst enableGen (0 :: Unsigned 18) (rstCounter + 1)
{-# NOINLINE system0 #-}

{-# ANN system Synthesize { t_name = "system", t_inputs = [], t_output = PortName "" } #-}
system = system0 systemClockGen resetGen

systemResult = L.last (sampleN 200050 system)
