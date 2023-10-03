{-# LANGUAGE CPP #-}

module Test.Cores.I2C where

import qualified Data.List as L

import Clash.Explicit.Prelude
import Clash.Cores.I2C

import Test.Cores.I2C.Slave
import Test.Cores.I2C.Config

system0 :: Clock System -> Reset System -> Signal System (Vec 16 (Unsigned 8), Bool, Bool)
system0 clk arst = bundle (registerFile,done,fault)
 where
  (_dout,hostAck,_busy,al,ackOut,i2cO) =
    i2c clk arst rst (pure True) (pure 19) start stop (pure False) write (pure True) din i2cI

  (start,stop,write,din,done,fault) = unbundle $
    config clk (bundle (rst, fmap not rst,hostAck,ackOut,al))

  (_,sclOen,_,sdaOen) = unbundle i2cO
  scl  = fmap bitCoerce sclOen
  i2cI = bundle (scl,sdaS)

  (sdaS,registerFile) = unbundle
    (i2cSlave clk (bundle (scl, bitCoerce <$> sdaOen)))

  rst = liftA2 (<) rstCounter 500
  rstCounter = register clk arst enableGen (0 :: Unsigned 18) (rstCounter + 1)
-- See: https://github.com/clash-lang/clash-compiler/pull/2511
{-# CLASH_OPAQUE system0 #-}

{-# ANN system Synthesize { t_name = "system", t_inputs = [], t_output = PortName "" } #-}
system :: Signal System (Vec 16 (Unsigned 8), Bool, Bool)
system = system0 systemClockGen resetGen

systemResult :: (Vec 16 (Unsigned 8), Bool, Bool)
systemResult = L.last (sampleN 200050 system)
