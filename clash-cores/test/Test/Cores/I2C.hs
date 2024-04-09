{-# LANGUAGE CPP #-}

module Test.Cores.I2C where

import qualified Data.List as L

import Clash.Explicit.Prelude
import Clash.Cores.I2C

import Data.Maybe
import Test.Cores.I2C.Config
import Test.Cores.I2C.Slave
import Test.Tasty
import Test.Tasty.HUnit


system0 :: Clock System -> Reset System -> Signal System (Vec 16 (Unsigned 8), Bool, Bool)
system0 clk arst = bundle (registerFile,i2cDone <$> confO,i2cFault <$> confO)
 where
  (_dout,hostAck,_busy,al,ackOut,i2cO) =
    i2c clk arst rst (pure True) (pure 19) (i2cClaim <$> confO) (i2cOp <$> confO) (pure True) i2cI

  confO = config clk $ ConfI <$> rst <*> fmap not rst <*> hostAck <*> ackOut <*> al

  (sclOut,sdaOut) = unbundle i2cO
  scl  = fmap (bitCoerce . isNothing) sclOut
  sda  = fmap (bitCoerce . isNothing) sdaOut
  i2cI = bundle (scl,sdaS)

  (sdaS,registerFile) = unbundle
    (i2cSlave clk (bundle (scl, sda)))

  rst = liftA2 (<) rstCounter 500
  rstCounter = register clk arst enableGen (0 :: Unsigned 18) (rstCounter + 1)
-- See: https://github.com/clash-lang/clash-compiler/pull/2511
{-# CLASH_OPAQUE system0 #-}

{-# ANN system (defSyn "system") #-}
system :: Signal System (Vec 16 (Unsigned 8), Bool, Bool)
system = system0 systemClockGen resetGen

systemResult :: (Vec 16 (Unsigned 8), Bool, Bool)
systemResult = L.last (sampleN 200050 system)

i2cTest :: TestTree
i2cTest =
  testCase "I2C" $
    assertBool "I2C core test procedure failed" (not fault)
 where
  fault =
    any (\(_,_,f) -> f) (takeWhile (\ (_, done, _) -> not done) $ sample system)
