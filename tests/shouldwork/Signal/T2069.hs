module T2069 where

import Clash.Explicit.BlockRam
import Clash.Explicit.Prelude
import Clash.Explicit.Testbench

-- TODO with different configs
topEntity
  :: Clock System
  -> Clock System
  -> Signal System (RamOp 1 (Unsigned 8))
  -> Signal System (RamOp 1 (Unsigned 8))
  -> (Signal System (Unsigned 8), Signal System (Unsigned 8))
topEntity clkA clkB inpA inpB = trueDualPortBlockRam tdpDefault clkA clkB enableGen enableGen inpA inpB
{-# NOINLINE topEntity #-}


-- TODO why do we still have this test? don't the Dual tests cover everything this one does?
testBench :: Signal System Bool
testBench = done
  where
    testInput = register clk rst en
                  (RamWrite 0 42, RamRead 0) $ pure (RamRead 0, RamRead 0)
    expectedOutput = outputVerifier' clk rst $ (0,0) :> (0, 0) :> (42, 42) :> Nil
    done = expectedOutput $ ignoreFor clk rst en d2 (0, 0) $ bundle $
             uncurry (topEntity clk clk) $ unbundle testInput
    clk = tbSystemClockGen (not <$> done)
    rst = systemResetGen
    en = enableGen
{-# NOINLINE testBench #-}
