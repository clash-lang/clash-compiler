{-# LANGUAGE CPP #-}

module T2069 where

import Clash.Explicit.BlockRam
import Clash.Explicit.Prelude
import Clash.Explicit.Testbench

topEntity
  :: Clock System
  -> Clock System
  -> Signal System (RamOp 1 (Unsigned 8))
  -> Signal System (RamOp 1 (Unsigned 8))
  -> (Signal System (Unsigned 8), Signal System (Unsigned 8))
topEntity = trueDualPortBlockRam
-- See: https://github.com/clash-lang/clash-compiler/pull/2511
{-# CLASH_OPAQUE topEntity #-}

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
-- See: https://github.com/clash-lang/clash-compiler/pull/2511
{-# CLASH_OPAQUE testBench #-}
