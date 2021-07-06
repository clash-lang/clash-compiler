module RomNegative where

import Clash.Prelude
import qualified Clash.Explicit.Prelude as Explicit
import Clash.Explicit.Testbench

topEntity
  :: Clock System
  -> Reset System
  -> Enable System
  -> Signal System (Signed 64)
  -> Signal System (Unsigned 8)
topEntity = exposeClockResetEnable go
 where
  go rd = mux ((< 0) <$> rd) 0 (unpack <$> romFile d256 "memory.list" rd)
{-# NOINLINE topEntity #-}

testBench :: Signal System Bool
testBench = done
  where
    testInput      = Explicit.register clk rst enableGen minBound
                       (testInput + 1)
    expectedOutput = outputVerifier' clk rst $ replicate d10 0
    done           = expectedOutput . ignoreFor clk rst enableGen d1 0 $
                       topEntity clk rst enableGen testInput
    clk            = tbSystemClockGen (not <$> done)
    rst            = systemResetGen
