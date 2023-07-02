{-# LANGUAGE CPP #-}

module MAC where

import Clash.Prelude
import Clash.Explicit.Testbench

ma acc (x,y) = acc + x * y

macT acc (x,y) = (acc',o)
  where
    acc' = ma acc (x,y)
    o    = acc

mac
  :: SystemClockResetEnable
  => Signal System (Signed 9, Signed 9)
  -> Signal System (Signed 9)
mac = macT `mealy` 0

topEntity
  :: Clock System
  -> Reset System
  -> Enable System
  -> Signal System (Signed 9,Signed 9)
  -> Signal System (Signed 9)
topEntity clk rst xy = r
  where
    r = exposeClockResetEnable mac clk rst xy
-- See: https://github.com/clash-lang/clash-compiler/pull/2511
{-# CLASH_OPAQUE topEntity #-}

testBench :: Signal System Bool
testBench = done
  where
    testInput      = stimuliGenerator clk rst $(listToVecTH [(1,1) :: (Signed 9,Signed 9),(2,2),(3,3),(4,4)])
    expectedOutput = outputVerifier' clk rst $(listToVecTH [0 :: Signed 9,1,5,14])
    done           = expectedOutput (topEntity clk rst (enableGen) testInput)
    clk            = tbSystemClockGen (not <$> done)
    rst            = systemResetGen
