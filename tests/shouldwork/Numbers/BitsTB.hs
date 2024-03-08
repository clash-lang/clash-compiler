{-# LANGUAGE CPP #-}

module BitsTB where
import Clash.Prelude
import Clash.Explicit.Testbench
import Bits
import Clash.Sized.Vector (unsafeFromList)

expected1 = $(lift $ map topEntity1 $ inputs)
expected2 = $(lift $ map topEntity2 $ inputs)
expected3 = $(lift $ map topEntity3 $ inputs)

topEntity = False

bitsTB1 :: Signal System Bool
bitsTB1 = done
  where
    testInput      = stimuliGenerator clk rst inputs
    expectedOutput = outputVerifier' clk rst expected1
    done           = expectedOutput (topEntity1 <$> testInput)
    clk            = tbSystemClockGen (not <$> done)
    rst            = systemResetGen
{-# ANN bitsTB1 (TestBench 'topEntity) #-}

bitsTB2 :: Signal System Bool
bitsTB2 = done
  where
    testInput      = stimuliGenerator clk rst inputs
    expectedOutput = outputVerifier' clk rst expected2
    done           = expectedOutput (topEntity2 <$> testInput)
    clk            = tbSystemClockGen (not <$> done)
    rst            = systemResetGen
{-# ANN bitsTB2 (TestBench 'topEntity) #-}

bitsTB3 :: Signal System Bool
bitsTB3 = done
  where
    testInput      = stimuliGenerator clk rst inputs
    expectedOutput = outputVerifier' clk rst expected3
    done           = expectedOutput (topEntity3 <$> testInput)
    clk            = tbSystemClockGen (not <$> done)
    rst            = systemResetGen
{-# ANN bitsTB3 (TestBench 'topEntity) #-}
