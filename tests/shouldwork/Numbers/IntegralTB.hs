module IntegralTB where
import Clash.Prelude
import Clash.Explicit.Testbench
import qualified Integral

instance ShowX Ordering

expected = $(lift $ map Integral.topEntity $ Integral.inputs)

topEntity = Integral.topEntity
{-# NOINLINE topEntity #-}

testBench :: Signal System Bool
testBench = done
  where
    testInput      = stimuliGenerator clk rst Integral.inputs
    expectedOutput = outputVerifier' clk rst expected
    done           = expectedOutput (topEntity <$> testInput)
    clk            = tbSystemClockGen (not <$> done)
    rst            = systemResetGen
