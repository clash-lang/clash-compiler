module NumConstantFoldingTB_2 where
import Clash.Prelude
import Clash.Explicit.Testbench
import qualified NumConstantFolding_2

expected = $(lift NumConstantFolding_2.topEntity) :> Nil

topEntity = NumConstantFolding_2.topEntity

testBench :: Signal System Bool
testBench = done
  where
    expectedOutput = outputVerifier' clk rst expected
    done           = expectedOutput (pure topEntity)
    clk            = tbSystemClockGen (not <$> done)
    rst            = systemResetGen

