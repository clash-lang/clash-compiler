module NumConstantFoldingTB where
import Clash.Prelude
import Clash.Explicit.Testbench
import qualified NumConstantFolding

instance ShowX Ordering

expected = $(lift NumConstantFolding.topEntity) :> Nil

topEntity = NumConstantFolding.topEntity

testBench :: Signal System Bool
testBench = done
  where
    expectedOutput = outputVerifier clk rst expected
    done           = expectedOutput (pure topEntity)
    clk            = tbSystemClockGen (not <$> done)
    rst            = systemResetGen
