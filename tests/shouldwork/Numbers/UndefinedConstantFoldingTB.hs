module UndefinedConstantFoldingTB where
import Clash.Prelude
import Clash.Explicit.Testbench
import qualified UndefinedConstantFolding

instance ShowX Ordering

expected = $(lift UndefinedConstantFolding.topEntity) :> Nil

topEntity = UndefinedConstantFolding.topEntity

testBench :: Signal System Bool
testBench = done
  where
    expectedOutput = outputVerifier' clk rst expected
    done           = expectedOutput (pure topEntity)
    clk            = tbSystemClockGen (not <$> done)
    rst            = systemResetGen
