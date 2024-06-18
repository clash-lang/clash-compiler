{-# LANGUAGE TemplateHaskell #-}

module NumConstantFoldingTB_1 where
import Clash.Prelude
import Clash.Explicit.Testbench
import qualified NumConstantFolding_1

expected = $(lift NumConstantFolding_1.topEntity) :> Nil

topEntity = NumConstantFolding_1.topEntity

testBench :: Signal System Bool
testBench = done
  where
    expectedOutput = outputVerifier' clk rst expected
    done           = expectedOutput (pure topEntity)
    clk            = tbSystemClockGen (not <$> done)
    rst            = systemResetGen
