{-# LANGUAGE TemplateHaskell #-}

module ExpWithClashCF where

import Clash.Prelude
import Clash.Explicit.Testbench

import qualified Exp
import qualified ConstantFoldingUtil as CFU


-- Constant folded topEntity (GHC/TemplateHaskell)
expected = $(lift (pack Exp.packedExpectedOutputs))

-- Constant folded (?) topEntity (Clash)
topEntity = pack Exp.packedExpectedOutputs -- map Exp.topEntity Exp.testInput

testBench :: Signal System Bool
testBench = done
  where
    expectedOutput = outputVerifierBitVector' clk rst (expected :> Nil)
    done           = expectedOutput (pure topEntity)
    clk            = tbSystemClockGen (not <$> done)
    rst            = systemResetGen


mainVHDL = CFU.mainVHDL
mainVerilog = CFU.mainVHDL
mainSystemVerilog = CFU.mainVHDL
