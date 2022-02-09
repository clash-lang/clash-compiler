{-# OPTIONS_GHC -Wno-missing-signatures #-}

module DualBlockRam0 where

import Clash.Explicit.Prelude
import Clash.Explicit.Testbench

import Test.Tasty.Clash.CollectSimResults

import DualBlockRamDefinitions
import DualBlockRamTypes

topEntity :: TdpRam A B
topEntity = tdpRam
{-#NOINLINE topEntity #-}

testBench = strictAnd <$> doneA <*> (unsafeSynchronizer clk10 clk20 doneB)
  where
    --Template haskell simulation
    simOutA = $(collectSimResults (length opsA) $ pack <$> (fst simEntityAB))
    simOutB = $(collectSimResults (length opsB) $ pack <$> (snd simEntityAB))

    --topEntity output
    (portA, portB) = topOut topEntity clk20 noRst20 clk10 noRst10

    --Verification
    outputVerifierA = outputVerifierWith
     (\clk rst -> assertBitVector clk rst "outputVerifierBitVector Port A")
    outoutVerifierB = outputVerifierWith
     (\clk rst -> assertBitVector clk rst "outputVerifierBitVector Port B")

    doneA  = outputVerifierA clk20 noRst20 simOutA $ pack <$> portA
    doneB  = outoutVerifierB clk10 noRst10 simOutB $ pack <$> portB

    -- Testbench clocks
    clk20 :: Clock A
    clk20 = tbClockGen (not <$> doneA)
    clk10 :: Clock B
    clk10 = tbClockGen (not <$> doneB)
{-# NOINLINE testBench #-}
