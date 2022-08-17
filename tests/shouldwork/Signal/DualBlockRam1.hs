{-# OPTIONS_GHC -Wno-missing-signatures -Wno-partial-type-signatures #-}
{-# OPTIONS_GHC -freverse-errors #-}

module DualBlockRam1 where

import Clash.Explicit.Prelude
import Clash.Explicit.Testbench

import Test.Tasty.Clash.CollectSimResults

import DualBlockRamDefinitions
import DualBlockRamTypes

topEntity :: TdpRam B C
topEntity = tdpRam
{-#NOINLINE topEntity #-}

testBench = strictAnd <$> doneA <*> (unsafeSynchronizer clk7 clk10 doneB)
  where
    -- Template haskell simulation
    simOutA = $(collectSimResults (length opsA) $ pack <$> (fst simEntityBC))
    simOutB =
      $(collectSimResults (length opsB * 14 `div` 10) $
          pack <$> (snd simEntityBC))

    -- topEntity output
    (portA, portB) = topOut topEntity clk10 noRst10 clk7 noRst7

    -- Verification
    outputVerifierA = outputVerifierWith
     (\clk rst -> assertBitVector clk rst "outputVerifierBitVector Port A")
    outoutVerifierB = outputVerifierWith
     (\clk rst -> assertBitVector clk rst "outputVerifierBitVector Port B")

    doneA  = outputVerifierA clk10 clk10 noRst10 simOutA $ pack <$> portA
    doneA' = not <$> doneA
    doneB  = outoutVerifierB clk7 clk7 noRst7 simOutB $ pack <$> portB
    doneB' = not <$> doneB

    -- Testbench clocks
    clk10 :: Clock B
    clk10 = tbClockGen @B doneA'
    clk7 :: Clock C
    clk7 = tbClockGen @C doneB'
