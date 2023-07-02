{-# LANGUAGE CPP #-}

{-# OPTIONS_GHC -freverse-errors #-}

module DualBlockRam where

import Clash.Explicit.Prelude
import Clash.Explicit.Testbench

import Test.Tasty.Clash.CollectSimResults

import DualBlockRamDefinitions
import DualBlockRamTypes

topEntityAB :: TdpRam A B
topEntityAB = tdpRam
-- See: https://github.com/clash-lang/clash-compiler/pull/2511
{-# CLASH_OPAQUE topEntityAB #-}
{-# ANN topEntityAB (defSyn "topEntityAB") #-}

testBenchAB :: Signal A Bool
testBenchAB = strictAnd <$> doneA <*> (unsafeSynchronizer clk10 clk20 doneB)
  where
    --Template haskell simulation
    simOutA = $(collectSimResults (length opsA) $ pack <$> (fst simEntityAB))
    simOutB = $(collectSimResults (length opsB) $ pack <$> (snd simEntityAB))

    --topEntity output
    (portA, portB) = topOut topEntityAB clk20 noRst20 clk10 noRst10

    --Verification
    outputVerifierA = outputVerifierWith
     (\clk rst -> assertBitVector clk rst "outputVerifierBitVector Port A")
    outoutVerifierB = outputVerifierWith
     (\clk rst -> assertBitVector clk rst "outputVerifierBitVector Port B")

    doneA  = outputVerifierA clk20 clk20 noRst20 simOutA $ pack <$> portA
    doneB  = outoutVerifierB clk10 clk10 noRst10 simOutB $ pack <$> portB

    -- Testbench clocks
    clk20 :: Clock A
    clk20 = tbClockGen (not <$> doneA)
    clk10 :: Clock B
    clk10 = tbClockGen (not <$> doneB)
-- See: https://github.com/clash-lang/clash-compiler/pull/2511
{-# CLASH_OPAQUE testBenchAB #-}
{-# ANN testBenchAB (TestBench 'topEntityAB) #-}

topEntityBC :: TdpRam B C
topEntityBC = tdpRam
{-#NOINLINE topEntityBC #-}
{-# ANN topEntityBC (defSyn "topEntityBC") #-}

testBenchBC :: Signal B Bool
testBenchBC = strictAnd <$> doneA <*> (unsafeSynchronizer clk7 clk10 doneB)
  where
    -- Template haskell simulation
    simOutA = $(collectSimResults (length opsA) $ pack <$> (fst simEntityBC))
    simOutB =
      $(collectSimResults (length opsB * 14 `div` 10) $
          pack <$> (snd simEntityBC))

    -- topEntity output
    (portA, portB) = topOut topEntityBC clk10 noRst10 clk7 noRst7

    -- Verification
    outputVerifierA = outputVerifierWith
     (\clk rst -> assertBitVector clk rst "outputVerifierBitVector Port A")
    outoutVerifierB = outputVerifierWith
     (\clk rst -> assertBitVector clk rst "outputVerifierBitVector Port B")

    doneA  = outputVerifierA clk10 clk10 noRst10 simOutA $ pack <$> portA
    doneB  = outoutVerifierB clk7 clk7 noRst7 simOutB $ pack <$> portB

    -- Testbench clocks
    clk10 :: Clock B
    clk10 = tbClockGen (not <$> doneA)
    clk7 :: Clock C
    clk7 = tbClockGen (not <$> doneB)
-- See: https://github.com/clash-lang/clash-compiler/pull/2511
{-# CLASH_OPAQUE testBenchBC #-}
{-# ANN testBenchBC (TestBench 'topEntityBC) #-}
