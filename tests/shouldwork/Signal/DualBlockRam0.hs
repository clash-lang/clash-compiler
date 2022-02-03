{-# OPTIONS_GHC -fconstraint-solver-iterations=0 #-}

module DualBlockRam0 where

import qualified Prelude as P

import Clash.Explicit.Prelude
import Clash.Explicit.BlockRam
import Clash.Explicit.Testbench
import Data.Bifunctor (bimap)
import Clash.Sized.Internal.BitVector
import DualBlockRamDefinitions
import DualBlockRamTypes

import Test.Tasty.Clash.CollectSimResults

runTest = sampleN 250 testBench
testBench = strictAnd <$> doneA <*> (unsafeSynchronizer clk10 clk20 doneB)
  where
    --Template haskell simulation
    processSimOutput x = replace 0 undefined# $ tail x
    simOutA = processSimOutput $(collectSimResults 250 $ pack <$> (fst simEntityAB))
    simOutB = processSimOutput $(collectSimResults 125 $ pack <$> (snd simEntityAB))

    --topEntity output
    (portA, portB) = topOut clk20 rst20 clk10 rst10
    actualOutputA = ignoreFor clk20 rst20 enableGen d1 (unpack $ head simOutA) portA
    actualOutputB = ignoreFor clk10 rst10 enableGen d1 (unpack $ head simOutB) portB

    --Verification
    outputVerifierA = outputVerifierWith
     (\clk rst -> assertBitVector clk rst "outputVerifierBitVector Port A")
    outoutVerifierB = outputVerifierWith
     (\clk rst -> assertBitVector clk rst "outputVerifierBitVector Port B")

    doneA  = outputVerifierA clk20 rst20 simOutA $ pack <$> actualOutputA
    doneA' = not <$> doneA
    doneB  = outoutVerifierB clk10 rst10 simOutB $ pack <$> actualOutputB
    doneB' = not <$> doneB

    -- Testbench clocks
    clk20 :: Clock A
    clk20 = tbClockGen @A doneA'
    clk10 :: Clock B
    clk10 = tbClockGen @B doneB'
