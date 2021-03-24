{-# OPTIONS_GHC -fconstraint-solver-iterations=0 #-}
{-# OPTIONS_GHC -freverse-errors #-}
{-# LANGUAGE PartialTypeSignatures #-}

module DualBlockRam1 where

import qualified Prelude as P

import Clash.Explicit.Prelude
import Clash.Explicit.BlockRam
import Clash.Explicit.Testbench
import Data.Bifunctor (bimap)
import Clash.Sized.Internal.BitVector
import Test.Tasty.Clash.CollectSimResults
import DualBlockRamDefinitions
import DualBlockRamTypes

runTest = sampleN 330 testBench
testBench = strictAnd <$> doneA <*> (unsafeSynchronizer clk7 clk10 doneB)
  where
    -- Template haskell simulation
    processSimOutput x = replace 0 undefined# $ tail x
    simOutA = processSimOutput $(collectSimResults 322 $ pack <$> (fst simEntityBC))
    simOutB = processSimOutput $(collectSimResults 226 $ pack <$> (snd simEntityBC))

    -- topEntity output
    (portA, portB) = topOut clk10 rst10 clk7 rst7
    actualOutputA = ignoreFor clk10 rst10 enableGen d1 (unpack $ head simOutA) portA
    actualOutputB = ignoreFor clk7 rst7 enableGen d1 (unpack $ head simOutB) portB

    -- Verification
    outputVerifierA = outputVerifierWith
     (\clk rst -> assertBitVector clk rst "outputVerifierBitVector Port A")
    outoutVerifierB = outputVerifierWith
     (\clk rst -> assertBitVector clk rst "outputVerifierBitVector Port B")

    doneA  = outputVerifierA clk10 rst10 simOutA $ pack <$> portA
    doneA' = not <$> doneA
    doneB  = outoutVerifierB clk7 rst7 simOutB $ pack <$> portB
    doneB' = not <$> doneB

    -- Testbench clocks
    clk10 :: Clock B
    clk10 = tbClockGen @B doneA'
    clk7 :: Clock C
    clk7 = tbClockGen @C doneB'
