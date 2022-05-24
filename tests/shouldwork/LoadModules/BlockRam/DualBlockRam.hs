{-# OPTIONS_GHC -fconstraint-solver-iterations=0 #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fplugin=GHC.TypeLits.Extra.Solver #-}
{-# OPTIONS_GHC -fplugin=GHC.TypeLits.KnownNat.Solver #-}
{-# OPTIONS_GHC -fplugin=GHC.TypeLits.Normalise #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-# LANGUAGE NoImplicitPrelude #-}
module DualBlockRam where
import Clash.Explicit.Prelude
import Clash.Explicit.BlockRam(WriteMode(..))
import Clash.Explicit.Testbench
import Clash.Sized.Internal.BitVector
import DualBlockRamDefinitions
import Test.Tasty.Clash.CollectSimResults

genTestBench top rstA rstB simOutA simOutB =
 strictAnd <$> doneA <*> (unsafeSynchronizer clkB clkA doneB)
  where
    --topEntity output
    (portA, portB) = top clkA clkB

    --Verification
    outputVerifierA = outputVerifierWith
     (\clk rst -> assertBitVector clk rst "outputVerifierBitVector Port A")
    outoutVerifierB = outputVerifierWith
     (\clk rst -> assertBitVector clk rst "outputVerifierBitVector Port B")

    doneA  = outputVerifierA clkA rstA simOutA $ pack <$> portA
    doneB  = outoutVerifierB clkB rstB simOutB $ pack <$> portB

    (clkA, clkB) = (tbClockGen (not <$> doneA), tbClockGen (not <$> doneB))
    -- Testbench clocks
{-# INLINE genTestBench #-}

-- testBench  configurations
testBench_WF_WF_AB = genTestBench topEntity_WF_WF_AB rst20 rst10 simOutA_WF_WF_AB simOutB_WF_WF_AB
{-# NOINLINE testBench_WF_WF_AB #-}
{-# ANN testBench_WF_WF_AB (TestBench 'topEntity_WF_WF_AB) #-}
testBench_WF_WF_BC = genTestBench topEntity_WF_WF_BC rst10 rst7 simOutA_WF_WF_BC simOutB_WF_WF_BC
{-# NOINLINE testBench_WF_WF_BC #-}
{-# ANN testBench_WF_WF_BC (TestBench 'topEntity_WF_WF_BC) #-}
testBench_WF_RF_AB = genTestBench topEntity_WF_RF_AB rst20 rst10 simOutA_WF_RF_AB simOutB_WF_RF_AB
{-# NOINLINE testBench_WF_RF_AB #-}
{-# ANN testBench_WF_RF_AB (TestBench 'topEntity_WF_RF_AB) #-}
testBench_WF_RF_BC = genTestBench topEntity_WF_RF_BC rst10 rst7 simOutA_WF_RF_BC simOutB_WF_RF_BC
{-# NOINLINE testBench_WF_RF_BC #-}
{-# ANN testBench_WF_RF_BC (TestBench 'topEntity_WF_RF_BC) #-}
testBench_WF_NC_AB = genTestBench topEntity_WF_NC_AB rst20 rst10 simOutA_WF_NC_AB simOutB_WF_NC_AB
{-# NOINLINE testBench_WF_NC_AB #-}
{-# ANN testBench_WF_NC_AB (TestBench 'topEntity_WF_NC_AB) #-}
testBench_WF_NC_BC = genTestBench topEntity_WF_NC_BC rst10 rst7 simOutA_WF_NC_BC simOutB_WF_NC_BC
{-# NOINLINE testBench_WF_NC_BC #-}
{-# ANN testBench_WF_NC_BC (TestBench 'topEntity_WF_NC_BC) #-}
testBench_RF_WF_AB = genTestBench topEntity_RF_WF_AB rst20 rst10 simOutA_RF_WF_AB simOutB_RF_WF_AB
{-# NOINLINE testBench_RF_WF_AB #-}
{-# ANN testBench_RF_WF_AB (TestBench 'topEntity_RF_WF_AB) #-}
testBench_RF_WF_BC = genTestBench topEntity_RF_WF_BC rst10 rst7 simOutA_RF_WF_BC simOutB_RF_WF_BC
{-# NOINLINE testBench_RF_WF_BC #-}
{-# ANN testBench_RF_WF_BC (TestBench 'topEntity_RF_WF_BC) #-}
testBench_RF_RF_AB = genTestBench topEntity_RF_RF_AB rst20 rst10 simOutA_RF_RF_AB simOutB_RF_RF_AB
{-# NOINLINE testBench_RF_RF_AB #-}
{-# ANN testBench_RF_RF_AB (TestBench 'topEntity_RF_RF_AB) #-}
testBench_RF_RF_BC = genTestBench topEntity_RF_RF_BC rst10 rst7 simOutA_RF_RF_BC simOutB_RF_RF_BC
{-# NOINLINE testBench_RF_RF_BC #-}
{-# ANN testBench_RF_RF_BC (TestBench 'topEntity_RF_RF_BC) #-}
testBench_RF_NC_AB = genTestBench topEntity_RF_NC_AB rst20 rst10 simOutA_RF_NC_AB simOutB_RF_NC_AB
{-# NOINLINE testBench_RF_NC_AB #-}
{-# ANN testBench_RF_NC_AB (TestBench 'topEntity_RF_NC_AB) #-}
testBench_RF_NC_BC = genTestBench topEntity_RF_NC_BC rst10 rst7 simOutA_RF_NC_BC simOutB_RF_NC_BC
{-# NOINLINE testBench_RF_NC_BC #-}
{-# ANN testBench_RF_NC_BC (TestBench 'topEntity_RF_NC_BC) #-}
testBench_NC_WF_AB = genTestBench topEntity_NC_WF_AB rst20 rst10 simOutA_NC_WF_AB simOutB_NC_WF_AB
{-# NOINLINE testBench_NC_WF_AB #-}
{-# ANN testBench_NC_WF_AB (TestBench 'topEntity_NC_WF_AB) #-}
testBench_NC_WF_BC = genTestBench topEntity_NC_WF_BC rst10 rst7 simOutA_NC_WF_BC simOutB_NC_WF_BC
{-# NOINLINE testBench_NC_WF_BC #-}
{-# ANN testBench_NC_WF_BC (TestBench 'topEntity_NC_WF_BC) #-}
testBench_NC_RF_AB = genTestBench topEntity_NC_RF_AB rst20 rst10 simOutA_NC_RF_AB simOutB_NC_RF_AB
{-# NOINLINE testBench_NC_RF_AB #-}
{-# ANN testBench_NC_RF_AB (TestBench 'topEntity_NC_RF_AB) #-}
testBench_NC_RF_BC = genTestBench topEntity_NC_RF_BC rst10 rst7 simOutA_NC_RF_BC simOutB_NC_RF_BC
{-# NOINLINE testBench_NC_RF_BC #-}
{-# ANN testBench_NC_RF_BC (TestBench 'topEntity_NC_RF_BC) #-}
testBench_NC_NC_AB = genTestBench topEntity_NC_NC_AB rst20 rst10 simOutA_NC_NC_AB simOutB_NC_NC_AB
{-# NOINLINE testBench_NC_NC_AB #-}
{-# ANN testBench_NC_NC_AB (TestBench 'topEntity_NC_NC_AB) #-}
testBench_NC_NC_BC = genTestBench topEntity_NC_NC_BC rst10 rst7 simOutA_NC_NC_BC simOutB_NC_NC_BC
{-# NOINLINE testBench_NC_NC_BC #-}
{-# ANN testBench_NC_NC_BC (TestBench 'topEntity_NC_NC_BC) #-}

topEntity_WF_WF_AB = topOut WriteFirst WriteFirst rst20 rst10
{-# NOINLINE topEntity_WF_WF_AB #-}
{-# ANN topEntity_WF_WF_AB (defSyn "topEntity_WF_WF_AB") #-}
topEntity_WF_WF_BC = topOut WriteFirst WriteFirst rst10 rst7
{-# NOINLINE topEntity_WF_WF_BC #-}
{-# ANN topEntity_WF_WF_BC (defSyn "topEntity_WF_WF_BC") #-}
topEntity_WF_RF_AB = topOut WriteFirst ReadFirst rst20 rst10
{-# NOINLINE topEntity_WF_RF_AB #-}
{-# ANN topEntity_WF_RF_AB (defSyn "topEntity_WF_RF_AB") #-}
topEntity_WF_RF_BC = topOut WriteFirst ReadFirst rst10 rst7
{-# NOINLINE topEntity_WF_RF_BC #-}
{-# ANN topEntity_WF_RF_BC (defSyn "topEntity_WF_RF_BC") #-}
topEntity_WF_NC_AB = topOut WriteFirst NoChange rst20 rst10
{-# NOINLINE topEntity_WF_NC_AB #-}
{-# ANN topEntity_WF_NC_AB (defSyn "topEntity_WF_NC_AB") #-}
topEntity_WF_NC_BC = topOut WriteFirst NoChange rst10 rst7
{-# NOINLINE topEntity_WF_NC_BC #-}
{-# ANN topEntity_WF_NC_BC (defSyn "topEntity_WF_NC_BC") #-}
topEntity_RF_WF_AB = topOut ReadFirst WriteFirst rst20 rst10
{-# NOINLINE topEntity_RF_WF_AB #-}
{-# ANN topEntity_RF_WF_AB (defSyn "topEntity_RF_WF_AB") #-}
topEntity_RF_WF_BC = topOut ReadFirst WriteFirst rst10 rst7
{-# NOINLINE topEntity_RF_WF_BC #-}
{-# ANN topEntity_RF_WF_BC (defSyn "topEntity_RF_WF_BC") #-}
topEntity_RF_RF_AB = topOut ReadFirst ReadFirst rst20 rst10
{-# NOINLINE topEntity_RF_RF_AB #-}
{-# ANN topEntity_RF_RF_AB (defSyn "topEntity_RF_RF_AB") #-}
topEntity_RF_RF_BC = topOut ReadFirst ReadFirst rst10 rst7
{-# NOINLINE topEntity_RF_RF_BC #-}
{-# ANN topEntity_RF_RF_BC (defSyn "topEntity_RF_RF_BC") #-}
topEntity_RF_NC_AB = topOut ReadFirst NoChange rst20 rst10
{-# NOINLINE topEntity_RF_NC_AB #-}
{-# ANN topEntity_RF_NC_AB (defSyn "topEntity_RF_NC_AB") #-}
topEntity_RF_NC_BC = topOut ReadFirst NoChange rst10 rst7
{-# NOINLINE topEntity_RF_NC_BC #-}
{-# ANN topEntity_RF_NC_BC (defSyn "topEntity_RF_NC_BC") #-}
topEntity_NC_WF_AB = topOut NoChange WriteFirst rst20 rst10
{-# NOINLINE topEntity_NC_WF_AB #-}
{-# ANN topEntity_NC_WF_AB (defSyn "topEntity_NC_WF_AB") #-}
topEntity_NC_WF_BC = topOut NoChange WriteFirst rst10 rst7
{-# NOINLINE topEntity_NC_WF_BC #-}
{-# ANN topEntity_NC_WF_BC (defSyn "topEntity_NC_WF_BC") #-}
topEntity_NC_RF_AB = topOut NoChange ReadFirst rst20 rst10
{-# NOINLINE topEntity_NC_RF_AB #-}
{-# ANN topEntity_NC_RF_AB (defSyn "topEntity_NC_RF_AB") #-}
topEntity_NC_RF_BC = topOut NoChange ReadFirst rst10 rst7
{-# NOINLINE topEntity_NC_RF_BC #-}
{-# ANN topEntity_NC_RF_BC (defSyn "topEntity_NC_RF_BC") #-}
topEntity_NC_NC_AB = topOut NoChange NoChange rst20 rst10
{-# NOINLINE topEntity_NC_NC_AB #-}
{-# ANN topEntity_NC_NC_AB (defSyn "topEntity_NC_NC_AB") #-}
topEntity_NC_NC_BC = topOut NoChange NoChange rst10 rst7
{-# NOINLINE topEntity_NC_NC_BC #-}
{-# ANN topEntity_NC_NC_BC (defSyn "topEntity_NC_NC_BC") #-}

-- Expected output generation for all configurations with Template Haskell
simOutA_WF_WF_AB = $(collectSimResults (length opsA+1) $ pack <$> (fst $ topOut WriteFirst WriteFirst rst20 rst10 clk20 clk10))
{-# INLINE simOutA_WF_WF_AB #-}
simOutA_WF_WF_BC = $(collectSimResults (length opsA+1) $ pack <$> (fst $ topOut WriteFirst WriteFirst rst10 rst7 clk10 clk7))
{-# INLINE simOutA_WF_WF_BC #-}
simOutA_WF_RF_AB = $(collectSimResults (length opsA+1) $ pack <$> (fst $ topOut WriteFirst ReadFirst rst20 rst10 clk20 clk10))
{-# INLINE simOutA_WF_RF_AB #-}
simOutA_WF_RF_BC = $(collectSimResults (length opsA+1) $ pack <$> (fst $ topOut WriteFirst ReadFirst rst10 rst7 clk10 clk7))
{-# INLINE simOutA_WF_RF_BC #-}
simOutA_WF_NC_AB = $(collectSimResults (length opsA+1) $ pack <$> (fst $ topOut WriteFirst NoChange rst20 rst10 clk20 clk10))
{-# INLINE simOutA_WF_NC_AB #-}
simOutA_WF_NC_BC = $(collectSimResults (length opsA+1) $ pack <$> (fst $ topOut WriteFirst NoChange rst10 rst7 clk10 clk7))
{-# INLINE simOutA_WF_NC_BC #-}
simOutA_RF_WF_AB = $(collectSimResults (length opsA+1) $ pack <$> (fst $ topOut ReadFirst WriteFirst rst20 rst10 clk20 clk10))
{-# INLINE simOutA_RF_WF_AB #-}
simOutA_RF_WF_BC = $(collectSimResults (length opsA+1) $ pack <$> (fst $ topOut ReadFirst WriteFirst rst10 rst7 clk10 clk7))
{-# INLINE simOutA_RF_WF_BC #-}
simOutA_RF_RF_AB = $(collectSimResults (length opsA+1) $ pack <$> (fst $ topOut ReadFirst ReadFirst rst20 rst10 clk20 clk10))
{-# INLINE simOutA_RF_RF_AB #-}
simOutA_RF_RF_BC = $(collectSimResults (length opsA+1) $ pack <$> (fst $ topOut ReadFirst ReadFirst rst10 rst7 clk10 clk7))
{-# INLINE simOutA_RF_RF_BC #-}
simOutA_RF_NC_AB = $(collectSimResults (length opsA+1) $ pack <$> (fst $ topOut ReadFirst NoChange rst20 rst10 clk20 clk10))
{-# INLINE simOutA_RF_NC_AB #-}
simOutA_RF_NC_BC = $(collectSimResults (length opsA+1) $ pack <$> (fst $ topOut ReadFirst NoChange rst10 rst7 clk10 clk7))
{-# INLINE simOutA_RF_NC_BC #-}
simOutA_NC_WF_AB = $(collectSimResults (length opsA+1) $ pack <$> (fst $ topOut NoChange WriteFirst rst20 rst10 clk20 clk10))
{-# INLINE simOutA_NC_WF_AB #-}
simOutA_NC_WF_BC = $(collectSimResults (length opsA+1) $ pack <$> (fst $ topOut NoChange WriteFirst rst10 rst7 clk10 clk7))
{-# INLINE simOutA_NC_WF_BC #-}
simOutA_NC_RF_AB = $(collectSimResults (length opsA+1) $ pack <$> (fst $ topOut NoChange ReadFirst rst20 rst10 clk20 clk10))
{-# INLINE simOutA_NC_RF_AB #-}
simOutA_NC_RF_BC = $(collectSimResults (length opsA+1) $ pack <$> (fst $ topOut NoChange ReadFirst rst10 rst7 clk10 clk7))
{-# INLINE simOutA_NC_RF_BC #-}
simOutA_NC_NC_AB = $(collectSimResults (length opsA+1) $ pack <$> (fst $ topOut NoChange NoChange rst20 rst10 clk20 clk10))
{-# INLINE simOutA_NC_NC_AB #-}
simOutA_NC_NC_BC = $(collectSimResults (length opsA+1) $ pack <$> (fst $ topOut NoChange NoChange rst10 rst7 clk10 clk7))
{-# INLINE simOutA_NC_NC_BC #-}
simOutB_WF_WF_AB = $(collectSimResults (length opsB+1) $ pack <$> (snd $ topOut WriteFirst WriteFirst rst20 rst10 clk20 clk10))
{-# INLINE simOutB_WF_WF_AB #-}
simOutB_WF_WF_BC = $(collectSimResults (length opsB+1) $ pack <$> (snd $ topOut WriteFirst WriteFirst rst10 rst7 clk10 clk7))
{-# INLINE simOutB_WF_WF_BC #-}
simOutB_WF_RF_AB = $(collectSimResults (length opsB+1) $ pack <$> (snd $ topOut WriteFirst ReadFirst rst20 rst10 clk20 clk10))
{-# INLINE simOutB_WF_RF_AB #-}
simOutB_WF_RF_BC = $(collectSimResults (length opsB+1) $ pack <$> (snd $ topOut WriteFirst ReadFirst rst10 rst7 clk10 clk7))
{-# INLINE simOutB_WF_RF_BC #-}
simOutB_WF_NC_AB = $(collectSimResults (length opsB+1) $ pack <$> (snd $ topOut WriteFirst NoChange rst20 rst10 clk20 clk10))
{-# INLINE simOutB_WF_NC_AB #-}
simOutB_WF_NC_BC = $(collectSimResults (length opsB+1) $ pack <$> (snd $ topOut WriteFirst NoChange rst10 rst7 clk10 clk7))
{-# INLINE simOutB_WF_NC_BC #-}
simOutB_RF_WF_AB = $(collectSimResults (length opsB+1) $ pack <$> (snd $ topOut ReadFirst WriteFirst rst20 rst10 clk20 clk10))
{-# INLINE simOutB_RF_WF_AB #-}
simOutB_RF_WF_BC = $(collectSimResults (length opsB+1) $ pack <$> (snd $ topOut ReadFirst WriteFirst rst10 rst7 clk10 clk7))
{-# INLINE simOutB_RF_WF_BC #-}
simOutB_RF_RF_AB = $(collectSimResults (length opsB+1) $ pack <$> (snd $ topOut ReadFirst ReadFirst rst20 rst10 clk20 clk10))
{-# INLINE simOutB_RF_RF_AB #-}
simOutB_RF_RF_BC = $(collectSimResults (length opsB+1) $ pack <$> (snd $ topOut ReadFirst ReadFirst rst10 rst7 clk10 clk7))
{-# INLINE simOutB_RF_RF_BC #-}
simOutB_RF_NC_AB = $(collectSimResults (length opsB+1) $ pack <$> (snd $ topOut ReadFirst NoChange rst20 rst10 clk20 clk10))
{-# INLINE simOutB_RF_NC_AB #-}
simOutB_RF_NC_BC = $(collectSimResults (length opsB+1) $ pack <$> (snd $ topOut ReadFirst NoChange rst10 rst7 clk10 clk7))
{-# INLINE simOutB_RF_NC_BC #-}
simOutB_NC_WF_AB = $(collectSimResults (length opsB+1) $ pack <$> (snd $ topOut NoChange WriteFirst rst20 rst10 clk20 clk10))
{-# INLINE simOutB_NC_WF_AB #-}
simOutB_NC_WF_BC = $(collectSimResults (length opsB+1) $ pack <$> (snd $ topOut NoChange WriteFirst rst10 rst7 clk10 clk7))
{-# INLINE simOutB_NC_WF_BC #-}
simOutB_NC_RF_AB = $(collectSimResults (length opsB+1) $ pack <$> (snd $ topOut NoChange ReadFirst rst20 rst10 clk20 clk10))
{-# INLINE simOutB_NC_RF_AB #-}
simOutB_NC_RF_BC = $(collectSimResults (length opsB+1) $ pack <$> (snd $ topOut NoChange ReadFirst rst10 rst7 clk10 clk7))
{-# INLINE simOutB_NC_RF_BC #-}
simOutB_NC_NC_AB = $(collectSimResults (length opsB+1) $ pack <$> (snd $ topOut NoChange NoChange rst20 rst10 clk20 clk10))
{-# INLINE simOutB_NC_NC_AB #-}
simOutB_NC_NC_BC = $(collectSimResults (length opsB+1) $ pack <$> (snd $ topOut NoChange NoChange rst10 rst7 clk10 clk7))
{-# INLINE simOutB_NC_NC_BC #-}
