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
testBench_WF_WF_F_F_AB = genTestBench topEntity_WF_WF_F_F_AB rst20 rst10 simOutA_WF_WF_F_F_AB simOutB_WF_WF_F_F_AB
{-# NOINLINE testBench_WF_WF_F_F_AB #-}
{-# ANN testBench_WF_WF_F_F_AB (TestBench 'topEntity_WF_WF_F_F_AB) #-}
testBench_WF_WF_F_T_AB = genTestBench topEntity_WF_WF_F_T_AB rst20 rst10 simOutA_WF_WF_F_T_AB simOutB_WF_WF_F_T_AB
{-# NOINLINE testBench_WF_WF_F_T_AB #-}
{-# ANN testBench_WF_WF_F_T_AB (TestBench 'topEntity_WF_WF_F_T_AB) #-}
testBench_WF_WF_T_F_AB = genTestBench topEntity_WF_WF_T_F_AB rst20 rst10 simOutA_WF_WF_T_F_AB simOutB_WF_WF_T_F_AB
{-# NOINLINE testBench_WF_WF_T_F_AB #-}
{-# ANN testBench_WF_WF_T_F_AB (TestBench 'topEntity_WF_WF_T_F_AB) #-}
testBench_WF_WF_T_T_AB = genTestBench topEntity_WF_WF_T_T_AB rst20 rst10 simOutA_WF_WF_T_T_AB simOutB_WF_WF_T_T_AB
{-# NOINLINE testBench_WF_WF_T_T_AB #-}
{-# ANN testBench_WF_WF_T_T_AB (TestBench 'topEntity_WF_WF_T_T_AB) #-}
testBench_WF_WF_F_F_BC = genTestBench topEntity_WF_WF_F_F_BC rst10 rst7 simOutA_WF_WF_F_F_BC simOutB_WF_WF_F_F_BC
{-# NOINLINE testBench_WF_WF_F_F_BC #-}
{-# ANN testBench_WF_WF_F_F_BC (TestBench 'topEntity_WF_WF_F_F_BC) #-}
testBench_WF_WF_F_T_BC = genTestBench topEntity_WF_WF_F_T_BC rst10 rst7 simOutA_WF_WF_F_T_BC simOutB_WF_WF_F_T_BC
{-# NOINLINE testBench_WF_WF_F_T_BC #-}
{-# ANN testBench_WF_WF_F_T_BC (TestBench 'topEntity_WF_WF_F_T_BC) #-}
testBench_WF_WF_T_F_BC = genTestBench topEntity_WF_WF_T_F_BC rst10 rst7 simOutA_WF_WF_T_F_BC simOutB_WF_WF_T_F_BC
{-# NOINLINE testBench_WF_WF_T_F_BC #-}
{-# ANN testBench_WF_WF_T_F_BC (TestBench 'topEntity_WF_WF_T_F_BC) #-}
testBench_WF_WF_T_T_BC = genTestBench topEntity_WF_WF_T_T_BC rst10 rst7 simOutA_WF_WF_T_T_BC simOutB_WF_WF_T_T_BC
{-# NOINLINE testBench_WF_WF_T_T_BC #-}
{-# ANN testBench_WF_WF_T_T_BC (TestBench 'topEntity_WF_WF_T_T_BC) #-}
testBench_WF_RF_F_F_AB = genTestBench topEntity_WF_RF_F_F_AB rst20 rst10 simOutA_WF_RF_F_F_AB simOutB_WF_RF_F_F_AB
{-# NOINLINE testBench_WF_RF_F_F_AB #-}
{-# ANN testBench_WF_RF_F_F_AB (TestBench 'topEntity_WF_RF_F_F_AB) #-}
testBench_WF_RF_F_T_AB = genTestBench topEntity_WF_RF_F_T_AB rst20 rst10 simOutA_WF_RF_F_T_AB simOutB_WF_RF_F_T_AB
{-# NOINLINE testBench_WF_RF_F_T_AB #-}
{-# ANN testBench_WF_RF_F_T_AB (TestBench 'topEntity_WF_RF_F_T_AB) #-}
testBench_WF_RF_T_F_AB = genTestBench topEntity_WF_RF_T_F_AB rst20 rst10 simOutA_WF_RF_T_F_AB simOutB_WF_RF_T_F_AB
{-# NOINLINE testBench_WF_RF_T_F_AB #-}
{-# ANN testBench_WF_RF_T_F_AB (TestBench 'topEntity_WF_RF_T_F_AB) #-}
testBench_WF_RF_T_T_AB = genTestBench topEntity_WF_RF_T_T_AB rst20 rst10 simOutA_WF_RF_T_T_AB simOutB_WF_RF_T_T_AB
{-# NOINLINE testBench_WF_RF_T_T_AB #-}
{-# ANN testBench_WF_RF_T_T_AB (TestBench 'topEntity_WF_RF_T_T_AB) #-}
testBench_WF_RF_F_F_BC = genTestBench topEntity_WF_RF_F_F_BC rst10 rst7 simOutA_WF_RF_F_F_BC simOutB_WF_RF_F_F_BC
{-# NOINLINE testBench_WF_RF_F_F_BC #-}
{-# ANN testBench_WF_RF_F_F_BC (TestBench 'topEntity_WF_RF_F_F_BC) #-}
testBench_WF_RF_F_T_BC = genTestBench topEntity_WF_RF_F_T_BC rst10 rst7 simOutA_WF_RF_F_T_BC simOutB_WF_RF_F_T_BC
{-# NOINLINE testBench_WF_RF_F_T_BC #-}
{-# ANN testBench_WF_RF_F_T_BC (TestBench 'topEntity_WF_RF_F_T_BC) #-}
testBench_WF_RF_T_F_BC = genTestBench topEntity_WF_RF_T_F_BC rst10 rst7 simOutA_WF_RF_T_F_BC simOutB_WF_RF_T_F_BC
{-# NOINLINE testBench_WF_RF_T_F_BC #-}
{-# ANN testBench_WF_RF_T_F_BC (TestBench 'topEntity_WF_RF_T_F_BC) #-}
testBench_WF_RF_T_T_BC = genTestBench topEntity_WF_RF_T_T_BC rst10 rst7 simOutA_WF_RF_T_T_BC simOutB_WF_RF_T_T_BC
{-# NOINLINE testBench_WF_RF_T_T_BC #-}
{-# ANN testBench_WF_RF_T_T_BC (TestBench 'topEntity_WF_RF_T_T_BC) #-}
testBench_WF_NC_F_F_AB = genTestBench topEntity_WF_NC_F_F_AB rst20 rst10 simOutA_WF_NC_F_F_AB simOutB_WF_NC_F_F_AB
{-# NOINLINE testBench_WF_NC_F_F_AB #-}
{-# ANN testBench_WF_NC_F_F_AB (TestBench 'topEntity_WF_NC_F_F_AB) #-}
testBench_WF_NC_F_T_AB = genTestBench topEntity_WF_NC_F_T_AB rst20 rst10 simOutA_WF_NC_F_T_AB simOutB_WF_NC_F_T_AB
{-# NOINLINE testBench_WF_NC_F_T_AB #-}
{-# ANN testBench_WF_NC_F_T_AB (TestBench 'topEntity_WF_NC_F_T_AB) #-}
testBench_WF_NC_T_F_AB = genTestBench topEntity_WF_NC_T_F_AB rst20 rst10 simOutA_WF_NC_T_F_AB simOutB_WF_NC_T_F_AB
{-# NOINLINE testBench_WF_NC_T_F_AB #-}
{-# ANN testBench_WF_NC_T_F_AB (TestBench 'topEntity_WF_NC_T_F_AB) #-}
testBench_WF_NC_T_T_AB = genTestBench topEntity_WF_NC_T_T_AB rst20 rst10 simOutA_WF_NC_T_T_AB simOutB_WF_NC_T_T_AB
{-# NOINLINE testBench_WF_NC_T_T_AB #-}
{-# ANN testBench_WF_NC_T_T_AB (TestBench 'topEntity_WF_NC_T_T_AB) #-}
testBench_WF_NC_F_F_BC = genTestBench topEntity_WF_NC_F_F_BC rst10 rst7 simOutA_WF_NC_F_F_BC simOutB_WF_NC_F_F_BC
{-# NOINLINE testBench_WF_NC_F_F_BC #-}
{-# ANN testBench_WF_NC_F_F_BC (TestBench 'topEntity_WF_NC_F_F_BC) #-}
testBench_WF_NC_F_T_BC = genTestBench topEntity_WF_NC_F_T_BC rst10 rst7 simOutA_WF_NC_F_T_BC simOutB_WF_NC_F_T_BC
{-# NOINLINE testBench_WF_NC_F_T_BC #-}
{-# ANN testBench_WF_NC_F_T_BC (TestBench 'topEntity_WF_NC_F_T_BC) #-}
testBench_WF_NC_T_F_BC = genTestBench topEntity_WF_NC_T_F_BC rst10 rst7 simOutA_WF_NC_T_F_BC simOutB_WF_NC_T_F_BC
{-# NOINLINE testBench_WF_NC_T_F_BC #-}
{-# ANN testBench_WF_NC_T_F_BC (TestBench 'topEntity_WF_NC_T_F_BC) #-}
testBench_WF_NC_T_T_BC = genTestBench topEntity_WF_NC_T_T_BC rst10 rst7 simOutA_WF_NC_T_T_BC simOutB_WF_NC_T_T_BC
{-# NOINLINE testBench_WF_NC_T_T_BC #-}
{-# ANN testBench_WF_NC_T_T_BC (TestBench 'topEntity_WF_NC_T_T_BC) #-}
testBench_RF_WF_F_F_AB = genTestBench topEntity_RF_WF_F_F_AB rst20 rst10 simOutA_RF_WF_F_F_AB simOutB_RF_WF_F_F_AB
{-# NOINLINE testBench_RF_WF_F_F_AB #-}
{-# ANN testBench_RF_WF_F_F_AB (TestBench 'topEntity_RF_WF_F_F_AB) #-}
testBench_RF_WF_F_T_AB = genTestBench topEntity_RF_WF_F_T_AB rst20 rst10 simOutA_RF_WF_F_T_AB simOutB_RF_WF_F_T_AB
{-# NOINLINE testBench_RF_WF_F_T_AB #-}
{-# ANN testBench_RF_WF_F_T_AB (TestBench 'topEntity_RF_WF_F_T_AB) #-}
testBench_RF_WF_T_F_AB = genTestBench topEntity_RF_WF_T_F_AB rst20 rst10 simOutA_RF_WF_T_F_AB simOutB_RF_WF_T_F_AB
{-# NOINLINE testBench_RF_WF_T_F_AB #-}
{-# ANN testBench_RF_WF_T_F_AB (TestBench 'topEntity_RF_WF_T_F_AB) #-}
testBench_RF_WF_T_T_AB = genTestBench topEntity_RF_WF_T_T_AB rst20 rst10 simOutA_RF_WF_T_T_AB simOutB_RF_WF_T_T_AB
{-# NOINLINE testBench_RF_WF_T_T_AB #-}
{-# ANN testBench_RF_WF_T_T_AB (TestBench 'topEntity_RF_WF_T_T_AB) #-}
testBench_RF_WF_F_F_BC = genTestBench topEntity_RF_WF_F_F_BC rst10 rst7 simOutA_RF_WF_F_F_BC simOutB_RF_WF_F_F_BC
{-# NOINLINE testBench_RF_WF_F_F_BC #-}
{-# ANN testBench_RF_WF_F_F_BC (TestBench 'topEntity_RF_WF_F_F_BC) #-}
testBench_RF_WF_F_T_BC = genTestBench topEntity_RF_WF_F_T_BC rst10 rst7 simOutA_RF_WF_F_T_BC simOutB_RF_WF_F_T_BC
{-# NOINLINE testBench_RF_WF_F_T_BC #-}
{-# ANN testBench_RF_WF_F_T_BC (TestBench 'topEntity_RF_WF_F_T_BC) #-}
testBench_RF_WF_T_F_BC = genTestBench topEntity_RF_WF_T_F_BC rst10 rst7 simOutA_RF_WF_T_F_BC simOutB_RF_WF_T_F_BC
{-# NOINLINE testBench_RF_WF_T_F_BC #-}
{-# ANN testBench_RF_WF_T_F_BC (TestBench 'topEntity_RF_WF_T_F_BC) #-}
testBench_RF_WF_T_T_BC = genTestBench topEntity_RF_WF_T_T_BC rst10 rst7 simOutA_RF_WF_T_T_BC simOutB_RF_WF_T_T_BC
{-# NOINLINE testBench_RF_WF_T_T_BC #-}
{-# ANN testBench_RF_WF_T_T_BC (TestBench 'topEntity_RF_WF_T_T_BC) #-}
testBench_RF_RF_F_F_AB = genTestBench topEntity_RF_RF_F_F_AB rst20 rst10 simOutA_RF_RF_F_F_AB simOutB_RF_RF_F_F_AB
{-# NOINLINE testBench_RF_RF_F_F_AB #-}
{-# ANN testBench_RF_RF_F_F_AB (TestBench 'topEntity_RF_RF_F_F_AB) #-}
testBench_RF_RF_F_T_AB = genTestBench topEntity_RF_RF_F_T_AB rst20 rst10 simOutA_RF_RF_F_T_AB simOutB_RF_RF_F_T_AB
{-# NOINLINE testBench_RF_RF_F_T_AB #-}
{-# ANN testBench_RF_RF_F_T_AB (TestBench 'topEntity_RF_RF_F_T_AB) #-}
testBench_RF_RF_T_F_AB = genTestBench topEntity_RF_RF_T_F_AB rst20 rst10 simOutA_RF_RF_T_F_AB simOutB_RF_RF_T_F_AB
{-# NOINLINE testBench_RF_RF_T_F_AB #-}
{-# ANN testBench_RF_RF_T_F_AB (TestBench 'topEntity_RF_RF_T_F_AB) #-}
testBench_RF_RF_T_T_AB = genTestBench topEntity_RF_RF_T_T_AB rst20 rst10 simOutA_RF_RF_T_T_AB simOutB_RF_RF_T_T_AB
{-# NOINLINE testBench_RF_RF_T_T_AB #-}
{-# ANN testBench_RF_RF_T_T_AB (TestBench 'topEntity_RF_RF_T_T_AB) #-}
testBench_RF_RF_F_F_BC = genTestBench topEntity_RF_RF_F_F_BC rst10 rst7 simOutA_RF_RF_F_F_BC simOutB_RF_RF_F_F_BC
{-# NOINLINE testBench_RF_RF_F_F_BC #-}
{-# ANN testBench_RF_RF_F_F_BC (TestBench 'topEntity_RF_RF_F_F_BC) #-}
testBench_RF_RF_F_T_BC = genTestBench topEntity_RF_RF_F_T_BC rst10 rst7 simOutA_RF_RF_F_T_BC simOutB_RF_RF_F_T_BC
{-# NOINLINE testBench_RF_RF_F_T_BC #-}
{-# ANN testBench_RF_RF_F_T_BC (TestBench 'topEntity_RF_RF_F_T_BC) #-}
testBench_RF_RF_T_F_BC = genTestBench topEntity_RF_RF_T_F_BC rst10 rst7 simOutA_RF_RF_T_F_BC simOutB_RF_RF_T_F_BC
{-# NOINLINE testBench_RF_RF_T_F_BC #-}
{-# ANN testBench_RF_RF_T_F_BC (TestBench 'topEntity_RF_RF_T_F_BC) #-}
testBench_RF_RF_T_T_BC = genTestBench topEntity_RF_RF_T_T_BC rst10 rst7 simOutA_RF_RF_T_T_BC simOutB_RF_RF_T_T_BC
{-# NOINLINE testBench_RF_RF_T_T_BC #-}
{-# ANN testBench_RF_RF_T_T_BC (TestBench 'topEntity_RF_RF_T_T_BC) #-}
testBench_RF_NC_F_F_AB = genTestBench topEntity_RF_NC_F_F_AB rst20 rst10 simOutA_RF_NC_F_F_AB simOutB_RF_NC_F_F_AB
{-# NOINLINE testBench_RF_NC_F_F_AB #-}
{-# ANN testBench_RF_NC_F_F_AB (TestBench 'topEntity_RF_NC_F_F_AB) #-}
testBench_RF_NC_F_T_AB = genTestBench topEntity_RF_NC_F_T_AB rst20 rst10 simOutA_RF_NC_F_T_AB simOutB_RF_NC_F_T_AB
{-# NOINLINE testBench_RF_NC_F_T_AB #-}
{-# ANN testBench_RF_NC_F_T_AB (TestBench 'topEntity_RF_NC_F_T_AB) #-}
testBench_RF_NC_T_F_AB = genTestBench topEntity_RF_NC_T_F_AB rst20 rst10 simOutA_RF_NC_T_F_AB simOutB_RF_NC_T_F_AB
{-# NOINLINE testBench_RF_NC_T_F_AB #-}
{-# ANN testBench_RF_NC_T_F_AB (TestBench 'topEntity_RF_NC_T_F_AB) #-}
testBench_RF_NC_T_T_AB = genTestBench topEntity_RF_NC_T_T_AB rst20 rst10 simOutA_RF_NC_T_T_AB simOutB_RF_NC_T_T_AB
{-# NOINLINE testBench_RF_NC_T_T_AB #-}
{-# ANN testBench_RF_NC_T_T_AB (TestBench 'topEntity_RF_NC_T_T_AB) #-}
testBench_RF_NC_F_F_BC = genTestBench topEntity_RF_NC_F_F_BC rst10 rst7 simOutA_RF_NC_F_F_BC simOutB_RF_NC_F_F_BC
{-# NOINLINE testBench_RF_NC_F_F_BC #-}
{-# ANN testBench_RF_NC_F_F_BC (TestBench 'topEntity_RF_NC_F_F_BC) #-}
testBench_RF_NC_F_T_BC = genTestBench topEntity_RF_NC_F_T_BC rst10 rst7 simOutA_RF_NC_F_T_BC simOutB_RF_NC_F_T_BC
{-# NOINLINE testBench_RF_NC_F_T_BC #-}
{-# ANN testBench_RF_NC_F_T_BC (TestBench 'topEntity_RF_NC_F_T_BC) #-}
testBench_RF_NC_T_F_BC = genTestBench topEntity_RF_NC_T_F_BC rst10 rst7 simOutA_RF_NC_T_F_BC simOutB_RF_NC_T_F_BC
{-# NOINLINE testBench_RF_NC_T_F_BC #-}
{-# ANN testBench_RF_NC_T_F_BC (TestBench 'topEntity_RF_NC_T_F_BC) #-}
testBench_RF_NC_T_T_BC = genTestBench topEntity_RF_NC_T_T_BC rst10 rst7 simOutA_RF_NC_T_T_BC simOutB_RF_NC_T_T_BC
{-# NOINLINE testBench_RF_NC_T_T_BC #-}
{-# ANN testBench_RF_NC_T_T_BC (TestBench 'topEntity_RF_NC_T_T_BC) #-}
testBench_NC_WF_F_F_AB = genTestBench topEntity_NC_WF_F_F_AB rst20 rst10 simOutA_NC_WF_F_F_AB simOutB_NC_WF_F_F_AB
{-# NOINLINE testBench_NC_WF_F_F_AB #-}
{-# ANN testBench_NC_WF_F_F_AB (TestBench 'topEntity_NC_WF_F_F_AB) #-}
testBench_NC_WF_F_T_AB = genTestBench topEntity_NC_WF_F_T_AB rst20 rst10 simOutA_NC_WF_F_T_AB simOutB_NC_WF_F_T_AB
{-# NOINLINE testBench_NC_WF_F_T_AB #-}
{-# ANN testBench_NC_WF_F_T_AB (TestBench 'topEntity_NC_WF_F_T_AB) #-}
testBench_NC_WF_T_F_AB = genTestBench topEntity_NC_WF_T_F_AB rst20 rst10 simOutA_NC_WF_T_F_AB simOutB_NC_WF_T_F_AB
{-# NOINLINE testBench_NC_WF_T_F_AB #-}
{-# ANN testBench_NC_WF_T_F_AB (TestBench 'topEntity_NC_WF_T_F_AB) #-}
testBench_NC_WF_T_T_AB = genTestBench topEntity_NC_WF_T_T_AB rst20 rst10 simOutA_NC_WF_T_T_AB simOutB_NC_WF_T_T_AB
{-# NOINLINE testBench_NC_WF_T_T_AB #-}
{-# ANN testBench_NC_WF_T_T_AB (TestBench 'topEntity_NC_WF_T_T_AB) #-}
testBench_NC_WF_F_F_BC = genTestBench topEntity_NC_WF_F_F_BC rst10 rst7 simOutA_NC_WF_F_F_BC simOutB_NC_WF_F_F_BC
{-# NOINLINE testBench_NC_WF_F_F_BC #-}
{-# ANN testBench_NC_WF_F_F_BC (TestBench 'topEntity_NC_WF_F_F_BC) #-}
testBench_NC_WF_F_T_BC = genTestBench topEntity_NC_WF_F_T_BC rst10 rst7 simOutA_NC_WF_F_T_BC simOutB_NC_WF_F_T_BC
{-# NOINLINE testBench_NC_WF_F_T_BC #-}
{-# ANN testBench_NC_WF_F_T_BC (TestBench 'topEntity_NC_WF_F_T_BC) #-}
testBench_NC_WF_T_F_BC = genTestBench topEntity_NC_WF_T_F_BC rst10 rst7 simOutA_NC_WF_T_F_BC simOutB_NC_WF_T_F_BC
{-# NOINLINE testBench_NC_WF_T_F_BC #-}
{-# ANN testBench_NC_WF_T_F_BC (TestBench 'topEntity_NC_WF_T_F_BC) #-}
testBench_NC_WF_T_T_BC = genTestBench topEntity_NC_WF_T_T_BC rst10 rst7 simOutA_NC_WF_T_T_BC simOutB_NC_WF_T_T_BC
{-# NOINLINE testBench_NC_WF_T_T_BC #-}
{-# ANN testBench_NC_WF_T_T_BC (TestBench 'topEntity_NC_WF_T_T_BC) #-}
testBench_NC_RF_F_F_AB = genTestBench topEntity_NC_RF_F_F_AB rst20 rst10 simOutA_NC_RF_F_F_AB simOutB_NC_RF_F_F_AB
{-# NOINLINE testBench_NC_RF_F_F_AB #-}
{-# ANN testBench_NC_RF_F_F_AB (TestBench 'topEntity_NC_RF_F_F_AB) #-}
testBench_NC_RF_F_T_AB = genTestBench topEntity_NC_RF_F_T_AB rst20 rst10 simOutA_NC_RF_F_T_AB simOutB_NC_RF_F_T_AB
{-# NOINLINE testBench_NC_RF_F_T_AB #-}
{-# ANN testBench_NC_RF_F_T_AB (TestBench 'topEntity_NC_RF_F_T_AB) #-}
testBench_NC_RF_T_F_AB = genTestBench topEntity_NC_RF_T_F_AB rst20 rst10 simOutA_NC_RF_T_F_AB simOutB_NC_RF_T_F_AB
{-# NOINLINE testBench_NC_RF_T_F_AB #-}
{-# ANN testBench_NC_RF_T_F_AB (TestBench 'topEntity_NC_RF_T_F_AB) #-}
testBench_NC_RF_T_T_AB = genTestBench topEntity_NC_RF_T_T_AB rst20 rst10 simOutA_NC_RF_T_T_AB simOutB_NC_RF_T_T_AB
{-# NOINLINE testBench_NC_RF_T_T_AB #-}
{-# ANN testBench_NC_RF_T_T_AB (TestBench 'topEntity_NC_RF_T_T_AB) #-}
testBench_NC_RF_F_F_BC = genTestBench topEntity_NC_RF_F_F_BC rst10 rst7 simOutA_NC_RF_F_F_BC simOutB_NC_RF_F_F_BC
{-# NOINLINE testBench_NC_RF_F_F_BC #-}
{-# ANN testBench_NC_RF_F_F_BC (TestBench 'topEntity_NC_RF_F_F_BC) #-}
testBench_NC_RF_F_T_BC = genTestBench topEntity_NC_RF_F_T_BC rst10 rst7 simOutA_NC_RF_F_T_BC simOutB_NC_RF_F_T_BC
{-# NOINLINE testBench_NC_RF_F_T_BC #-}
{-# ANN testBench_NC_RF_F_T_BC (TestBench 'topEntity_NC_RF_F_T_BC) #-}
testBench_NC_RF_T_F_BC = genTestBench topEntity_NC_RF_T_F_BC rst10 rst7 simOutA_NC_RF_T_F_BC simOutB_NC_RF_T_F_BC
{-# NOINLINE testBench_NC_RF_T_F_BC #-}
{-# ANN testBench_NC_RF_T_F_BC (TestBench 'topEntity_NC_RF_T_F_BC) #-}
testBench_NC_RF_T_T_BC = genTestBench topEntity_NC_RF_T_T_BC rst10 rst7 simOutA_NC_RF_T_T_BC simOutB_NC_RF_T_T_BC
{-# NOINLINE testBench_NC_RF_T_T_BC #-}
{-# ANN testBench_NC_RF_T_T_BC (TestBench 'topEntity_NC_RF_T_T_BC) #-}
testBench_NC_NC_F_F_AB = genTestBench topEntity_NC_NC_F_F_AB rst20 rst10 simOutA_NC_NC_F_F_AB simOutB_NC_NC_F_F_AB
{-# NOINLINE testBench_NC_NC_F_F_AB #-}
{-# ANN testBench_NC_NC_F_F_AB (TestBench 'topEntity_NC_NC_F_F_AB) #-}
testBench_NC_NC_F_T_AB = genTestBench topEntity_NC_NC_F_T_AB rst20 rst10 simOutA_NC_NC_F_T_AB simOutB_NC_NC_F_T_AB
{-# NOINLINE testBench_NC_NC_F_T_AB #-}
{-# ANN testBench_NC_NC_F_T_AB (TestBench 'topEntity_NC_NC_F_T_AB) #-}
testBench_NC_NC_T_F_AB = genTestBench topEntity_NC_NC_T_F_AB rst20 rst10 simOutA_NC_NC_T_F_AB simOutB_NC_NC_T_F_AB
{-# NOINLINE testBench_NC_NC_T_F_AB #-}
{-# ANN testBench_NC_NC_T_F_AB (TestBench 'topEntity_NC_NC_T_F_AB) #-}
testBench_NC_NC_T_T_AB = genTestBench topEntity_NC_NC_T_T_AB rst20 rst10 simOutA_NC_NC_T_T_AB simOutB_NC_NC_T_T_AB
{-# NOINLINE testBench_NC_NC_T_T_AB #-}
{-# ANN testBench_NC_NC_T_T_AB (TestBench 'topEntity_NC_NC_T_T_AB) #-}
testBench_NC_NC_F_F_BC = genTestBench topEntity_NC_NC_F_F_BC rst10 rst7 simOutA_NC_NC_F_F_BC simOutB_NC_NC_F_F_BC
{-# NOINLINE testBench_NC_NC_F_F_BC #-}
{-# ANN testBench_NC_NC_F_F_BC (TestBench 'topEntity_NC_NC_F_F_BC) #-}
testBench_NC_NC_F_T_BC = genTestBench topEntity_NC_NC_F_T_BC rst10 rst7 simOutA_NC_NC_F_T_BC simOutB_NC_NC_F_T_BC
{-# NOINLINE testBench_NC_NC_F_T_BC #-}
{-# ANN testBench_NC_NC_F_T_BC (TestBench 'topEntity_NC_NC_F_T_BC) #-}
testBench_NC_NC_T_F_BC = genTestBench topEntity_NC_NC_T_F_BC rst10 rst7 simOutA_NC_NC_T_F_BC simOutB_NC_NC_T_F_BC
{-# NOINLINE testBench_NC_NC_T_F_BC #-}
{-# ANN testBench_NC_NC_T_F_BC (TestBench 'topEntity_NC_NC_T_F_BC) #-}
testBench_NC_NC_T_T_BC = genTestBench topEntity_NC_NC_T_T_BC rst10 rst7 simOutA_NC_NC_T_T_BC simOutB_NC_NC_T_T_BC
{-# NOINLINE testBench_NC_NC_T_T_BC #-}
{-# ANN testBench_NC_NC_T_T_BC (TestBench 'topEntity_NC_NC_T_T_BC) #-}

topEntity_WF_WF_F_F_AB = topOut WriteFirst WriteFirst False False rst20 rst10
{-# NOINLINE topEntity_WF_WF_F_F_AB #-}
{-# ANN topEntity_WF_WF_F_F_AB (defSyn "topEntity_WF_WF_F_F_AB") #-}
topEntity_WF_WF_F_T_AB = topOut WriteFirst WriteFirst False True rst20 rst10
{-# NOINLINE topEntity_WF_WF_F_T_AB #-}
{-# ANN topEntity_WF_WF_F_T_AB (defSyn "topEntity_WF_WF_F_T_AB") #-}
topEntity_WF_WF_T_F_AB = topOut WriteFirst WriteFirst True False rst20 rst10
{-# NOINLINE topEntity_WF_WF_T_F_AB #-}
{-# ANN topEntity_WF_WF_T_F_AB (defSyn "topEntity_WF_WF_T_F_AB") #-}
topEntity_WF_WF_T_T_AB = topOut WriteFirst WriteFirst True True rst20 rst10
{-# NOINLINE topEntity_WF_WF_T_T_AB #-}
{-# ANN topEntity_WF_WF_T_T_AB (defSyn "topEntity_WF_WF_T_T_AB") #-}
topEntity_WF_WF_F_F_BC = topOut WriteFirst WriteFirst False False rst10 rst7
{-# NOINLINE topEntity_WF_WF_F_F_BC #-}
{-# ANN topEntity_WF_WF_F_F_BC (defSyn "topEntity_WF_WF_F_F_BC") #-}
topEntity_WF_WF_F_T_BC = topOut WriteFirst WriteFirst False True rst10 rst7
{-# NOINLINE topEntity_WF_WF_F_T_BC #-}
{-# ANN topEntity_WF_WF_F_T_BC (defSyn "topEntity_WF_WF_F_T_BC") #-}
topEntity_WF_WF_T_F_BC = topOut WriteFirst WriteFirst True False rst10 rst7
{-# NOINLINE topEntity_WF_WF_T_F_BC #-}
{-# ANN topEntity_WF_WF_T_F_BC (defSyn "topEntity_WF_WF_T_F_BC") #-}
topEntity_WF_WF_T_T_BC = topOut WriteFirst WriteFirst True True rst10 rst7
{-# NOINLINE topEntity_WF_WF_T_T_BC #-}
{-# ANN topEntity_WF_WF_T_T_BC (defSyn "topEntity_WF_WF_T_T_BC") #-}
topEntity_WF_RF_F_F_AB = topOut WriteFirst ReadFirst False False rst20 rst10
{-# NOINLINE topEntity_WF_RF_F_F_AB #-}
{-# ANN topEntity_WF_RF_F_F_AB (defSyn "topEntity_WF_RF_F_F_AB") #-}
topEntity_WF_RF_F_T_AB = topOut WriteFirst ReadFirst False True rst20 rst10
{-# NOINLINE topEntity_WF_RF_F_T_AB #-}
{-# ANN topEntity_WF_RF_F_T_AB (defSyn "topEntity_WF_RF_F_T_AB") #-}
topEntity_WF_RF_T_F_AB = topOut WriteFirst ReadFirst True False rst20 rst10
{-# NOINLINE topEntity_WF_RF_T_F_AB #-}
{-# ANN topEntity_WF_RF_T_F_AB (defSyn "topEntity_WF_RF_T_F_AB") #-}
topEntity_WF_RF_T_T_AB = topOut WriteFirst ReadFirst True True rst20 rst10
{-# NOINLINE topEntity_WF_RF_T_T_AB #-}
{-# ANN topEntity_WF_RF_T_T_AB (defSyn "topEntity_WF_RF_T_T_AB") #-}
topEntity_WF_RF_F_F_BC = topOut WriteFirst ReadFirst False False rst10 rst7
{-# NOINLINE topEntity_WF_RF_F_F_BC #-}
{-# ANN topEntity_WF_RF_F_F_BC (defSyn "topEntity_WF_RF_F_F_BC") #-}
topEntity_WF_RF_F_T_BC = topOut WriteFirst ReadFirst False True rst10 rst7
{-# NOINLINE topEntity_WF_RF_F_T_BC #-}
{-# ANN topEntity_WF_RF_F_T_BC (defSyn "topEntity_WF_RF_F_T_BC") #-}
topEntity_WF_RF_T_F_BC = topOut WriteFirst ReadFirst True False rst10 rst7
{-# NOINLINE topEntity_WF_RF_T_F_BC #-}
{-# ANN topEntity_WF_RF_T_F_BC (defSyn "topEntity_WF_RF_T_F_BC") #-}
topEntity_WF_RF_T_T_BC = topOut WriteFirst ReadFirst True True rst10 rst7
{-# NOINLINE topEntity_WF_RF_T_T_BC #-}
{-# ANN topEntity_WF_RF_T_T_BC (defSyn "topEntity_WF_RF_T_T_BC") #-}
topEntity_WF_NC_F_F_AB = topOut WriteFirst NoChange False False rst20 rst10
{-# NOINLINE topEntity_WF_NC_F_F_AB #-}
{-# ANN topEntity_WF_NC_F_F_AB (defSyn "topEntity_WF_NC_F_F_AB") #-}
topEntity_WF_NC_F_T_AB = topOut WriteFirst NoChange False True rst20 rst10
{-# NOINLINE topEntity_WF_NC_F_T_AB #-}
{-# ANN topEntity_WF_NC_F_T_AB (defSyn "topEntity_WF_NC_F_T_AB") #-}
topEntity_WF_NC_T_F_AB = topOut WriteFirst NoChange True False rst20 rst10
{-# NOINLINE topEntity_WF_NC_T_F_AB #-}
{-# ANN topEntity_WF_NC_T_F_AB (defSyn "topEntity_WF_NC_T_F_AB") #-}
topEntity_WF_NC_T_T_AB = topOut WriteFirst NoChange True True rst20 rst10
{-# NOINLINE topEntity_WF_NC_T_T_AB #-}
{-# ANN topEntity_WF_NC_T_T_AB (defSyn "topEntity_WF_NC_T_T_AB") #-}
topEntity_WF_NC_F_F_BC = topOut WriteFirst NoChange False False rst10 rst7
{-# NOINLINE topEntity_WF_NC_F_F_BC #-}
{-# ANN topEntity_WF_NC_F_F_BC (defSyn "topEntity_WF_NC_F_F_BC") #-}
topEntity_WF_NC_F_T_BC = topOut WriteFirst NoChange False True rst10 rst7
{-# NOINLINE topEntity_WF_NC_F_T_BC #-}
{-# ANN topEntity_WF_NC_F_T_BC (defSyn "topEntity_WF_NC_F_T_BC") #-}
topEntity_WF_NC_T_F_BC = topOut WriteFirst NoChange True False rst10 rst7
{-# NOINLINE topEntity_WF_NC_T_F_BC #-}
{-# ANN topEntity_WF_NC_T_F_BC (defSyn "topEntity_WF_NC_T_F_BC") #-}
topEntity_WF_NC_T_T_BC = topOut WriteFirst NoChange True True rst10 rst7
{-# NOINLINE topEntity_WF_NC_T_T_BC #-}
{-# ANN topEntity_WF_NC_T_T_BC (defSyn "topEntity_WF_NC_T_T_BC") #-}
topEntity_RF_WF_F_F_AB = topOut ReadFirst WriteFirst False False rst20 rst10
{-# NOINLINE topEntity_RF_WF_F_F_AB #-}
{-# ANN topEntity_RF_WF_F_F_AB (defSyn "topEntity_RF_WF_F_F_AB") #-}
topEntity_RF_WF_F_T_AB = topOut ReadFirst WriteFirst False True rst20 rst10
{-# NOINLINE topEntity_RF_WF_F_T_AB #-}
{-# ANN topEntity_RF_WF_F_T_AB (defSyn "topEntity_RF_WF_F_T_AB") #-}
topEntity_RF_WF_T_F_AB = topOut ReadFirst WriteFirst True False rst20 rst10
{-# NOINLINE topEntity_RF_WF_T_F_AB #-}
{-# ANN topEntity_RF_WF_T_F_AB (defSyn "topEntity_RF_WF_T_F_AB") #-}
topEntity_RF_WF_T_T_AB = topOut ReadFirst WriteFirst True True rst20 rst10
{-# NOINLINE topEntity_RF_WF_T_T_AB #-}
{-# ANN topEntity_RF_WF_T_T_AB (defSyn "topEntity_RF_WF_T_T_AB") #-}
topEntity_RF_WF_F_F_BC = topOut ReadFirst WriteFirst False False rst10 rst7
{-# NOINLINE topEntity_RF_WF_F_F_BC #-}
{-# ANN topEntity_RF_WF_F_F_BC (defSyn "topEntity_RF_WF_F_F_BC") #-}
topEntity_RF_WF_F_T_BC = topOut ReadFirst WriteFirst False True rst10 rst7
{-# NOINLINE topEntity_RF_WF_F_T_BC #-}
{-# ANN topEntity_RF_WF_F_T_BC (defSyn "topEntity_RF_WF_F_T_BC") #-}
topEntity_RF_WF_T_F_BC = topOut ReadFirst WriteFirst True False rst10 rst7
{-# NOINLINE topEntity_RF_WF_T_F_BC #-}
{-# ANN topEntity_RF_WF_T_F_BC (defSyn "topEntity_RF_WF_T_F_BC") #-}
topEntity_RF_WF_T_T_BC = topOut ReadFirst WriteFirst True True rst10 rst7
{-# NOINLINE topEntity_RF_WF_T_T_BC #-}
{-# ANN topEntity_RF_WF_T_T_BC (defSyn "topEntity_RF_WF_T_T_BC") #-}
topEntity_RF_RF_F_F_AB = topOut ReadFirst ReadFirst False False rst20 rst10
{-# NOINLINE topEntity_RF_RF_F_F_AB #-}
{-# ANN topEntity_RF_RF_F_F_AB (defSyn "topEntity_RF_RF_F_F_AB") #-}
topEntity_RF_RF_F_T_AB = topOut ReadFirst ReadFirst False True rst20 rst10
{-# NOINLINE topEntity_RF_RF_F_T_AB #-}
{-# ANN topEntity_RF_RF_F_T_AB (defSyn "topEntity_RF_RF_F_T_AB") #-}
topEntity_RF_RF_T_F_AB = topOut ReadFirst ReadFirst True False rst20 rst10
{-# NOINLINE topEntity_RF_RF_T_F_AB #-}
{-# ANN topEntity_RF_RF_T_F_AB (defSyn "topEntity_RF_RF_T_F_AB") #-}
topEntity_RF_RF_T_T_AB = topOut ReadFirst ReadFirst True True rst20 rst10
{-# NOINLINE topEntity_RF_RF_T_T_AB #-}
{-# ANN topEntity_RF_RF_T_T_AB (defSyn "topEntity_RF_RF_T_T_AB") #-}
topEntity_RF_RF_F_F_BC = topOut ReadFirst ReadFirst False False rst10 rst7
{-# NOINLINE topEntity_RF_RF_F_F_BC #-}
{-# ANN topEntity_RF_RF_F_F_BC (defSyn "topEntity_RF_RF_F_F_BC") #-}
topEntity_RF_RF_F_T_BC = topOut ReadFirst ReadFirst False True rst10 rst7
{-# NOINLINE topEntity_RF_RF_F_T_BC #-}
{-# ANN topEntity_RF_RF_F_T_BC (defSyn "topEntity_RF_RF_F_T_BC") #-}
topEntity_RF_RF_T_F_BC = topOut ReadFirst ReadFirst True False rst10 rst7
{-# NOINLINE topEntity_RF_RF_T_F_BC #-}
{-# ANN topEntity_RF_RF_T_F_BC (defSyn "topEntity_RF_RF_T_F_BC") #-}
topEntity_RF_RF_T_T_BC = topOut ReadFirst ReadFirst True True rst10 rst7
{-# NOINLINE topEntity_RF_RF_T_T_BC #-}
{-# ANN topEntity_RF_RF_T_T_BC (defSyn "topEntity_RF_RF_T_T_BC") #-}
topEntity_RF_NC_F_F_AB = topOut ReadFirst NoChange False False rst20 rst10
{-# NOINLINE topEntity_RF_NC_F_F_AB #-}
{-# ANN topEntity_RF_NC_F_F_AB (defSyn "topEntity_RF_NC_F_F_AB") #-}
topEntity_RF_NC_F_T_AB = topOut ReadFirst NoChange False True rst20 rst10
{-# NOINLINE topEntity_RF_NC_F_T_AB #-}
{-# ANN topEntity_RF_NC_F_T_AB (defSyn "topEntity_RF_NC_F_T_AB") #-}
topEntity_RF_NC_T_F_AB = topOut ReadFirst NoChange True False rst20 rst10
{-# NOINLINE topEntity_RF_NC_T_F_AB #-}
{-# ANN topEntity_RF_NC_T_F_AB (defSyn "topEntity_RF_NC_T_F_AB") #-}
topEntity_RF_NC_T_T_AB = topOut ReadFirst NoChange True True rst20 rst10
{-# NOINLINE topEntity_RF_NC_T_T_AB #-}
{-# ANN topEntity_RF_NC_T_T_AB (defSyn "topEntity_RF_NC_T_T_AB") #-}
topEntity_RF_NC_F_F_BC = topOut ReadFirst NoChange False False rst10 rst7
{-# NOINLINE topEntity_RF_NC_F_F_BC #-}
{-# ANN topEntity_RF_NC_F_F_BC (defSyn "topEntity_RF_NC_F_F_BC") #-}
topEntity_RF_NC_F_T_BC = topOut ReadFirst NoChange False True rst10 rst7
{-# NOINLINE topEntity_RF_NC_F_T_BC #-}
{-# ANN topEntity_RF_NC_F_T_BC (defSyn "topEntity_RF_NC_F_T_BC") #-}
topEntity_RF_NC_T_F_BC = topOut ReadFirst NoChange True False rst10 rst7
{-# NOINLINE topEntity_RF_NC_T_F_BC #-}
{-# ANN topEntity_RF_NC_T_F_BC (defSyn "topEntity_RF_NC_T_F_BC") #-}
topEntity_RF_NC_T_T_BC = topOut ReadFirst NoChange True True rst10 rst7
{-# NOINLINE topEntity_RF_NC_T_T_BC #-}
{-# ANN topEntity_RF_NC_T_T_BC (defSyn "topEntity_RF_NC_T_T_BC") #-}
topEntity_NC_WF_F_F_AB = topOut NoChange WriteFirst False False rst20 rst10
{-# NOINLINE topEntity_NC_WF_F_F_AB #-}
{-# ANN topEntity_NC_WF_F_F_AB (defSyn "topEntity_NC_WF_F_F_AB") #-}
topEntity_NC_WF_F_T_AB = topOut NoChange WriteFirst False True rst20 rst10
{-# NOINLINE topEntity_NC_WF_F_T_AB #-}
{-# ANN topEntity_NC_WF_F_T_AB (defSyn "topEntity_NC_WF_F_T_AB") #-}
topEntity_NC_WF_T_F_AB = topOut NoChange WriteFirst True False rst20 rst10
{-# NOINLINE topEntity_NC_WF_T_F_AB #-}
{-# ANN topEntity_NC_WF_T_F_AB (defSyn "topEntity_NC_WF_T_F_AB") #-}
topEntity_NC_WF_T_T_AB = topOut NoChange WriteFirst True True rst20 rst10
{-# NOINLINE topEntity_NC_WF_T_T_AB #-}
{-# ANN topEntity_NC_WF_T_T_AB (defSyn "topEntity_NC_WF_T_T_AB") #-}
topEntity_NC_WF_F_F_BC = topOut NoChange WriteFirst False False rst10 rst7
{-# NOINLINE topEntity_NC_WF_F_F_BC #-}
{-# ANN topEntity_NC_WF_F_F_BC (defSyn "topEntity_NC_WF_F_F_BC") #-}
topEntity_NC_WF_F_T_BC = topOut NoChange WriteFirst False True rst10 rst7
{-# NOINLINE topEntity_NC_WF_F_T_BC #-}
{-# ANN topEntity_NC_WF_F_T_BC (defSyn "topEntity_NC_WF_F_T_BC") #-}
topEntity_NC_WF_T_F_BC = topOut NoChange WriteFirst True False rst10 rst7
{-# NOINLINE topEntity_NC_WF_T_F_BC #-}
{-# ANN topEntity_NC_WF_T_F_BC (defSyn "topEntity_NC_WF_T_F_BC") #-}
topEntity_NC_WF_T_T_BC = topOut NoChange WriteFirst True True rst10 rst7
{-# NOINLINE topEntity_NC_WF_T_T_BC #-}
{-# ANN topEntity_NC_WF_T_T_BC (defSyn "topEntity_NC_WF_T_T_BC") #-}
topEntity_NC_RF_F_F_AB = topOut NoChange ReadFirst False False rst20 rst10
{-# NOINLINE topEntity_NC_RF_F_F_AB #-}
{-# ANN topEntity_NC_RF_F_F_AB (defSyn "topEntity_NC_RF_F_F_AB") #-}
topEntity_NC_RF_F_T_AB = topOut NoChange ReadFirst False True rst20 rst10
{-# NOINLINE topEntity_NC_RF_F_T_AB #-}
{-# ANN topEntity_NC_RF_F_T_AB (defSyn "topEntity_NC_RF_F_T_AB") #-}
topEntity_NC_RF_T_F_AB = topOut NoChange ReadFirst True False rst20 rst10
{-# NOINLINE topEntity_NC_RF_T_F_AB #-}
{-# ANN topEntity_NC_RF_T_F_AB (defSyn "topEntity_NC_RF_T_F_AB") #-}
topEntity_NC_RF_T_T_AB = topOut NoChange ReadFirst True True rst20 rst10
{-# NOINLINE topEntity_NC_RF_T_T_AB #-}
{-# ANN topEntity_NC_RF_T_T_AB (defSyn "topEntity_NC_RF_T_T_AB") #-}
topEntity_NC_RF_F_F_BC = topOut NoChange ReadFirst False False rst10 rst7
{-# NOINLINE topEntity_NC_RF_F_F_BC #-}
{-# ANN topEntity_NC_RF_F_F_BC (defSyn "topEntity_NC_RF_F_F_BC") #-}
topEntity_NC_RF_F_T_BC = topOut NoChange ReadFirst False True rst10 rst7
{-# NOINLINE topEntity_NC_RF_F_T_BC #-}
{-# ANN topEntity_NC_RF_F_T_BC (defSyn "topEntity_NC_RF_F_T_BC") #-}
topEntity_NC_RF_T_F_BC = topOut NoChange ReadFirst True False rst10 rst7
{-# NOINLINE topEntity_NC_RF_T_F_BC #-}
{-# ANN topEntity_NC_RF_T_F_BC (defSyn "topEntity_NC_RF_T_F_BC") #-}
topEntity_NC_RF_T_T_BC = topOut NoChange ReadFirst True True rst10 rst7
{-# NOINLINE topEntity_NC_RF_T_T_BC #-}
{-# ANN topEntity_NC_RF_T_T_BC (defSyn "topEntity_NC_RF_T_T_BC") #-}
topEntity_NC_NC_F_F_AB = topOut NoChange NoChange False False rst20 rst10
{-# NOINLINE topEntity_NC_NC_F_F_AB #-}
{-# ANN topEntity_NC_NC_F_F_AB (defSyn "topEntity_NC_NC_F_F_AB") #-}
topEntity_NC_NC_F_T_AB = topOut NoChange NoChange False True rst20 rst10
{-# NOINLINE topEntity_NC_NC_F_T_AB #-}
{-# ANN topEntity_NC_NC_F_T_AB (defSyn "topEntity_NC_NC_F_T_AB") #-}
topEntity_NC_NC_T_F_AB = topOut NoChange NoChange True False rst20 rst10
{-# NOINLINE topEntity_NC_NC_T_F_AB #-}
{-# ANN topEntity_NC_NC_T_F_AB (defSyn "topEntity_NC_NC_T_F_AB") #-}
topEntity_NC_NC_T_T_AB = topOut NoChange NoChange True True rst20 rst10
{-# NOINLINE topEntity_NC_NC_T_T_AB #-}
{-# ANN topEntity_NC_NC_T_T_AB (defSyn "topEntity_NC_NC_T_T_AB") #-}
topEntity_NC_NC_F_F_BC = topOut NoChange NoChange False False rst10 rst7
{-# NOINLINE topEntity_NC_NC_F_F_BC #-}
{-# ANN topEntity_NC_NC_F_F_BC (defSyn "topEntity_NC_NC_F_F_BC") #-}
topEntity_NC_NC_F_T_BC = topOut NoChange NoChange False True rst10 rst7
{-# NOINLINE topEntity_NC_NC_F_T_BC #-}
{-# ANN topEntity_NC_NC_F_T_BC (defSyn "topEntity_NC_NC_F_T_BC") #-}
topEntity_NC_NC_T_F_BC = topOut NoChange NoChange True False rst10 rst7
{-# NOINLINE topEntity_NC_NC_T_F_BC #-}
{-# ANN topEntity_NC_NC_T_F_BC (defSyn "topEntity_NC_NC_T_F_BC") #-}
topEntity_NC_NC_T_T_BC = topOut NoChange NoChange True True rst10 rst7
{-# NOINLINE topEntity_NC_NC_T_T_BC #-}
{-# ANN topEntity_NC_NC_T_T_BC (defSyn "topEntity_NC_NC_T_T_BC") #-}

-- Expected output generation for all configurations with Template Haskell
simOutA_WF_WF_F_F_AB = $(collectSimResults (length opsA+1) $ pack <$> (fst $ topOut WriteFirst WriteFirst False False rst20 rst10 clk20 clk10))
{-# INLINE simOutA_WF_WF_F_F_AB #-}
simOutA_WF_WF_F_T_AB = $(collectSimResults (length opsA+1) $ pack <$> (fst $ topOut WriteFirst WriteFirst False True rst20 rst10 clk20 clk10))
{-# INLINE simOutA_WF_WF_F_T_AB #-}
simOutA_WF_WF_T_F_AB = $(collectSimResults (length opsA+1) $ pack <$> (fst $ topOut WriteFirst WriteFirst True False rst20 rst10 clk20 clk10))
{-# INLINE simOutA_WF_WF_T_F_AB #-}
simOutA_WF_WF_T_T_AB = $(collectSimResults (length opsA+1) $ pack <$> (fst $ topOut WriteFirst WriteFirst True True rst20 rst10 clk20 clk10))
{-# INLINE simOutA_WF_WF_T_T_AB #-}
simOutA_WF_WF_F_F_BC = $(collectSimResults (length opsA+1) $ pack <$> (fst $ topOut WriteFirst WriteFirst False False rst10 rst7 clk10 clk7))
{-# INLINE simOutA_WF_WF_F_F_BC #-}
simOutA_WF_WF_F_T_BC = $(collectSimResults (length opsA+1) $ pack <$> (fst $ topOut WriteFirst WriteFirst False True rst10 rst7 clk10 clk7))
{-# INLINE simOutA_WF_WF_F_T_BC #-}
simOutA_WF_WF_T_F_BC = $(collectSimResults (length opsA+1) $ pack <$> (fst $ topOut WriteFirst WriteFirst True False rst10 rst7 clk10 clk7))
{-# INLINE simOutA_WF_WF_T_F_BC #-}
simOutA_WF_WF_T_T_BC = $(collectSimResults (length opsA+1) $ pack <$> (fst $ topOut WriteFirst WriteFirst True True rst10 rst7 clk10 clk7))
{-# INLINE simOutA_WF_WF_T_T_BC #-}
simOutA_WF_RF_F_F_AB = $(collectSimResults (length opsA+1) $ pack <$> (fst $ topOut WriteFirst ReadFirst False False rst20 rst10 clk20 clk10))
{-# INLINE simOutA_WF_RF_F_F_AB #-}
simOutA_WF_RF_F_T_AB = $(collectSimResults (length opsA+1) $ pack <$> (fst $ topOut WriteFirst ReadFirst False True rst20 rst10 clk20 clk10))
{-# INLINE simOutA_WF_RF_F_T_AB #-}
simOutA_WF_RF_T_F_AB = $(collectSimResults (length opsA+1) $ pack <$> (fst $ topOut WriteFirst ReadFirst True False rst20 rst10 clk20 clk10))
{-# INLINE simOutA_WF_RF_T_F_AB #-}
simOutA_WF_RF_T_T_AB = $(collectSimResults (length opsA+1) $ pack <$> (fst $ topOut WriteFirst ReadFirst True True rst20 rst10 clk20 clk10))
{-# INLINE simOutA_WF_RF_T_T_AB #-}
simOutA_WF_RF_F_F_BC = $(collectSimResults (length opsA+1) $ pack <$> (fst $ topOut WriteFirst ReadFirst False False rst10 rst7 clk10 clk7))
{-# INLINE simOutA_WF_RF_F_F_BC #-}
simOutA_WF_RF_F_T_BC = $(collectSimResults (length opsA+1) $ pack <$> (fst $ topOut WriteFirst ReadFirst False True rst10 rst7 clk10 clk7))
{-# INLINE simOutA_WF_RF_F_T_BC #-}
simOutA_WF_RF_T_F_BC = $(collectSimResults (length opsA+1) $ pack <$> (fst $ topOut WriteFirst ReadFirst True False rst10 rst7 clk10 clk7))
{-# INLINE simOutA_WF_RF_T_F_BC #-}
simOutA_WF_RF_T_T_BC = $(collectSimResults (length opsA+1) $ pack <$> (fst $ topOut WriteFirst ReadFirst True True rst10 rst7 clk10 clk7))
{-# INLINE simOutA_WF_RF_T_T_BC #-}
simOutA_WF_NC_F_F_AB = $(collectSimResults (length opsA+1) $ pack <$> (fst $ topOut WriteFirst NoChange False False rst20 rst10 clk20 clk10))
{-# INLINE simOutA_WF_NC_F_F_AB #-}
simOutA_WF_NC_F_T_AB = $(collectSimResults (length opsA+1) $ pack <$> (fst $ topOut WriteFirst NoChange False True rst20 rst10 clk20 clk10))
{-# INLINE simOutA_WF_NC_F_T_AB #-}
simOutA_WF_NC_T_F_AB = $(collectSimResults (length opsA+1) $ pack <$> (fst $ topOut WriteFirst NoChange True False rst20 rst10 clk20 clk10))
{-# INLINE simOutA_WF_NC_T_F_AB #-}
simOutA_WF_NC_T_T_AB = $(collectSimResults (length opsA+1) $ pack <$> (fst $ topOut WriteFirst NoChange True True rst20 rst10 clk20 clk10))
{-# INLINE simOutA_WF_NC_T_T_AB #-}
simOutA_WF_NC_F_F_BC = $(collectSimResults (length opsA+1) $ pack <$> (fst $ topOut WriteFirst NoChange False False rst10 rst7 clk10 clk7))
{-# INLINE simOutA_WF_NC_F_F_BC #-}
simOutA_WF_NC_F_T_BC = $(collectSimResults (length opsA+1) $ pack <$> (fst $ topOut WriteFirst NoChange False True rst10 rst7 clk10 clk7))
{-# INLINE simOutA_WF_NC_F_T_BC #-}
simOutA_WF_NC_T_F_BC = $(collectSimResults (length opsA+1) $ pack <$> (fst $ topOut WriteFirst NoChange True False rst10 rst7 clk10 clk7))
{-# INLINE simOutA_WF_NC_T_F_BC #-}
simOutA_WF_NC_T_T_BC = $(collectSimResults (length opsA+1) $ pack <$> (fst $ topOut WriteFirst NoChange True True rst10 rst7 clk10 clk7))
{-# INLINE simOutA_WF_NC_T_T_BC #-}
simOutA_RF_WF_F_F_AB = $(collectSimResults (length opsA+1) $ pack <$> (fst $ topOut ReadFirst WriteFirst False False rst20 rst10 clk20 clk10))
{-# INLINE simOutA_RF_WF_F_F_AB #-}
simOutA_RF_WF_F_T_AB = $(collectSimResults (length opsA+1) $ pack <$> (fst $ topOut ReadFirst WriteFirst False True rst20 rst10 clk20 clk10))
{-# INLINE simOutA_RF_WF_F_T_AB #-}
simOutA_RF_WF_T_F_AB = $(collectSimResults (length opsA+1) $ pack <$> (fst $ topOut ReadFirst WriteFirst True False rst20 rst10 clk20 clk10))
{-# INLINE simOutA_RF_WF_T_F_AB #-}
simOutA_RF_WF_T_T_AB = $(collectSimResults (length opsA+1) $ pack <$> (fst $ topOut ReadFirst WriteFirst True True rst20 rst10 clk20 clk10))
{-# INLINE simOutA_RF_WF_T_T_AB #-}
simOutA_RF_WF_F_F_BC = $(collectSimResults (length opsA+1) $ pack <$> (fst $ topOut ReadFirst WriteFirst False False rst10 rst7 clk10 clk7))
{-# INLINE simOutA_RF_WF_F_F_BC #-}
simOutA_RF_WF_F_T_BC = $(collectSimResults (length opsA+1) $ pack <$> (fst $ topOut ReadFirst WriteFirst False True rst10 rst7 clk10 clk7))
{-# INLINE simOutA_RF_WF_F_T_BC #-}
simOutA_RF_WF_T_F_BC = $(collectSimResults (length opsA+1) $ pack <$> (fst $ topOut ReadFirst WriteFirst True False rst10 rst7 clk10 clk7))
{-# INLINE simOutA_RF_WF_T_F_BC #-}
simOutA_RF_WF_T_T_BC = $(collectSimResults (length opsA+1) $ pack <$> (fst $ topOut ReadFirst WriteFirst True True rst10 rst7 clk10 clk7))
{-# INLINE simOutA_RF_WF_T_T_BC #-}
simOutA_RF_RF_F_F_AB = $(collectSimResults (length opsA+1) $ pack <$> (fst $ topOut ReadFirst ReadFirst False False rst20 rst10 clk20 clk10))
{-# INLINE simOutA_RF_RF_F_F_AB #-}
simOutA_RF_RF_F_T_AB = $(collectSimResults (length opsA+1) $ pack <$> (fst $ topOut ReadFirst ReadFirst False True rst20 rst10 clk20 clk10))
{-# INLINE simOutA_RF_RF_F_T_AB #-}
simOutA_RF_RF_T_F_AB = $(collectSimResults (length opsA+1) $ pack <$> (fst $ topOut ReadFirst ReadFirst True False rst20 rst10 clk20 clk10))
{-# INLINE simOutA_RF_RF_T_F_AB #-}
simOutA_RF_RF_T_T_AB = $(collectSimResults (length opsA+1) $ pack <$> (fst $ topOut ReadFirst ReadFirst True True rst20 rst10 clk20 clk10))
{-# INLINE simOutA_RF_RF_T_T_AB #-}
simOutA_RF_RF_F_F_BC = $(collectSimResults (length opsA+1) $ pack <$> (fst $ topOut ReadFirst ReadFirst False False rst10 rst7 clk10 clk7))
{-# INLINE simOutA_RF_RF_F_F_BC #-}
simOutA_RF_RF_F_T_BC = $(collectSimResults (length opsA+1) $ pack <$> (fst $ topOut ReadFirst ReadFirst False True rst10 rst7 clk10 clk7))
{-# INLINE simOutA_RF_RF_F_T_BC #-}
simOutA_RF_RF_T_F_BC = $(collectSimResults (length opsA+1) $ pack <$> (fst $ topOut ReadFirst ReadFirst True False rst10 rst7 clk10 clk7))
{-# INLINE simOutA_RF_RF_T_F_BC #-}
simOutA_RF_RF_T_T_BC = $(collectSimResults (length opsA+1) $ pack <$> (fst $ topOut ReadFirst ReadFirst True True rst10 rst7 clk10 clk7))
{-# INLINE simOutA_RF_RF_T_T_BC #-}
simOutA_RF_NC_F_F_AB = $(collectSimResults (length opsA+1) $ pack <$> (fst $ topOut ReadFirst NoChange False False rst20 rst10 clk20 clk10))
{-# INLINE simOutA_RF_NC_F_F_AB #-}
simOutA_RF_NC_F_T_AB = $(collectSimResults (length opsA+1) $ pack <$> (fst $ topOut ReadFirst NoChange False True rst20 rst10 clk20 clk10))
{-# INLINE simOutA_RF_NC_F_T_AB #-}
simOutA_RF_NC_T_F_AB = $(collectSimResults (length opsA+1) $ pack <$> (fst $ topOut ReadFirst NoChange True False rst20 rst10 clk20 clk10))
{-# INLINE simOutA_RF_NC_T_F_AB #-}
simOutA_RF_NC_T_T_AB = $(collectSimResults (length opsA+1) $ pack <$> (fst $ topOut ReadFirst NoChange True True rst20 rst10 clk20 clk10))
{-# INLINE simOutA_RF_NC_T_T_AB #-}
simOutA_RF_NC_F_F_BC = $(collectSimResults (length opsA+1) $ pack <$> (fst $ topOut ReadFirst NoChange False False rst10 rst7 clk10 clk7))
{-# INLINE simOutA_RF_NC_F_F_BC #-}
simOutA_RF_NC_F_T_BC = $(collectSimResults (length opsA+1) $ pack <$> (fst $ topOut ReadFirst NoChange False True rst10 rst7 clk10 clk7))
{-# INLINE simOutA_RF_NC_F_T_BC #-}
simOutA_RF_NC_T_F_BC = $(collectSimResults (length opsA+1) $ pack <$> (fst $ topOut ReadFirst NoChange True False rst10 rst7 clk10 clk7))
{-# INLINE simOutA_RF_NC_T_F_BC #-}
simOutA_RF_NC_T_T_BC = $(collectSimResults (length opsA+1) $ pack <$> (fst $ topOut ReadFirst NoChange True True rst10 rst7 clk10 clk7))
{-# INLINE simOutA_RF_NC_T_T_BC #-}
simOutA_NC_WF_F_F_AB = $(collectSimResults (length opsA+1) $ pack <$> (fst $ topOut NoChange WriteFirst False False rst20 rst10 clk20 clk10))
{-# INLINE simOutA_NC_WF_F_F_AB #-}
simOutA_NC_WF_F_T_AB = $(collectSimResults (length opsA+1) $ pack <$> (fst $ topOut NoChange WriteFirst False True rst20 rst10 clk20 clk10))
{-# INLINE simOutA_NC_WF_F_T_AB #-}
simOutA_NC_WF_T_F_AB = $(collectSimResults (length opsA+1) $ pack <$> (fst $ topOut NoChange WriteFirst True False rst20 rst10 clk20 clk10))
{-# INLINE simOutA_NC_WF_T_F_AB #-}
simOutA_NC_WF_T_T_AB = $(collectSimResults (length opsA+1) $ pack <$> (fst $ topOut NoChange WriteFirst True True rst20 rst10 clk20 clk10))
{-# INLINE simOutA_NC_WF_T_T_AB #-}
simOutA_NC_WF_F_F_BC = $(collectSimResults (length opsA+1) $ pack <$> (fst $ topOut NoChange WriteFirst False False rst10 rst7 clk10 clk7))
{-# INLINE simOutA_NC_WF_F_F_BC #-}
simOutA_NC_WF_F_T_BC = $(collectSimResults (length opsA+1) $ pack <$> (fst $ topOut NoChange WriteFirst False True rst10 rst7 clk10 clk7))
{-# INLINE simOutA_NC_WF_F_T_BC #-}
simOutA_NC_WF_T_F_BC = $(collectSimResults (length opsA+1) $ pack <$> (fst $ topOut NoChange WriteFirst True False rst10 rst7 clk10 clk7))
{-# INLINE simOutA_NC_WF_T_F_BC #-}
simOutA_NC_WF_T_T_BC = $(collectSimResults (length opsA+1) $ pack <$> (fst $ topOut NoChange WriteFirst True True rst10 rst7 clk10 clk7))
{-# INLINE simOutA_NC_WF_T_T_BC #-}
simOutA_NC_RF_F_F_AB = $(collectSimResults (length opsA+1) $ pack <$> (fst $ topOut NoChange ReadFirst False False rst20 rst10 clk20 clk10))
{-# INLINE simOutA_NC_RF_F_F_AB #-}
simOutA_NC_RF_F_T_AB = $(collectSimResults (length opsA+1) $ pack <$> (fst $ topOut NoChange ReadFirst False True rst20 rst10 clk20 clk10))
{-# INLINE simOutA_NC_RF_F_T_AB #-}
simOutA_NC_RF_T_F_AB = $(collectSimResults (length opsA+1) $ pack <$> (fst $ topOut NoChange ReadFirst True False rst20 rst10 clk20 clk10))
{-# INLINE simOutA_NC_RF_T_F_AB #-}
simOutA_NC_RF_T_T_AB = $(collectSimResults (length opsA+1) $ pack <$> (fst $ topOut NoChange ReadFirst True True rst20 rst10 clk20 clk10))
{-# INLINE simOutA_NC_RF_T_T_AB #-}
simOutA_NC_RF_F_F_BC = $(collectSimResults (length opsA+1) $ pack <$> (fst $ topOut NoChange ReadFirst False False rst10 rst7 clk10 clk7))
{-# INLINE simOutA_NC_RF_F_F_BC #-}
simOutA_NC_RF_F_T_BC = $(collectSimResults (length opsA+1) $ pack <$> (fst $ topOut NoChange ReadFirst False True rst10 rst7 clk10 clk7))
{-# INLINE simOutA_NC_RF_F_T_BC #-}
simOutA_NC_RF_T_F_BC = $(collectSimResults (length opsA+1) $ pack <$> (fst $ topOut NoChange ReadFirst True False rst10 rst7 clk10 clk7))
{-# INLINE simOutA_NC_RF_T_F_BC #-}
simOutA_NC_RF_T_T_BC = $(collectSimResults (length opsA+1) $ pack <$> (fst $ topOut NoChange ReadFirst True True rst10 rst7 clk10 clk7))
{-# INLINE simOutA_NC_RF_T_T_BC #-}
simOutA_NC_NC_F_F_AB = $(collectSimResults (length opsA+1) $ pack <$> (fst $ topOut NoChange NoChange False False rst20 rst10 clk20 clk10))
{-# INLINE simOutA_NC_NC_F_F_AB #-}
simOutA_NC_NC_F_T_AB = $(collectSimResults (length opsA+1) $ pack <$> (fst $ topOut NoChange NoChange False True rst20 rst10 clk20 clk10))
{-# INLINE simOutA_NC_NC_F_T_AB #-}
simOutA_NC_NC_T_F_AB = $(collectSimResults (length opsA+1) $ pack <$> (fst $ topOut NoChange NoChange True False rst20 rst10 clk20 clk10))
{-# INLINE simOutA_NC_NC_T_F_AB #-}
simOutA_NC_NC_T_T_AB = $(collectSimResults (length opsA+1) $ pack <$> (fst $ topOut NoChange NoChange True True rst20 rst10 clk20 clk10))
{-# INLINE simOutA_NC_NC_T_T_AB #-}
simOutA_NC_NC_F_F_BC = $(collectSimResults (length opsA+1) $ pack <$> (fst $ topOut NoChange NoChange False False rst10 rst7 clk10 clk7))
{-# INLINE simOutA_NC_NC_F_F_BC #-}
simOutA_NC_NC_F_T_BC = $(collectSimResults (length opsA+1) $ pack <$> (fst $ topOut NoChange NoChange False True rst10 rst7 clk10 clk7))
{-# INLINE simOutA_NC_NC_F_T_BC #-}
simOutA_NC_NC_T_F_BC = $(collectSimResults (length opsA+1) $ pack <$> (fst $ topOut NoChange NoChange True False rst10 rst7 clk10 clk7))
{-# INLINE simOutA_NC_NC_T_F_BC #-}
simOutA_NC_NC_T_T_BC = $(collectSimResults (length opsA+1) $ pack <$> (fst $ topOut NoChange NoChange True True rst10 rst7 clk10 clk7))
{-# INLINE simOutA_NC_NC_T_T_BC #-}
simOutB_WF_WF_F_F_AB = $(collectSimResults (length opsB+1) $ pack <$> (snd $ topOut WriteFirst WriteFirst False False rst20 rst10 clk20 clk10))
{-# INLINE simOutB_WF_WF_F_F_AB #-}
simOutB_WF_WF_F_T_AB = $(collectSimResults (length opsB+1) $ pack <$> (snd $ topOut WriteFirst WriteFirst False True rst20 rst10 clk20 clk10))
{-# INLINE simOutB_WF_WF_F_T_AB #-}
simOutB_WF_WF_T_F_AB = $(collectSimResults (length opsB+1) $ pack <$> (snd $ topOut WriteFirst WriteFirst True False rst20 rst10 clk20 clk10))
{-# INLINE simOutB_WF_WF_T_F_AB #-}
simOutB_WF_WF_T_T_AB = $(collectSimResults (length opsB+1) $ pack <$> (snd $ topOut WriteFirst WriteFirst True True rst20 rst10 clk20 clk10))
{-# INLINE simOutB_WF_WF_T_T_AB #-}
simOutB_WF_WF_F_F_BC = $(collectSimResults (length opsB+1) $ pack <$> (snd $ topOut WriteFirst WriteFirst False False rst10 rst7 clk10 clk7))
{-# INLINE simOutB_WF_WF_F_F_BC #-}
simOutB_WF_WF_F_T_BC = $(collectSimResults (length opsB+1) $ pack <$> (snd $ topOut WriteFirst WriteFirst False True rst10 rst7 clk10 clk7))
{-# INLINE simOutB_WF_WF_F_T_BC #-}
simOutB_WF_WF_T_F_BC = $(collectSimResults (length opsB+1) $ pack <$> (snd $ topOut WriteFirst WriteFirst True False rst10 rst7 clk10 clk7))
{-# INLINE simOutB_WF_WF_T_F_BC #-}
simOutB_WF_WF_T_T_BC = $(collectSimResults (length opsB+1) $ pack <$> (snd $ topOut WriteFirst WriteFirst True True rst10 rst7 clk10 clk7))
{-# INLINE simOutB_WF_WF_T_T_BC #-}
simOutB_WF_RF_F_F_AB = $(collectSimResults (length opsB+1) $ pack <$> (snd $ topOut WriteFirst ReadFirst False False rst20 rst10 clk20 clk10))
{-# INLINE simOutB_WF_RF_F_F_AB #-}
simOutB_WF_RF_F_T_AB = $(collectSimResults (length opsB+1) $ pack <$> (snd $ topOut WriteFirst ReadFirst False True rst20 rst10 clk20 clk10))
{-# INLINE simOutB_WF_RF_F_T_AB #-}
simOutB_WF_RF_T_F_AB = $(collectSimResults (length opsB+1) $ pack <$> (snd $ topOut WriteFirst ReadFirst True False rst20 rst10 clk20 clk10))
{-# INLINE simOutB_WF_RF_T_F_AB #-}
simOutB_WF_RF_T_T_AB = $(collectSimResults (length opsB+1) $ pack <$> (snd $ topOut WriteFirst ReadFirst True True rst20 rst10 clk20 clk10))
{-# INLINE simOutB_WF_RF_T_T_AB #-}
simOutB_WF_RF_F_F_BC = $(collectSimResults (length opsB+1) $ pack <$> (snd $ topOut WriteFirst ReadFirst False False rst10 rst7 clk10 clk7))
{-# INLINE simOutB_WF_RF_F_F_BC #-}
simOutB_WF_RF_F_T_BC = $(collectSimResults (length opsB+1) $ pack <$> (snd $ topOut WriteFirst ReadFirst False True rst10 rst7 clk10 clk7))
{-# INLINE simOutB_WF_RF_F_T_BC #-}
simOutB_WF_RF_T_F_BC = $(collectSimResults (length opsB+1) $ pack <$> (snd $ topOut WriteFirst ReadFirst True False rst10 rst7 clk10 clk7))
{-# INLINE simOutB_WF_RF_T_F_BC #-}
simOutB_WF_RF_T_T_BC = $(collectSimResults (length opsB+1) $ pack <$> (snd $ topOut WriteFirst ReadFirst True True rst10 rst7 clk10 clk7))
{-# INLINE simOutB_WF_RF_T_T_BC #-}
simOutB_WF_NC_F_F_AB = $(collectSimResults (length opsB+1) $ pack <$> (snd $ topOut WriteFirst NoChange False False rst20 rst10 clk20 clk10))
{-# INLINE simOutB_WF_NC_F_F_AB #-}
simOutB_WF_NC_F_T_AB = $(collectSimResults (length opsB+1) $ pack <$> (snd $ topOut WriteFirst NoChange False True rst20 rst10 clk20 clk10))
{-# INLINE simOutB_WF_NC_F_T_AB #-}
simOutB_WF_NC_T_F_AB = $(collectSimResults (length opsB+1) $ pack <$> (snd $ topOut WriteFirst NoChange True False rst20 rst10 clk20 clk10))
{-# INLINE simOutB_WF_NC_T_F_AB #-}
simOutB_WF_NC_T_T_AB = $(collectSimResults (length opsB+1) $ pack <$> (snd $ topOut WriteFirst NoChange True True rst20 rst10 clk20 clk10))
{-# INLINE simOutB_WF_NC_T_T_AB #-}
simOutB_WF_NC_F_F_BC = $(collectSimResults (length opsB+1) $ pack <$> (snd $ topOut WriteFirst NoChange False False rst10 rst7 clk10 clk7))
{-# INLINE simOutB_WF_NC_F_F_BC #-}
simOutB_WF_NC_F_T_BC = $(collectSimResults (length opsB+1) $ pack <$> (snd $ topOut WriteFirst NoChange False True rst10 rst7 clk10 clk7))
{-# INLINE simOutB_WF_NC_F_T_BC #-}
simOutB_WF_NC_T_F_BC = $(collectSimResults (length opsB+1) $ pack <$> (snd $ topOut WriteFirst NoChange True False rst10 rst7 clk10 clk7))
{-# INLINE simOutB_WF_NC_T_F_BC #-}
simOutB_WF_NC_T_T_BC = $(collectSimResults (length opsB+1) $ pack <$> (snd $ topOut WriteFirst NoChange True True rst10 rst7 clk10 clk7))
{-# INLINE simOutB_WF_NC_T_T_BC #-}
simOutB_RF_WF_F_F_AB = $(collectSimResults (length opsB+1) $ pack <$> (snd $ topOut ReadFirst WriteFirst False False rst20 rst10 clk20 clk10))
{-# INLINE simOutB_RF_WF_F_F_AB #-}
simOutB_RF_WF_F_T_AB = $(collectSimResults (length opsB+1) $ pack <$> (snd $ topOut ReadFirst WriteFirst False True rst20 rst10 clk20 clk10))
{-# INLINE simOutB_RF_WF_F_T_AB #-}
simOutB_RF_WF_T_F_AB = $(collectSimResults (length opsB+1) $ pack <$> (snd $ topOut ReadFirst WriteFirst True False rst20 rst10 clk20 clk10))
{-# INLINE simOutB_RF_WF_T_F_AB #-}
simOutB_RF_WF_T_T_AB = $(collectSimResults (length opsB+1) $ pack <$> (snd $ topOut ReadFirst WriteFirst True True rst20 rst10 clk20 clk10))
{-# INLINE simOutB_RF_WF_T_T_AB #-}
simOutB_RF_WF_F_F_BC = $(collectSimResults (length opsB+1) $ pack <$> (snd $ topOut ReadFirst WriteFirst False False rst10 rst7 clk10 clk7))
{-# INLINE simOutB_RF_WF_F_F_BC #-}
simOutB_RF_WF_F_T_BC = $(collectSimResults (length opsB+1) $ pack <$> (snd $ topOut ReadFirst WriteFirst False True rst10 rst7 clk10 clk7))
{-# INLINE simOutB_RF_WF_F_T_BC #-}
simOutB_RF_WF_T_F_BC = $(collectSimResults (length opsB+1) $ pack <$> (snd $ topOut ReadFirst WriteFirst True False rst10 rst7 clk10 clk7))
{-# INLINE simOutB_RF_WF_T_F_BC #-}
simOutB_RF_WF_T_T_BC = $(collectSimResults (length opsB+1) $ pack <$> (snd $ topOut ReadFirst WriteFirst True True rst10 rst7 clk10 clk7))
{-# INLINE simOutB_RF_WF_T_T_BC #-}
simOutB_RF_RF_F_F_AB = $(collectSimResults (length opsB+1) $ pack <$> (snd $ topOut ReadFirst ReadFirst False False rst20 rst10 clk20 clk10))
{-# INLINE simOutB_RF_RF_F_F_AB #-}
simOutB_RF_RF_F_T_AB = $(collectSimResults (length opsB+1) $ pack <$> (snd $ topOut ReadFirst ReadFirst False True rst20 rst10 clk20 clk10))
{-# INLINE simOutB_RF_RF_F_T_AB #-}
simOutB_RF_RF_T_F_AB = $(collectSimResults (length opsB+1) $ pack <$> (snd $ topOut ReadFirst ReadFirst True False rst20 rst10 clk20 clk10))
{-# INLINE simOutB_RF_RF_T_F_AB #-}
simOutB_RF_RF_T_T_AB = $(collectSimResults (length opsB+1) $ pack <$> (snd $ topOut ReadFirst ReadFirst True True rst20 rst10 clk20 clk10))
{-# INLINE simOutB_RF_RF_T_T_AB #-}
simOutB_RF_RF_F_F_BC = $(collectSimResults (length opsB+1) $ pack <$> (snd $ topOut ReadFirst ReadFirst False False rst10 rst7 clk10 clk7))
{-# INLINE simOutB_RF_RF_F_F_BC #-}
simOutB_RF_RF_F_T_BC = $(collectSimResults (length opsB+1) $ pack <$> (snd $ topOut ReadFirst ReadFirst False True rst10 rst7 clk10 clk7))
{-# INLINE simOutB_RF_RF_F_T_BC #-}
simOutB_RF_RF_T_F_BC = $(collectSimResults (length opsB+1) $ pack <$> (snd $ topOut ReadFirst ReadFirst True False rst10 rst7 clk10 clk7))
{-# INLINE simOutB_RF_RF_T_F_BC #-}
simOutB_RF_RF_T_T_BC = $(collectSimResults (length opsB+1) $ pack <$> (snd $ topOut ReadFirst ReadFirst True True rst10 rst7 clk10 clk7))
{-# INLINE simOutB_RF_RF_T_T_BC #-}
simOutB_RF_NC_F_F_AB = $(collectSimResults (length opsB+1) $ pack <$> (snd $ topOut ReadFirst NoChange False False rst20 rst10 clk20 clk10))
{-# INLINE simOutB_RF_NC_F_F_AB #-}
simOutB_RF_NC_F_T_AB = $(collectSimResults (length opsB+1) $ pack <$> (snd $ topOut ReadFirst NoChange False True rst20 rst10 clk20 clk10))
{-# INLINE simOutB_RF_NC_F_T_AB #-}
simOutB_RF_NC_T_F_AB = $(collectSimResults (length opsB+1) $ pack <$> (snd $ topOut ReadFirst NoChange True False rst20 rst10 clk20 clk10))
{-# INLINE simOutB_RF_NC_T_F_AB #-}
simOutB_RF_NC_T_T_AB = $(collectSimResults (length opsB+1) $ pack <$> (snd $ topOut ReadFirst NoChange True True rst20 rst10 clk20 clk10))
{-# INLINE simOutB_RF_NC_T_T_AB #-}
simOutB_RF_NC_F_F_BC = $(collectSimResults (length opsB+1) $ pack <$> (snd $ topOut ReadFirst NoChange False False rst10 rst7 clk10 clk7))
{-# INLINE simOutB_RF_NC_F_F_BC #-}
simOutB_RF_NC_F_T_BC = $(collectSimResults (length opsB+1) $ pack <$> (snd $ topOut ReadFirst NoChange False True rst10 rst7 clk10 clk7))
{-# INLINE simOutB_RF_NC_F_T_BC #-}
simOutB_RF_NC_T_F_BC = $(collectSimResults (length opsB+1) $ pack <$> (snd $ topOut ReadFirst NoChange True False rst10 rst7 clk10 clk7))
{-# INLINE simOutB_RF_NC_T_F_BC #-}
simOutB_RF_NC_T_T_BC = $(collectSimResults (length opsB+1) $ pack <$> (snd $ topOut ReadFirst NoChange True True rst10 rst7 clk10 clk7))
{-# INLINE simOutB_RF_NC_T_T_BC #-}
simOutB_NC_WF_F_F_AB = $(collectSimResults (length opsB+1) $ pack <$> (snd $ topOut NoChange WriteFirst False False rst20 rst10 clk20 clk10))
{-# INLINE simOutB_NC_WF_F_F_AB #-}
simOutB_NC_WF_F_T_AB = $(collectSimResults (length opsB+1) $ pack <$> (snd $ topOut NoChange WriteFirst False True rst20 rst10 clk20 clk10))
{-# INLINE simOutB_NC_WF_F_T_AB #-}
simOutB_NC_WF_T_F_AB = $(collectSimResults (length opsB+1) $ pack <$> (snd $ topOut NoChange WriteFirst True False rst20 rst10 clk20 clk10))
{-# INLINE simOutB_NC_WF_T_F_AB #-}
simOutB_NC_WF_T_T_AB = $(collectSimResults (length opsB+1) $ pack <$> (snd $ topOut NoChange WriteFirst True True rst20 rst10 clk20 clk10))
{-# INLINE simOutB_NC_WF_T_T_AB #-}
simOutB_NC_WF_F_F_BC = $(collectSimResults (length opsB+1) $ pack <$> (snd $ topOut NoChange WriteFirst False False rst10 rst7 clk10 clk7))
{-# INLINE simOutB_NC_WF_F_F_BC #-}
simOutB_NC_WF_F_T_BC = $(collectSimResults (length opsB+1) $ pack <$> (snd $ topOut NoChange WriteFirst False True rst10 rst7 clk10 clk7))
{-# INLINE simOutB_NC_WF_F_T_BC #-}
simOutB_NC_WF_T_F_BC = $(collectSimResults (length opsB+1) $ pack <$> (snd $ topOut NoChange WriteFirst True False rst10 rst7 clk10 clk7))
{-# INLINE simOutB_NC_WF_T_F_BC #-}
simOutB_NC_WF_T_T_BC = $(collectSimResults (length opsB+1) $ pack <$> (snd $ topOut NoChange WriteFirst True True rst10 rst7 clk10 clk7))
{-# INLINE simOutB_NC_WF_T_T_BC #-}
simOutB_NC_RF_F_F_AB = $(collectSimResults (length opsB+1) $ pack <$> (snd $ topOut NoChange ReadFirst False False rst20 rst10 clk20 clk10))
{-# INLINE simOutB_NC_RF_F_F_AB #-}
simOutB_NC_RF_F_T_AB = $(collectSimResults (length opsB+1) $ pack <$> (snd $ topOut NoChange ReadFirst False True rst20 rst10 clk20 clk10))
{-# INLINE simOutB_NC_RF_F_T_AB #-}
simOutB_NC_RF_T_F_AB = $(collectSimResults (length opsB+1) $ pack <$> (snd $ topOut NoChange ReadFirst True False rst20 rst10 clk20 clk10))
{-# INLINE simOutB_NC_RF_T_F_AB #-}
simOutB_NC_RF_T_T_AB = $(collectSimResults (length opsB+1) $ pack <$> (snd $ topOut NoChange ReadFirst True True rst20 rst10 clk20 clk10))
{-# INLINE simOutB_NC_RF_T_T_AB #-}
simOutB_NC_RF_F_F_BC = $(collectSimResults (length opsB+1) $ pack <$> (snd $ topOut NoChange ReadFirst False False rst10 rst7 clk10 clk7))
{-# INLINE simOutB_NC_RF_F_F_BC #-}
simOutB_NC_RF_F_T_BC = $(collectSimResults (length opsB+1) $ pack <$> (snd $ topOut NoChange ReadFirst False True rst10 rst7 clk10 clk7))
{-# INLINE simOutB_NC_RF_F_T_BC #-}
simOutB_NC_RF_T_F_BC = $(collectSimResults (length opsB+1) $ pack <$> (snd $ topOut NoChange ReadFirst True False rst10 rst7 clk10 clk7))
{-# INLINE simOutB_NC_RF_T_F_BC #-}
simOutB_NC_RF_T_T_BC = $(collectSimResults (length opsB+1) $ pack <$> (snd $ topOut NoChange ReadFirst True True rst10 rst7 clk10 clk7))
{-# INLINE simOutB_NC_RF_T_T_BC #-}
simOutB_NC_NC_F_F_AB = $(collectSimResults (length opsB+1) $ pack <$> (snd $ topOut NoChange NoChange False False rst20 rst10 clk20 clk10))
{-# INLINE simOutB_NC_NC_F_F_AB #-}
simOutB_NC_NC_F_T_AB = $(collectSimResults (length opsB+1) $ pack <$> (snd $ topOut NoChange NoChange False True rst20 rst10 clk20 clk10))
{-# INLINE simOutB_NC_NC_F_T_AB #-}
simOutB_NC_NC_T_F_AB = $(collectSimResults (length opsB+1) $ pack <$> (snd $ topOut NoChange NoChange True False rst20 rst10 clk20 clk10))
{-# INLINE simOutB_NC_NC_T_F_AB #-}
simOutB_NC_NC_T_T_AB = $(collectSimResults (length opsB+1) $ pack <$> (snd $ topOut NoChange NoChange True True rst20 rst10 clk20 clk10))
{-# INLINE simOutB_NC_NC_T_T_AB #-}
simOutB_NC_NC_F_F_BC = $(collectSimResults (length opsB+1) $ pack <$> (snd $ topOut NoChange NoChange False False rst10 rst7 clk10 clk7))
{-# INLINE simOutB_NC_NC_F_F_BC #-}
simOutB_NC_NC_F_T_BC = $(collectSimResults (length opsB+1) $ pack <$> (snd $ topOut NoChange NoChange False True rst10 rst7 clk10 clk7))
{-# INLINE simOutB_NC_NC_F_T_BC #-}
simOutB_NC_NC_T_F_BC = $(collectSimResults (length opsB+1) $ pack <$> (snd $ topOut NoChange NoChange True False rst10 rst7 clk10 clk7))
{-# INLINE simOutB_NC_NC_T_F_BC #-}
simOutB_NC_NC_T_T_BC = $(collectSimResults (length opsB+1) $ pack <$> (snd $ topOut NoChange NoChange True True rst10 rst7 clk10 clk7))
{-# INLINE simOutB_NC_NC_T_T_BC #-}
