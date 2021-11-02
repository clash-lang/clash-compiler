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

testBench  wmA wmB rstA rstB simOutA simOutB =
 strictAnd <$> doneA <*> (unsafeSynchronizer clkB clkA doneB)
  where
    --topEntity output
    (portA, portB) = topOut clkA clkB wmA wmB rstA rstB
    actualOutputA = ignoreFor clkA rstA enableGen d1 undefined portA
    actualOutputB = ignoreFor clkB rstB enableGen d1 undefined portB

    --Verification
    outputVerifierA = outputVerifierWith
     (\clk rst -> assertBitVector clk rst "outputVerifierBitVector Port A")
    outoutVerifierB = outputVerifierWith
     (\clk rst -> assertBitVector clk rst "outputVerifierBitVector Port B")

    doneA  = outputVerifierA clkA rstA simOutA $ pack <$> actualOutputA
    doneA' = not <$> doneA
    doneB  = outoutVerifierB clkB rstB simOutB $ pack <$> actualOutputB
    doneB' = not <$> doneB

    (clkA, clkB) = (tbClockGen doneA', tbClockGen doneB')
    -- Testbench clocks

-- testBench  configurations
testBench_WF_WF_AB = testBench WriteFirst WriteFirst rst20 rst10 simOutA_WF_WF_AB simOutB_WF_WF_AB
testBench_WF_WF_BC = testBench WriteFirst WriteFirst rst10 rst7 simOutA_WF_WF_BC simOutB_WF_WF_BC
testBench_WF_RF_AB = testBench WriteFirst ReadFirst rst20 rst10 simOutA_WF_RF_AB simOutB_WF_RF_AB
testBench_WF_RF_BC = testBench WriteFirst ReadFirst rst10 rst7 simOutA_WF_RF_BC simOutB_WF_RF_BC
testBench_WF_NC_AB = testBench WriteFirst NoChange rst20 rst10 simOutA_WF_NC_AB simOutB_WF_NC_AB
testBench_WF_NC_BC = testBench WriteFirst NoChange rst10 rst7 simOutA_WF_NC_BC simOutB_WF_NC_BC
testBench_RF_WF_AB = testBench ReadFirst WriteFirst rst20 rst10 simOutA_RF_WF_AB simOutB_RF_WF_AB
testBench_RF_WF_BC = testBench ReadFirst WriteFirst rst10 rst7 simOutA_RF_WF_BC simOutB_RF_WF_BC
testBench_RF_RF_AB = testBench ReadFirst ReadFirst rst20 rst10 simOutA_RF_RF_AB simOutB_RF_RF_AB
testBench_RF_RF_BC = testBench ReadFirst ReadFirst rst10 rst7 simOutA_RF_RF_BC simOutB_RF_RF_BC
testBench_RF_NC_AB = testBench ReadFirst NoChange rst20 rst10 simOutA_RF_NC_AB simOutB_RF_NC_AB
testBench_RF_NC_BC = testBench ReadFirst NoChange rst10 rst7 simOutA_RF_NC_BC simOutB_RF_NC_BC
testBench_NC_WF_AB = testBench NoChange WriteFirst rst20 rst10 simOutA_NC_WF_AB simOutB_NC_WF_AB
testBench_NC_WF_BC = testBench NoChange WriteFirst rst10 rst7 simOutA_NC_WF_BC simOutB_NC_WF_BC
testBench_NC_RF_AB = testBench NoChange ReadFirst rst20 rst10 simOutA_NC_RF_AB simOutB_NC_RF_AB
testBench_NC_RF_BC = testBench NoChange ReadFirst rst10 rst7 simOutA_NC_RF_BC simOutB_NC_RF_BC
testBench_NC_NC_AB = testBench NoChange NoChange rst20 rst10 simOutA_NC_NC_AB simOutB_NC_NC_AB
testBench_NC_NC_BC = testBench NoChange NoChange rst10 rst7 simOutA_NC_NC_BC simOutB_NC_NC_BC

-- Expected output generation for all configurations with Template Haskell
processSimOutput x = replace (0 ::Integer) undefined# $ tail x
simOutA_WF_WF_AB = processSimOutput $(collectSimResults 300 $ pack <$> (fst $ topOut clk20 clk10 WriteFirst WriteFirst rst20 rst10))
simOutA_WF_WF_BC = processSimOutput $(collectSimResults 300 $ pack <$> (fst $ topOut clk10 clk7 WriteFirst WriteFirst rst10 rst7))
simOutA_WF_RF_AB = processSimOutput $(collectSimResults 300 $ pack <$> (fst $ topOut clk20 clk10 WriteFirst ReadFirst rst20 rst10))
simOutA_WF_RF_BC = processSimOutput $(collectSimResults 300 $ pack <$> (fst $ topOut clk10 clk7 WriteFirst ReadFirst rst10 rst7))
simOutA_WF_NC_AB = processSimOutput $(collectSimResults 300 $ pack <$> (fst $ topOut clk20 clk10 WriteFirst NoChange rst20 rst10))
simOutA_WF_NC_BC = processSimOutput $(collectSimResults 300 $ pack <$> (fst $ topOut clk10 clk7 WriteFirst NoChange rst10 rst7))
simOutA_RF_WF_AB = processSimOutput $(collectSimResults 300 $ pack <$> (fst $ topOut clk20 clk10 ReadFirst WriteFirst rst20 rst10))
simOutA_RF_WF_BC = processSimOutput $(collectSimResults 300 $ pack <$> (fst $ topOut clk10 clk7 ReadFirst WriteFirst rst10 rst7))
simOutA_RF_RF_AB = processSimOutput $(collectSimResults 300 $ pack <$> (fst $ topOut clk20 clk10 ReadFirst ReadFirst rst20 rst10))
simOutA_RF_RF_BC = processSimOutput $(collectSimResults 300 $ pack <$> (fst $ topOut clk10 clk7 ReadFirst ReadFirst rst10 rst7))
simOutA_RF_NC_AB = processSimOutput $(collectSimResults 300 $ pack <$> (fst $ topOut clk20 clk10 ReadFirst NoChange rst20 rst10))
simOutA_RF_NC_BC = processSimOutput $(collectSimResults 300 $ pack <$> (fst $ topOut clk10 clk7 ReadFirst NoChange rst10 rst7))
simOutA_NC_WF_AB = processSimOutput $(collectSimResults 300 $ pack <$> (fst $ topOut clk20 clk10 NoChange WriteFirst rst20 rst10))
simOutA_NC_WF_BC = processSimOutput $(collectSimResults 300 $ pack <$> (fst $ topOut clk10 clk7 NoChange WriteFirst rst10 rst7))
simOutA_NC_RF_AB = processSimOutput $(collectSimResults 300 $ pack <$> (fst $ topOut clk20 clk10 NoChange ReadFirst rst20 rst10))
simOutA_NC_RF_BC = processSimOutput $(collectSimResults 300 $ pack <$> (fst $ topOut clk10 clk7 NoChange ReadFirst rst10 rst7))
simOutA_NC_NC_AB = processSimOutput $(collectSimResults 300 $ pack <$> (fst $ topOut clk20 clk10 NoChange NoChange rst20 rst10))
simOutA_NC_NC_BC = processSimOutput $(collectSimResults 300 $ pack <$> (fst $ topOut clk10 clk7 NoChange NoChange rst10 rst7))
simOutB_WF_WF_AB = processSimOutput $(collectSimResults 300 $ pack <$> (snd $ topOut clk20 clk10 WriteFirst WriteFirst rst20 rst10))
simOutB_WF_WF_BC = processSimOutput $(collectSimResults 300 $ pack <$> (snd $ topOut clk10 clk7 WriteFirst WriteFirst rst10 rst7))
simOutB_WF_RF_AB = processSimOutput $(collectSimResults 300 $ pack <$> (snd $ topOut clk20 clk10 WriteFirst ReadFirst rst20 rst10))
simOutB_WF_RF_BC = processSimOutput $(collectSimResults 300 $ pack <$> (snd $ topOut clk10 clk7 WriteFirst ReadFirst rst10 rst7))
simOutB_WF_NC_AB = processSimOutput $(collectSimResults 300 $ pack <$> (snd $ topOut clk20 clk10 WriteFirst NoChange rst20 rst10))
simOutB_WF_NC_BC = processSimOutput $(collectSimResults 300 $ pack <$> (snd $ topOut clk10 clk7 WriteFirst NoChange rst10 rst7))
simOutB_RF_WF_AB = processSimOutput $(collectSimResults 300 $ pack <$> (snd $ topOut clk20 clk10 ReadFirst WriteFirst rst20 rst10))
simOutB_RF_WF_BC = processSimOutput $(collectSimResults 300 $ pack <$> (snd $ topOut clk10 clk7 ReadFirst WriteFirst rst10 rst7))
simOutB_RF_RF_AB = processSimOutput $(collectSimResults 300 $ pack <$> (snd $ topOut clk20 clk10 ReadFirst ReadFirst rst20 rst10))
simOutB_RF_RF_BC = processSimOutput $(collectSimResults 300 $ pack <$> (snd $ topOut clk10 clk7 ReadFirst ReadFirst rst10 rst7))
simOutB_RF_NC_AB = processSimOutput $(collectSimResults 300 $ pack <$> (snd $ topOut clk20 clk10 ReadFirst NoChange rst20 rst10))
simOutB_RF_NC_BC = processSimOutput $(collectSimResults 300 $ pack <$> (snd $ topOut clk10 clk7 ReadFirst NoChange rst10 rst7))
simOutB_NC_WF_AB = processSimOutput $(collectSimResults 300 $ pack <$> (snd $ topOut clk20 clk10 NoChange WriteFirst rst20 rst10))
simOutB_NC_WF_BC = processSimOutput $(collectSimResults 300 $ pack <$> (snd $ topOut clk10 clk7 NoChange WriteFirst rst10 rst7))
simOutB_NC_RF_AB = processSimOutput $(collectSimResults 300 $ pack <$> (snd $ topOut clk20 clk10 NoChange ReadFirst rst20 rst10))
simOutB_NC_RF_BC = processSimOutput $(collectSimResults 300 $ pack <$> (snd $ topOut clk10 clk7 NoChange ReadFirst rst10 rst7))
simOutB_NC_NC_AB = processSimOutput $(collectSimResults 300 $ pack <$> (snd $ topOut clk20 clk10 NoChange NoChange rst20 rst10))
simOutB_NC_NC_BC = processSimOutput $(collectSimResults 300 $ pack <$> (snd $ topOut clk10 clk7 NoChange NoChange rst10 rst7))
