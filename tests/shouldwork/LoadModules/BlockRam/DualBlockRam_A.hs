{-# OPTIONS_GHC -fconstraint-solver-iterations=0 #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fplugin=GHC.TypeLits.Extra.Solver #-}
{-# OPTIONS_GHC -fplugin=GHC.TypeLits.KnownNat.Solver #-}
{-# OPTIONS_GHC -fplugin=GHC.TypeLits.Normalise #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-# LANGUAGE NoImplicitPrelude #-}
module DualBlockRam_A where
import Clash.Explicit.Prelude
import Clash.Explicit.BlockRam(WriteMode(..))
import DualBlockRamDefinitions
import Test.Tasty.Clash.CollectSimResults

-- testBench  configurations
testBench_WF_WF_F_F_AB = genTestBench topEntity_WF_WF_F_F_AB rst20 rst10 simOutA_WF_WF_F_F_AB simOutB_WF_WF_F_F_AB
{-# NOINLINE testBench_WF_WF_F_F_AB #-}
{-# ANN testBench_WF_WF_F_F_AB (TestBench 'topEntity_WF_WF_F_F_AB) #-}
testBench_WF_WF_F_F_BC = genTestBench topEntity_WF_WF_F_F_BC rst10 rst7 simOutA_WF_WF_F_F_BC simOutB_WF_WF_F_F_BC
{-# NOINLINE testBench_WF_WF_F_F_BC #-}
{-# ANN testBench_WF_WF_F_F_BC (TestBench 'topEntity_WF_WF_F_F_BC) #-}
testBench_WF_RF_F_F_AB = genTestBench topEntity_WF_RF_F_F_AB rst20 rst10 simOutA_WF_RF_F_F_AB simOutB_WF_RF_F_F_AB
{-# NOINLINE testBench_WF_RF_F_F_AB #-}
{-# ANN testBench_WF_RF_F_F_AB (TestBench 'topEntity_WF_RF_F_F_AB) #-}
testBench_WF_RF_F_F_BC = genTestBench topEntity_WF_RF_F_F_BC rst10 rst7 simOutA_WF_RF_F_F_BC simOutB_WF_RF_F_F_BC
{-# NOINLINE testBench_WF_RF_F_F_BC #-}
{-# ANN testBench_WF_RF_F_F_BC (TestBench 'topEntity_WF_RF_F_F_BC) #-}
testBench_WF_NC_F_F_AB = genTestBench topEntity_WF_NC_F_F_AB rst20 rst10 simOutA_WF_NC_F_F_AB simOutB_WF_NC_F_F_AB
{-# NOINLINE testBench_WF_NC_F_F_AB #-}
{-# ANN testBench_WF_NC_F_F_AB (TestBench 'topEntity_WF_NC_F_F_AB) #-}
testBench_WF_NC_F_F_BC = genTestBench topEntity_WF_NC_F_F_BC rst10 rst7 simOutA_WF_NC_F_F_BC simOutB_WF_NC_F_F_BC
{-# NOINLINE testBench_WF_NC_F_F_BC #-}
{-# ANN testBench_WF_NC_F_F_BC (TestBench 'topEntity_WF_NC_F_F_BC) #-}
testBench_RF_WF_F_F_AB = genTestBench topEntity_RF_WF_F_F_AB rst20 rst10 simOutA_RF_WF_F_F_AB simOutB_RF_WF_F_F_AB
{-# NOINLINE testBench_RF_WF_F_F_AB #-}
{-# ANN testBench_RF_WF_F_F_AB (TestBench 'topEntity_RF_WF_F_F_AB) #-}
testBench_RF_WF_F_F_BC = genTestBench topEntity_RF_WF_F_F_BC rst10 rst7 simOutA_RF_WF_F_F_BC simOutB_RF_WF_F_F_BC
{-# NOINLINE testBench_RF_WF_F_F_BC #-}
{-# ANN testBench_RF_WF_F_F_BC (TestBench 'topEntity_RF_WF_F_F_BC) #-}
testBench_RF_RF_F_F_AB = genTestBench topEntity_RF_RF_F_F_AB rst20 rst10 simOutA_RF_RF_F_F_AB simOutB_RF_RF_F_F_AB
{-# NOINLINE testBench_RF_RF_F_F_AB #-}
{-# ANN testBench_RF_RF_F_F_AB (TestBench 'topEntity_RF_RF_F_F_AB) #-}
testBench_RF_RF_F_F_BC = genTestBench topEntity_RF_RF_F_F_BC rst10 rst7 simOutA_RF_RF_F_F_BC simOutB_RF_RF_F_F_BC
{-# NOINLINE testBench_RF_RF_F_F_BC #-}
{-# ANN testBench_RF_RF_F_F_BC (TestBench 'topEntity_RF_RF_F_F_BC) #-}
testBench_RF_NC_F_F_AB = genTestBench topEntity_RF_NC_F_F_AB rst20 rst10 simOutA_RF_NC_F_F_AB simOutB_RF_NC_F_F_AB
{-# NOINLINE testBench_RF_NC_F_F_AB #-}
{-# ANN testBench_RF_NC_F_F_AB (TestBench 'topEntity_RF_NC_F_F_AB) #-}
testBench_RF_NC_F_F_BC = genTestBench topEntity_RF_NC_F_F_BC rst10 rst7 simOutA_RF_NC_F_F_BC simOutB_RF_NC_F_F_BC
{-# NOINLINE testBench_RF_NC_F_F_BC #-}
{-# ANN testBench_RF_NC_F_F_BC (TestBench 'topEntity_RF_NC_F_F_BC) #-}
testBench_NC_WF_F_F_AB = genTestBench topEntity_NC_WF_F_F_AB rst20 rst10 simOutA_NC_WF_F_F_AB simOutB_NC_WF_F_F_AB
{-# NOINLINE testBench_NC_WF_F_F_AB #-}
{-# ANN testBench_NC_WF_F_F_AB (TestBench 'topEntity_NC_WF_F_F_AB) #-}
testBench_NC_WF_F_F_BC = genTestBench topEntity_NC_WF_F_F_BC rst10 rst7 simOutA_NC_WF_F_F_BC simOutB_NC_WF_F_F_BC
{-# NOINLINE testBench_NC_WF_F_F_BC #-}
{-# ANN testBench_NC_WF_F_F_BC (TestBench 'topEntity_NC_WF_F_F_BC) #-}
testBench_NC_RF_F_F_AB = genTestBench topEntity_NC_RF_F_F_AB rst20 rst10 simOutA_NC_RF_F_F_AB simOutB_NC_RF_F_F_AB
{-# NOINLINE testBench_NC_RF_F_F_AB #-}
{-# ANN testBench_NC_RF_F_F_AB (TestBench 'topEntity_NC_RF_F_F_AB) #-}
testBench_NC_RF_F_F_BC = genTestBench topEntity_NC_RF_F_F_BC rst10 rst7 simOutA_NC_RF_F_F_BC simOutB_NC_RF_F_F_BC
{-# NOINLINE testBench_NC_RF_F_F_BC #-}
{-# ANN testBench_NC_RF_F_F_BC (TestBench 'topEntity_NC_RF_F_F_BC) #-}
testBench_NC_NC_F_F_AB = genTestBench topEntity_NC_NC_F_F_AB rst20 rst10 simOutA_NC_NC_F_F_AB simOutB_NC_NC_F_F_AB
{-# NOINLINE testBench_NC_NC_F_F_AB #-}
{-# ANN testBench_NC_NC_F_F_AB (TestBench 'topEntity_NC_NC_F_F_AB) #-}
testBench_NC_NC_F_F_BC = genTestBench topEntity_NC_NC_F_F_BC rst10 rst7 simOutA_NC_NC_F_F_BC simOutB_NC_NC_F_F_BC
{-# NOINLINE testBench_NC_NC_F_F_BC #-}
{-# ANN testBench_NC_NC_F_F_BC (TestBench 'topEntity_NC_NC_F_F_BC) #-}

topEntity_WF_WF_F_F_AB = topOut WriteFirst WriteFirst False False rst20 rst10
{-# NOINLINE topEntity_WF_WF_F_F_AB #-}
{-# ANN topEntity_WF_WF_F_F_AB (defSyn "topEntity_WF_WF_F_F_AB") #-}
topEntity_WF_WF_F_F_BC = topOut WriteFirst WriteFirst False False rst10 rst7
{-# NOINLINE topEntity_WF_WF_F_F_BC #-}
{-# ANN topEntity_WF_WF_F_F_BC (defSyn "topEntity_WF_WF_F_F_BC") #-}
topEntity_WF_RF_F_F_AB = topOut WriteFirst ReadFirst False False rst20 rst10
{-# NOINLINE topEntity_WF_RF_F_F_AB #-}
{-# ANN topEntity_WF_RF_F_F_AB (defSyn "topEntity_WF_RF_F_F_AB") #-}
topEntity_WF_RF_F_F_BC = topOut WriteFirst ReadFirst False False rst10 rst7
{-# NOINLINE topEntity_WF_RF_F_F_BC #-}
{-# ANN topEntity_WF_RF_F_F_BC (defSyn "topEntity_WF_RF_F_F_BC") #-}
topEntity_WF_NC_F_F_AB = topOut WriteFirst NoChange False False rst20 rst10
{-# NOINLINE topEntity_WF_NC_F_F_AB #-}
{-# ANN topEntity_WF_NC_F_F_AB (defSyn "topEntity_WF_NC_F_F_AB") #-}
topEntity_WF_NC_F_F_BC = topOut WriteFirst NoChange False False rst10 rst7
{-# NOINLINE topEntity_WF_NC_F_F_BC #-}
{-# ANN topEntity_WF_NC_F_F_BC (defSyn "topEntity_WF_NC_F_F_BC") #-}
topEntity_RF_WF_F_F_AB = topOut ReadFirst WriteFirst False False rst20 rst10
{-# NOINLINE topEntity_RF_WF_F_F_AB #-}
{-# ANN topEntity_RF_WF_F_F_AB (defSyn "topEntity_RF_WF_F_F_AB") #-}
topEntity_RF_WF_F_F_BC = topOut ReadFirst WriteFirst False False rst10 rst7
{-# NOINLINE topEntity_RF_WF_F_F_BC #-}
{-# ANN topEntity_RF_WF_F_F_BC (defSyn "topEntity_RF_WF_F_F_BC") #-}
topEntity_RF_RF_F_F_AB = topOut ReadFirst ReadFirst False False rst20 rst10
{-# NOINLINE topEntity_RF_RF_F_F_AB #-}
{-# ANN topEntity_RF_RF_F_F_AB (defSyn "topEntity_RF_RF_F_F_AB") #-}
topEntity_RF_RF_F_F_BC = topOut ReadFirst ReadFirst False False rst10 rst7
{-# NOINLINE topEntity_RF_RF_F_F_BC #-}
{-# ANN topEntity_RF_RF_F_F_BC (defSyn "topEntity_RF_RF_F_F_BC") #-}
topEntity_RF_NC_F_F_AB = topOut ReadFirst NoChange False False rst20 rst10
{-# NOINLINE topEntity_RF_NC_F_F_AB #-}
{-# ANN topEntity_RF_NC_F_F_AB (defSyn "topEntity_RF_NC_F_F_AB") #-}
topEntity_RF_NC_F_F_BC = topOut ReadFirst NoChange False False rst10 rst7
{-# NOINLINE topEntity_RF_NC_F_F_BC #-}
{-# ANN topEntity_RF_NC_F_F_BC (defSyn "topEntity_RF_NC_F_F_BC") #-}
topEntity_NC_WF_F_F_AB = topOut NoChange WriteFirst False False rst20 rst10
{-# NOINLINE topEntity_NC_WF_F_F_AB #-}
{-# ANN topEntity_NC_WF_F_F_AB (defSyn "topEntity_NC_WF_F_F_AB") #-}
topEntity_NC_WF_F_F_BC = topOut NoChange WriteFirst False False rst10 rst7
{-# NOINLINE topEntity_NC_WF_F_F_BC #-}
{-# ANN topEntity_NC_WF_F_F_BC (defSyn "topEntity_NC_WF_F_F_BC") #-}
topEntity_NC_RF_F_F_AB = topOut NoChange ReadFirst False False rst20 rst10
{-# NOINLINE topEntity_NC_RF_F_F_AB #-}
{-# ANN topEntity_NC_RF_F_F_AB (defSyn "topEntity_NC_RF_F_F_AB") #-}
topEntity_NC_RF_F_F_BC = topOut NoChange ReadFirst False False rst10 rst7
{-# NOINLINE topEntity_NC_RF_F_F_BC #-}
{-# ANN topEntity_NC_RF_F_F_BC (defSyn "topEntity_NC_RF_F_F_BC") #-}
topEntity_NC_NC_F_F_AB = topOut NoChange NoChange False False rst20 rst10
{-# NOINLINE topEntity_NC_NC_F_F_AB #-}
{-# ANN topEntity_NC_NC_F_F_AB (defSyn "topEntity_NC_NC_F_F_AB") #-}
topEntity_NC_NC_F_F_BC = topOut NoChange NoChange False False rst10 rst7
{-# NOINLINE topEntity_NC_NC_F_F_BC #-}
{-# ANN topEntity_NC_NC_F_F_BC (defSyn "topEntity_NC_NC_F_F_BC") #-}

-- Expected output generation for all configurations with Template Haskell
simOutA_WF_WF_F_F_AB = $(collectSimResults (length opsA+1) $ pack <$> (fst $ topOut WriteFirst WriteFirst False False rst20 rst10 clk20 clk10))
{-# INLINE simOutA_WF_WF_F_F_AB #-}
simOutA_WF_WF_F_F_BC = $(collectSimResults (length opsA+1) $ pack <$> (fst $ topOut WriteFirst WriteFirst False False rst10 rst7 clk10 clk7))
{-# INLINE simOutA_WF_WF_F_F_BC #-}
simOutA_WF_RF_F_F_AB = $(collectSimResults (length opsA+1) $ pack <$> (fst $ topOut WriteFirst ReadFirst False False rst20 rst10 clk20 clk10))
{-# INLINE simOutA_WF_RF_F_F_AB #-}
simOutA_WF_RF_F_F_BC = $(collectSimResults (length opsA+1) $ pack <$> (fst $ topOut WriteFirst ReadFirst False False rst10 rst7 clk10 clk7))
{-# INLINE simOutA_WF_RF_F_F_BC #-}
simOutA_WF_NC_F_F_AB = $(collectSimResults (length opsA+1) $ pack <$> (fst $ topOut WriteFirst NoChange False False rst20 rst10 clk20 clk10))
{-# INLINE simOutA_WF_NC_F_F_AB #-}
simOutA_WF_NC_F_F_BC = $(collectSimResults (length opsA+1) $ pack <$> (fst $ topOut WriteFirst NoChange False False rst10 rst7 clk10 clk7))
{-# INLINE simOutA_WF_NC_F_F_BC #-}
simOutA_RF_WF_F_F_AB = $(collectSimResults (length opsA+1) $ pack <$> (fst $ topOut ReadFirst WriteFirst False False rst20 rst10 clk20 clk10))
{-# INLINE simOutA_RF_WF_F_F_AB #-}
simOutA_RF_WF_F_F_BC = $(collectSimResults (length opsA+1) $ pack <$> (fst $ topOut ReadFirst WriteFirst False False rst10 rst7 clk10 clk7))
{-# INLINE simOutA_RF_WF_F_F_BC #-}
simOutA_RF_RF_F_F_AB = $(collectSimResults (length opsA+1) $ pack <$> (fst $ topOut ReadFirst ReadFirst False False rst20 rst10 clk20 clk10))
{-# INLINE simOutA_RF_RF_F_F_AB #-}
simOutA_RF_RF_F_F_BC = $(collectSimResults (length opsA+1) $ pack <$> (fst $ topOut ReadFirst ReadFirst False False rst10 rst7 clk10 clk7))
{-# INLINE simOutA_RF_RF_F_F_BC #-}
simOutA_RF_NC_F_F_AB = $(collectSimResults (length opsA+1) $ pack <$> (fst $ topOut ReadFirst NoChange False False rst20 rst10 clk20 clk10))
{-# INLINE simOutA_RF_NC_F_F_AB #-}
simOutA_RF_NC_F_F_BC = $(collectSimResults (length opsA+1) $ pack <$> (fst $ topOut ReadFirst NoChange False False rst10 rst7 clk10 clk7))
{-# INLINE simOutA_RF_NC_F_F_BC #-}
simOutA_NC_WF_F_F_AB = $(collectSimResults (length opsA+1) $ pack <$> (fst $ topOut NoChange WriteFirst False False rst20 rst10 clk20 clk10))
{-# INLINE simOutA_NC_WF_F_F_AB #-}
simOutA_NC_WF_F_F_BC = $(collectSimResults (length opsA+1) $ pack <$> (fst $ topOut NoChange WriteFirst False False rst10 rst7 clk10 clk7))
{-# INLINE simOutA_NC_WF_F_F_BC #-}
simOutA_NC_RF_F_F_AB = $(collectSimResults (length opsA+1) $ pack <$> (fst $ topOut NoChange ReadFirst False False rst20 rst10 clk20 clk10))
{-# INLINE simOutA_NC_RF_F_F_AB #-}
simOutA_NC_RF_F_F_BC = $(collectSimResults (length opsA+1) $ pack <$> (fst $ topOut NoChange ReadFirst False False rst10 rst7 clk10 clk7))
{-# INLINE simOutA_NC_RF_F_F_BC #-}
simOutA_NC_NC_F_F_AB = $(collectSimResults (length opsA+1) $ pack <$> (fst $ topOut NoChange NoChange False False rst20 rst10 clk20 clk10))
{-# INLINE simOutA_NC_NC_F_F_AB #-}
simOutA_NC_NC_F_F_BC = $(collectSimResults (length opsA+1) $ pack <$> (fst $ topOut NoChange NoChange False False rst10 rst7 clk10 clk7))
{-# INLINE simOutA_NC_NC_F_F_BC #-}
simOutB_WF_WF_F_F_AB = $(collectSimResults (length opsB+1) $ pack <$> (snd $ topOut WriteFirst WriteFirst False False rst20 rst10 clk20 clk10))
{-# INLINE simOutB_WF_WF_F_F_AB #-}
simOutB_WF_WF_F_F_BC = $(collectSimResults (length opsB+1) $ pack <$> (snd $ topOut WriteFirst WriteFirst False False rst10 rst7 clk10 clk7))
{-# INLINE simOutB_WF_WF_F_F_BC #-}
simOutB_WF_RF_F_F_AB = $(collectSimResults (length opsB+1) $ pack <$> (snd $ topOut WriteFirst ReadFirst False False rst20 rst10 clk20 clk10))
{-# INLINE simOutB_WF_RF_F_F_AB #-}
simOutB_WF_RF_F_F_BC = $(collectSimResults (length opsB+1) $ pack <$> (snd $ topOut WriteFirst ReadFirst False False rst10 rst7 clk10 clk7))
{-# INLINE simOutB_WF_RF_F_F_BC #-}
simOutB_WF_NC_F_F_AB = $(collectSimResults (length opsB+1) $ pack <$> (snd $ topOut WriteFirst NoChange False False rst20 rst10 clk20 clk10))
{-# INLINE simOutB_WF_NC_F_F_AB #-}
simOutB_WF_NC_F_F_BC = $(collectSimResults (length opsB+1) $ pack <$> (snd $ topOut WriteFirst NoChange False False rst10 rst7 clk10 clk7))
{-# INLINE simOutB_WF_NC_F_F_BC #-}
simOutB_RF_WF_F_F_AB = $(collectSimResults (length opsB+1) $ pack <$> (snd $ topOut ReadFirst WriteFirst False False rst20 rst10 clk20 clk10))
{-# INLINE simOutB_RF_WF_F_F_AB #-}
simOutB_RF_WF_F_F_BC = $(collectSimResults (length opsB+1) $ pack <$> (snd $ topOut ReadFirst WriteFirst False False rst10 rst7 clk10 clk7))
{-# INLINE simOutB_RF_WF_F_F_BC #-}
simOutB_RF_RF_F_F_AB = $(collectSimResults (length opsB+1) $ pack <$> (snd $ topOut ReadFirst ReadFirst False False rst20 rst10 clk20 clk10))
{-# INLINE simOutB_RF_RF_F_F_AB #-}
simOutB_RF_RF_F_F_BC = $(collectSimResults (length opsB+1) $ pack <$> (snd $ topOut ReadFirst ReadFirst False False rst10 rst7 clk10 clk7))
{-# INLINE simOutB_RF_RF_F_F_BC #-}
simOutB_RF_NC_F_F_AB = $(collectSimResults (length opsB+1) $ pack <$> (snd $ topOut ReadFirst NoChange False False rst20 rst10 clk20 clk10))
{-# INLINE simOutB_RF_NC_F_F_AB #-}
simOutB_RF_NC_F_F_BC = $(collectSimResults (length opsB+1) $ pack <$> (snd $ topOut ReadFirst NoChange False False rst10 rst7 clk10 clk7))
{-# INLINE simOutB_RF_NC_F_F_BC #-}
simOutB_NC_WF_F_F_AB = $(collectSimResults (length opsB+1) $ pack <$> (snd $ topOut NoChange WriteFirst False False rst20 rst10 clk20 clk10))
{-# INLINE simOutB_NC_WF_F_F_AB #-}
simOutB_NC_WF_F_F_BC = $(collectSimResults (length opsB+1) $ pack <$> (snd $ topOut NoChange WriteFirst False False rst10 rst7 clk10 clk7))
{-# INLINE simOutB_NC_WF_F_F_BC #-}
simOutB_NC_RF_F_F_AB = $(collectSimResults (length opsB+1) $ pack <$> (snd $ topOut NoChange ReadFirst False False rst20 rst10 clk20 clk10))
{-# INLINE simOutB_NC_RF_F_F_AB #-}
simOutB_NC_RF_F_F_BC = $(collectSimResults (length opsB+1) $ pack <$> (snd $ topOut NoChange ReadFirst False False rst10 rst7 clk10 clk7))
{-# INLINE simOutB_NC_RF_F_F_BC #-}
simOutB_NC_NC_F_F_AB = $(collectSimResults (length opsB+1) $ pack <$> (snd $ topOut NoChange NoChange False False rst20 rst10 clk20 clk10))
{-# INLINE simOutB_NC_NC_F_F_AB #-}
simOutB_NC_NC_F_F_BC = $(collectSimResults (length opsB+1) $ pack <$> (snd $ topOut NoChange NoChange False False rst10 rst7 clk10 clk7))
{-# INLINE simOutB_NC_NC_F_F_BC #-}
