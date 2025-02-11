{-# LANGUAGE CPP #-}

module IntelDDR where

import Clash.Annotations.TH
import Clash.Explicit.Prelude
import Clash.Intel.DDR

import VendorDDR

intelIn :: DomCxt slow fast fPeriod reset => VendorIn slow fast
intelIn = altddioIn (SSymbol @"Arria II GX")

intelOut :: DomCxt slow fast fPeriod reset => VendorOut slow fast
intelOut = altddioOut (SSymbol @"Arria II GX")

topEntityUA :: TopEntityNoEna System DDRA
topEntityUA clk rst = topEntityGeneric intelIn intelOut clk rst enableGen
{-# CLASH_OPAQUE topEntityUA #-}
{-# ANN topEntityUA (topEntityNoEnaAnn "topEntityUA") #-}

expOutUA :: Vec TestLen2 D
expOutUA = $(listToVecTH $ outAsyncReset expOut)

expInUA :: Vec TestLen (D, D)
expInUA = $(listToVecTH $ inAsyncReset expIn)

testBenchUA :: TestBenchT System DDRA
testBenchUA =
  testBenchGeneric (noEnable topEntityUA) expInUA expOutUA expOutUA
{-# CLASH_OPAQUE testBenchUA #-}
{-# ANN testBenchUA (TestBench 'topEntityUA) #-}

makeTopEntity 'testBenchUA

topEntityUS :: TopEntityNoEna XilinxSystem DDRS
topEntityUS clk rst = topEntityGeneric intelIn intelOut clk rst enableGen
{-# CLASH_OPAQUE topEntityUS #-}
{-# ANN topEntityUS (topEntityNoEnaAnn "topEntityUS") #-}

expOutUS :: Vec TestLen2 D
expOutUS = $(listToVecTH $ genOutSyncReset expOut)

expInUS :: Vec TestLen (D, D)
expInUS = $(listToVecTH $ inSyncReset expIn)

testBenchUS :: TestBenchT XilinxSystem DDRS
testBenchUS =
  testBenchGeneric (noEnable topEntityUS) expInUS expOutUS expOutUS
{-# CLASH_OPAQUE testBenchUS #-}
{-# ANN testBenchUS (TestBench 'topEntityUS) #-}

makeTopEntity 'testBenchUS

topEntityGA :: TopEntityEna System DDRA
topEntityGA = topEntityGeneric intelIn intelOut
{-# CLASH_OPAQUE topEntityGA #-}
{-# ANN topEntityGA (topEntityEnaAnn "topEntityGA") #-}

expOutGA :: Vec TestLen2 D
expOutGA = $(listToVecTH $ genOutEnable $ outAsyncReset expOut)

expInGA :: Vec TestLen (D, D)
expInGA = $(listToVecTH $ inEnable $ inAsyncReset expIn)

testBenchGA :: TestBenchT System DDRA
testBenchGA = testBenchGeneric topEntityGA expInGA expOutGA expOutGA
{-# CLASH_OPAQUE testBenchGA #-}
{-# ANN testBenchGA (TestBench 'topEntityGA) #-}

makeTopEntity 'testBenchGA

topEntityGS :: TopEntityEna XilinxSystem DDRS
topEntityGS = topEntityGeneric intelIn intelOut
{-# CLASH_OPAQUE topEntityGS #-}
{-# ANN topEntityGS (topEntityEnaAnn "topEntityGS") #-}

expOutGS :: Vec TestLen2 D
expOutGS = $(listToVecTH $ genOutEnable $ genOutSyncReset expOut)

expInGS :: Vec TestLen (D, D)
expInGS = $(listToVecTH $ inEnable $ inSyncReset expIn)

testBenchGS :: TestBenchT XilinxSystem DDRS
testBenchGS = testBenchGeneric topEntityGS expInGS expOutGS expOutGS
{-# CLASH_OPAQUE testBenchGS #-}
{-# ANN testBenchGS (TestBench 'topEntityGS) #-}

makeTopEntity 'testBenchGS

-- Note that this only works correctly in HDL when all test benches are exactly
-- equally long. Even though VHDL simulation will run until all individual
-- clocks have stopped, 'tbClockGen' for Verilog will end the simulation when
-- the first clock stops, cutting short the longer test bench if the lengths
-- were to differ.
testBenchAll :: Signal DDRA Bool
testBenchAll = done
 where
   doneUA = getDone testBenchUA
   doneUS = getDone testBenchUS
   doneGA = getDone testBenchGA
   doneGS = getDone testBenchGS
   doneS = doneUS `strictAnd` doneGS
   done =
     doneUA `strictAnd` doneGA `strictAnd`
     unsafeSynchronizer (clockGen @DDRS) (clockGen @DDRA) doneS
{-# CLASH_OPAQUE testBenchAll #-}
{-# ANN testBenchAll (defSyn "testBenchAll") #-}
