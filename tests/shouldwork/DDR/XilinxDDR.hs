{-# LANGUAGE CPP #-}

module XilinxDDR where

import Clash.Annotations.TH
import Clash.Explicit.Prelude
import Clash.Xilinx.DDR
import Data.Bifunctor

import VendorDDR

xilinxIn :: DomCxt slow fast fPeriod reset => VendorIn slow fast
xilinxIn clk rst en =
  fmap (bimap unpack unpack) . iddr clk rst en . fmap pack

xilinxOut :: DomCxt slow fast fPeriod reset => VendorOut slow fast
xilinxOut clk rst en =
  fmap unpack . oddr clk rst en . fmap (bimap pack pack)

topEntityUA :: TopEntityNoEna System DDRA
topEntityUA clk rst = topEntityGeneric xilinxIn xilinxOut clk rst enableGen
{-# CLASH_OPAQUE topEntityUA #-}
{-# ANN topEntityUA (topEntityNoEnaAnn "topEntityUA") #-}

expGenOutUA :: Vec TestLen2 D
expGenOutUA = $(listToVecTH $ outAsyncReset expOut)

expXilinxOutUA :: Vec TestLen2 D
expXilinxOutUA = $(listToVecTH $ outAsyncReset expOut)

expInUA :: Vec TestLen (D, D)
expInUA = $(listToVecTH $ inAsyncReset expIn)

testBenchUA :: TestBenchT System DDRA
testBenchUA =
  testBenchGeneric (noEnable topEntityUA) expInUA expGenOutUA expXilinxOutUA
{-# CLASH_OPAQUE testBenchUA #-}
{-# ANN testBenchUA (TestBench 'topEntityUA) #-}

makeTopEntity 'testBenchUA

topEntityUS :: TopEntityNoEna XilinxSystem DDRS
topEntityUS clk rst = topEntityGeneric xilinxIn xilinxOut clk rst enableGen
{-# CLASH_OPAQUE topEntityUS #-}
{-# ANN topEntityUS (topEntityNoEnaAnn "topEntityUS") #-}

expGenOutUS :: Vec TestLen2 D
expGenOutUS = $(listToVecTH $ genOutSyncReset expOut)

expXilinxOutUS :: Vec TestLen2 D
expXilinxOutUS = $(listToVecTH $ xilinxOutSyncReset expOut)

expInUS :: Vec TestLen (D, D)
expInUS = $(listToVecTH $ inSyncReset expIn)

testBenchUS :: TestBenchT XilinxSystem DDRS
testBenchUS =
  testBenchGeneric (noEnable topEntityUS) expInUS expGenOutUS expXilinxOutUS
{-# CLASH_OPAQUE testBenchUS #-}
{-# ANN testBenchUS (TestBench 'topEntityUS) #-}

makeTopEntity 'testBenchUS

topEntityGA :: TopEntityEna System DDRA
topEntityGA = topEntityGeneric xilinxIn xilinxOut
{-# CLASH_OPAQUE topEntityGA #-}
{-# ANN topEntityGA (topEntityEnaAnn "topEntityGA") #-}

expGenOutGA :: Vec TestLen2 D
expGenOutGA = $(listToVecTH $ genOutEnable $ outAsyncReset expOut)

expXilinxOutGA :: Vec TestLen2 D
expXilinxOutGA = $(listToVecTH $ xilinxOutEnable $ outAsyncReset expOut)

expInGA :: Vec TestLen (D, D)
expInGA = $(listToVecTH $ inEnable $ inAsyncReset expIn)

testBenchGA :: TestBenchT System DDRA
testBenchGA = testBenchGeneric topEntityGA expInGA expGenOutGA expXilinxOutGA
{-# CLASH_OPAQUE testBenchGA #-}
{-# ANN testBenchGA (TestBench 'topEntityGA) #-}

makeTopEntity 'testBenchGA

topEntityGS :: TopEntityEna XilinxSystem DDRS
topEntityGS = topEntityGeneric xilinxIn xilinxOut
{-# CLASH_OPAQUE topEntityGS #-}
{-# ANN topEntityGS (topEntityEnaAnn "topEntityGS") #-}

expGenOutGS :: Vec TestLen2 D
expGenOutGS = $(listToVecTH $ genOutEnable $ genOutSyncReset expOut)

expXilinxOutGS :: Vec TestLen2 D
expXilinxOutGS = $(listToVecTH $ xilinxOutEnable $ xilinxOutSyncReset expOut)

expInGS :: Vec TestLen (D, D)
expInGS = $(listToVecTH $ inEnable $ inSyncReset expIn)

testBenchGS :: TestBenchT XilinxSystem DDRS
testBenchGS = testBenchGeneric topEntityGS expInGS expGenOutGS expXilinxOutGS
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
