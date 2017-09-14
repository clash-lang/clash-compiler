module DDRin where

import Clash.Explicit.Prelude
import Clash.Explicit.DDR

type DomReal = Dom "A" 2000 -- real clock domain
type DomDDR  = Dom "A" 1000 -- fake doublespeed domain, used to model a ddr signal

{-
The four variants defined here are all the combinations of
  clock: Gated  or Ungated
  reset: Asynch or Sync
-}


topEntityGeneric :: Clock DomReal gated
          -> Reset DomReal synchronous
          -> Signal DomDDR (Unsigned 8)
          -> Signal DomReal (Unsigned 8, Unsigned 8)
topEntityGeneric clk rst = ddrIn clk rst (i0,i1,i2)
-- topEntityGeneric clk rst = xilinxIddr clk rst
-- topEntityGeneric clk rst = altddioIn (SSymbol @"Cyclone IV GX") clk rst


topEntityUA :: Clock DomReal Source
          -> Reset DomReal Asynchronous
          -> Signal DomDDR (Unsigned 8)
          -> Signal DomReal (Unsigned 8, Unsigned 8)
topEntityUA = topEntityGeneric

topEntityUS :: Clock DomReal Source
          -> Reset DomReal Synchronous
          -> Signal DomDDR (Unsigned 8)
          -> Signal DomReal (Unsigned 8, Unsigned 8)
topEntityUS = topEntityGeneric

topEntityGA :: Clock DomReal Gated
          -> Reset DomReal Asynchronous
          -> Signal DomDDR (Unsigned 8)
          -> Signal DomReal (Unsigned 8, Unsigned 8)
topEntityGA = topEntityGeneric

topEntityGS :: Clock DomReal Gated
          -> Reset DomReal Synchronous
          -> Signal DomDDR (Unsigned 8)
          -> Signal DomReal (Unsigned 8, Unsigned 8)
topEntityGS = topEntityGeneric


testinputAsync  = $(listToVecTH [1,2,3,4,5,6,7,8,9,10,11,12,13,14,15::Unsigned 8])
testinputSync   = $(listToVecTH   [2,3,4,5,6,7,8,9,10,11,12,13,14,15::Unsigned 8]) -- sync stimuliGenerator has a extra delay compared to the async one


i0 = 100
i1 = 101
i2 = 102

testoutputAsync = (i0,i1) :> (i2,1) :>    (2,3):>(4,5):>(6,7):>(8,9):>(10,11):>(12,13):>((14,15)::(Unsigned 8,Unsigned 8)) :> Nil
testoutputSync =  (undefined,undefined):>(i2,3):>(4,5):>(6,7):>(8,9):>(10,11):>(12,13):>((14,15)::(Unsigned 8,Unsigned 8)) :> Nil



testBenchUS :: Signal DomReal Bool
testBenchUS = done
  where
    testInput      = stimuliGenerator clkDDR rstDDR testinputSync
    expectedOutput = outputVerifier   clkReal rstReal testoutputSync
    actualOutput   = topEntityUS clkReal rstReal testInput
    done           = expectedOutput actualOutput
    done'          = not <$> done

    clkDDR         = tbClockGen @DomDDR (unsafeSynchronizer clkReal clkDDR done')
    clkReal        = tbClockGen @DomReal done'
    rstDDR         = syncResetGen @DomDDR
    rstReal        = syncResetGen @DomReal

testBenchUA :: Signal DomReal Bool
testBenchUA = done
  where
    testInput      = stimuliGenerator clkDDR rstDDR testinputAsync
    expectedOutput = outputVerifier   clkReal rstReal testoutputAsync
    actualOutput   = topEntityUA clkReal rstReal testInput
    done           = expectedOutput actualOutput
    done'          = not <$> done

    clkDDR         = tbClockGen @DomDDR (unsafeSynchronizer clkReal clkDDR done')
    clkReal        = tbClockGen @DomReal done'
    rstDDR         = asyncResetGen @DomDDR
    rstReal        = asyncResetGen @DomReal

testBenchGS :: Signal DomReal Bool
testBenchGS = done
  where
    testInput      = stimuliGenerator clkDDR rstDDR testinputSync
    expectedOutput = outputVerifier   clkReal rstReal testoutputSync
    actualOutput   = topEntityGS clkReal rstReal testInput
    done           = expectedOutput actualOutput
    done'          = not <$> done

    clkDDR         = let c = tbClockGen @DomDDR (unsafeSynchronizer clkReal clkDDR done') in clockGate c $ pure True
    clkReal        = let c = tbClockGen @DomReal done' in clockGate c $ pure True
    rstDDR         = syncResetGen @DomDDR
    rstReal        = syncResetGen @DomReal

testBenchGA :: Signal DomReal Bool
testBenchGA = done
  where
    testInput      = stimuliGenerator clkDDR rstDDR testinputAsync
    expectedOutput = outputVerifier   clkReal rstReal testoutputAsync
    actualOutput   = topEntityGA clkReal rstReal testInput
    done           = expectedOutput actualOutput
    done'          = not <$> done

    clkDDR         = let c = tbClockGen @DomDDR (unsafeSynchronizer clkReal clkDDR done') in clockGate c $ pure True
    clkReal        = let c = tbClockGen @DomReal done' in clockGate c $ pure True
    rstDDR         = asyncResetGen @DomDDR
    rstReal        = asyncResetGen @DomReal
