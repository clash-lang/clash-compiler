{-# LANGUAGE CPP #-}

{-# OPTIONS_GHC -Wno-orphans #-}

-- | 'ddrIn' with 'Vec' and 'RTree' payloads. In SystemVerilog these are
-- unpacked arrays, which cannot be concatenated into the packed result tuple
-- without converting them.
module DDRinVec where

import Clash.Explicit.Prelude
import Clash.Explicit.DDR
import Clash.Explicit.Testbench

createDomain vSystem{vName="Slow", vPeriod=2000, vResetKind=Asynchronous}
createDomain vSystem{vName="Fast", vPeriod=1000, vResetKind=Asynchronous}

type V = Vec 2 (Unsigned 8)
type T = RTree 1 (Unsigned 8)

-- | Turn a sample number into a 'Vec' payload whose elements differ
toV :: Unsigned 8 -> V
toV n = n :> (n + 100) :> Nil

dummyV :: V
dummyV = toV 0

topEntityVec
  :: Clock Slow
  -> Reset Slow
  -> Signal Fast V
  -> Signal Slow (V, V)
topEntityVec clk rst = ddrIn clk rst enableGen (dummyV, dummyV, dummyV)
{-# OPAQUE topEntityVec #-}
{-# ANN topEntityVec (defSyn "topEntityVec") #-}

topEntityTree
  :: Clock Slow
  -> Reset Slow
  -> Signal Fast T
  -> Signal Slow (T, T)
topEntityTree clk rst = ddrIn clk rst enableGen (v2t dummyV, v2t dummyV, v2t dummyV)
{-# OPAQUE topEntityTree #-}
{-# ANN topEntityTree (defSyn "topEntityTree") #-}

testinput :: Vec 17 V
testinput = map toV $(listToVecTH [1..17 :: Unsigned 8])

-- | Same expected output as the @DDRin@ test, with every sample mapped through
-- 'toV'
testoutput :: Vec 9 (V, V)
testoutput =
     (dummyV, dummyV)
  :> (dummyV, toV 2)
  :> map (\(a, b) -> (toV a, toV b))
       ((3,4) :> (5,6) :> (7,8) :> (9,10) :> (11,12) :> (13,14) :> (15,16) :> Nil)

testBenchVec :: Signal Slow Bool
testBenchVec = done
  where
    testInput      = stimuliGenerator clkFast rstFast testinput
    expectedOutput = outputVerifier' clkSlow rstSlow testoutput
    actualOutput   = ignoreFor clkSlow rstSlow enableGen d1 (dummyV, dummyV) (topEntityVec clkSlow rstSlow testInput)
    done           = expectedOutput actualOutput
    notDone        = not <$> done

    clkFast         = tbClockGen @Fast (unsafeSynchronizer clkSlow clkFast notDone)
    clkSlow        = tbClockGen @Slow notDone
    rstFast         = resetGen @Fast
    rstSlow        = resetGen @Slow
{-# OPAQUE testBenchVec #-}
{-# ANN testBenchVec (TestBench 'topEntityVec) #-}

testBenchTree :: Signal Slow Bool
testBenchTree = done
  where
    toT (a, b)     = (v2t a, v2t b)
    testInput      = stimuliGenerator clkFast rstFast (map v2t testinput)
    expectedOutput = outputVerifier' clkSlow rstSlow (map toT testoutput)
    actualOutput   = ignoreFor clkSlow rstSlow enableGen d1 (toT (dummyV, dummyV)) (topEntityTree clkSlow rstSlow testInput)
    done           = expectedOutput actualOutput
    notDone        = not <$> done

    clkFast         = tbClockGen @Fast (unsafeSynchronizer clkSlow clkFast notDone)
    clkSlow        = tbClockGen @Slow notDone
    rstFast         = resetGen @Fast
    rstSlow        = resetGen @Slow
{-# OPAQUE testBenchTree #-}
{-# ANN testBenchTree (TestBench 'topEntityTree) #-}
