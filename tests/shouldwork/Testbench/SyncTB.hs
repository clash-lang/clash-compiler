{-# LANGUAGE CPP #-}

module SyncTB where

import Clash.Explicit.Testbench
import Clash.Explicit.Prelude
import Clash.Signal (mux)

import Unsafe.Coerce

createDomain vSystem{vName="Dom2", vPeriod=2}
createDomain vSystem{vName="Dom7", vPeriod=7}
createDomain vSystem{vName="Dom9", vPeriod=9}

zeroAt0
  :: forall dom a
   . (KnownDomain dom, Num a)
  => Clock dom
  -> Reset dom
  -> Signal dom a
  -> Signal dom a
zeroAt0 clk rst a = mux en a 0
  where
    en = register clk rst (enableGen @dom) False (pure True)

topEntity
  :: Clock Dom2
  -> Clock Dom7
  -> Clock Dom9
  -> Signal Dom7 Integer
  -> Signal Dom9 Integer
topEntity clk2 clk7 clk9 i =
  delay
    clk9
    enableGen
    0
    (unsafeSynchronizer
      clk2
      clk9
      (delay
        clk2
        enableGen
        0
        (unsafeSynchronizer
          clk7
          clk2
          (delay clk7 enableGen 0 i))))
-- See: https://github.com/clash-lang/clash-compiler/pull/2511
{-# CLASH_OPAQUE topEntity #-}

testBench
  :: Signal Dom9 Bool
testBench = done
  where
    testInput      = stimuliGenerator clk7 rst7 $(listToVecTH [(1::Integer)..20])
    expectedOutput = outputVerifier'   clk9 rst9
                        (0 :> 1 :>  $(listToVecTH ([2,3,4,6,7,8,9,11,12,13,15,16]::[Integer])))
    done           = expectedOutput (zeroAt0 clk9 rst9 (topEntity clk2 clk7 clk9 testInput))
    notDone        = not <$> done
    clk2           = tbClockGen @Dom2 (unsafeSynchronizer clk9 clk2 notDone)
    clk7           = tbClockGen @Dom7 (unsafeSynchronizer clk9 clk7 notDone)
    clk9           = tbClockGen @Dom9 notDone
    rst7           = resetGen @Dom7
    rst9           = resetGen @Dom9
