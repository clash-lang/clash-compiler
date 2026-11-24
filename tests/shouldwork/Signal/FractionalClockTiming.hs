module FractionalClockTiming where

import Clash.Explicit.Prelude
import Clash.Explicit.Testbench

createDomain vSystem{vName="FractionalClock", vPeriod=5.055}
createDomain vSystem{vName="Reference", vPeriod=1100}

edges :: KnownDomain dom => Clock dom -> Enable dom -> Signal dom (Unsigned 8)
edges clk en = n
 where
  n = delay clk en 0 ((+1) <$> n)

topEntity :: Clock FractionalClock -> Signal FractionalClock (Unsigned 8, Unsigned 8)
topEntity clk = bundle (edges clk enableGen, edges clk inReset)
 where
  inReset = toEnable (unsafeToActiveHigh (resetGenN d100))
{-# OPAQUE topEntity #-}

testBench :: Signal Reference Bool
testBench = done
 where
  (ref, clk) = biTbClockGen (not <$> done)
  ticks = edges ref enableGen
  counts = unsafeSynchronizer clk ref (topEntity clk)
  -- At the second reference edge, 1100 ps have elapsed. Depending on simulator
  -- precision the clock period rounds to 5.055, 5.06 or 5.1 ps. Truncating it
  -- to 5 ps would produce 220 edges.
  check t (n, r) = t /= 1 || (n >= 215 && n <= 218 && r == 100)
  done = assert ref noReset "fractional clock period and reset duration"
    (check <$> ticks <*> counts) (pure True) ((== 2) <$> ticks)
{-# OPAQUE testBench #-}
