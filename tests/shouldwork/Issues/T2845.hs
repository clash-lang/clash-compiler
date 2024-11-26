module T2845 where

import Clash.Explicit.Prelude
import Clash.Explicit.Testbench

topEntity ::
  Signal System (Unsigned 8)
topEntity = cntr + x
 where
  cntr = register clk noReset enableGen 0 0
  x = register clk noReset enableGen 100 0
  done = (== 100) <$> cntr
  clk = tbClockGen $ not <$> done
