module T2839 where

import Clash.Explicit.Prelude
import Clash.Explicit.Testbench

topEntity ::
  Signal System (Unsigned 8)
topEntity = register clk noReset enableGen 100 0
 where
  cntr = register clk noReset enableGen (0 :: Unsigned 8) 0
  done = (== 100) <$> cntr
  clk = tbClockGen $ not <$> done
