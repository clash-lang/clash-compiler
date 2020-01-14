module T1007 where

import Clash.Prelude

topEntity clk rst = withClockResetEnable clk rst enableGen $ mealy @System f False

f s x = (x,s)
