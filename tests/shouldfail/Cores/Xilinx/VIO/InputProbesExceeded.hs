module InputProbesExceeded where

import Clash.Prelude
import Clash.Cores.Xilinx.VIO

type Dom = XilinxSystem

topEntity ::
  "clk" ::: Clock Dom ->
  "in"  ::: Signal Dom (Vec 257 Bool) ->
  "out" ::: Signal Dom ()
topEntity = vioProbe @Dom ()
