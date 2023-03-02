module InputBusWidthExceeded where

import Clash.Prelude
import Clash.Cores.Xilinx.VIO

type Dom = XilinxSystem

topEntity ::
  "clk" ::: Clock Dom ->
  "in"  ::: Signal Dom (BitVector 257) ->
  "out" ::: Signal Dom ()
topEntity = vioProbe @Dom ()
