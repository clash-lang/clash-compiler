module OutputBusWidthExceeded where

import Clash.Prelude
import Clash.Cores.Xilinx.VIO

type Dom = XilinxSystem

topEntity ::
  "clk" ::: Clock Dom ->
  "out" ::: Signal Dom (BitVector 257)
topEntity = vioProbe @Dom 0
