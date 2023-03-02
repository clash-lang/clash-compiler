module OutputProbesExceeded where

import Clash.Prelude
import Clash.Cores.Xilinx.VIO

type Dom = XilinxSystem

topEntity ::
  "clk" ::: Clock Dom ->
  "out" ::: Signal Dom (Vec 257 Bit)
topEntity = vioProbe @Dom (replicate (SNat @257) low)
