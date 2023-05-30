module OutputBusWidthExceeded where

import Clash.Prelude
import Clash.Cores.Xilinx.VIO

type Dom = XilinxSystem

inNames = Nil
outNames = singleton "probe_out"

topEntity ::
  "clk" ::: Clock Dom ->
  "out" ::: Signal Dom (BitVector 257)
topEntity = vioProbe @Dom inNames outNames 0
