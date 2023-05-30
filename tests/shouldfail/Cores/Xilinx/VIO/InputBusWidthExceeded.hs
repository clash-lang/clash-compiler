module InputBusWidthExceeded where

import Clash.Prelude
import Clash.Cores.Xilinx.VIO

type Dom = XilinxSystem

inNames = singleton "probe_in"
outNames = Nil

topEntity ::
  "clk" ::: Clock Dom ->
  "in"  ::: Signal Dom (BitVector 257) ->
  "out" ::: Signal Dom ()
topEntity = vioProbe @Dom inNames outNames ()
