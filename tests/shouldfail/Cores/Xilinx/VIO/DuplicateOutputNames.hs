module DuplicateOutputNames where

import Clash.Prelude
import Clash.Cores.Xilinx.VIO

type Dom = XilinxSystem

inNames = Nil
outNames = "a" :> "a" :> Nil

topEntity ::
  "clk" ::: Clock Dom ->
  "out" ::: Signal Dom (Bit, Bit)
topEntity = vioProbe @Dom inNames outNames (0, 0)
