module DuplicateInputNames where

import Clash.Prelude
import Clash.Cores.Xilinx.VIO

type Dom = XilinxSystem

inNames = "a" :> "a" :> Nil
outNames = "b" :> Nil

topEntity ::
  "clk" ::: Clock Dom ->
  "in" ::: Signal Dom (Bit, Bit) ->
  "out" ::: Signal Dom Bit
topEntity = vioProbe @Dom inNames outNames 0
