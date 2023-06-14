module DuplicateInputOutputNames where

import Clash.Prelude
import Clash.Cores.Xilinx.VIO

type Dom = XilinxSystem

inNames = "a" :> Nil
outNames = "a" :> Nil

topEntity ::
  "clk" ::: Clock Dom ->
  "in" ::: Signal Dom Bit ->
  "out" ::: Signal Dom Bit
topEntity = vioProbe @Dom inNames outNames 0
