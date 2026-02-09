module T3141 where

import Clash.Explicit.Prelude

topEntity ::
  Clock System ->
  Enable System ->
  Signal System (Unsigned 4) ->
  Signal System (Maybe (Unsigned 4, BitVector 8)) ->
  Signal System (BitVector 8)
topEntity clk ena = blockRamFile clk ena d16 "T3141.mem"
