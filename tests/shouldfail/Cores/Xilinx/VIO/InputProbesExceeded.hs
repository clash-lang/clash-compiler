{-# LANGUAGE TemplateHaskell #-}

module InputProbesExceeded where

import Clash.Prelude
import Clash.Cores.Xilinx.VIO

import qualified Data.List as L

type Dom = XilinxSystem

inNames = $(listToVecTH (L.map (("probe_in_" <>) . show) [0::Int, 1..256]))
outNames = Nil

topEntity ::
  "clk" ::: Clock Dom ->
  "in"  ::: Signal Dom (Vec 257 Bool) ->
  "out" ::: Signal Dom ()
topEntity = vioProbe @Dom inNames outNames ()
