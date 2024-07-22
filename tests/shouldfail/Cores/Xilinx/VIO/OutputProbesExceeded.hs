{-# LANGUAGE TemplateHaskell #-}

module OutputProbesExceeded where

import Clash.Prelude
import Clash.Cores.Xilinx.VIO

import qualified Data.List as L

type Dom = XilinxSystem

inNames = Nil
outNames = $(listToVecTH (L.map (("probe_out_" <>) . show) [0::Int, 1..256]))

topEntity ::
  "clk" ::: Clock Dom ->
  "out" ::: Signal Dom (Vec 257 Bit)
topEntity = vioProbe @Dom inNames outNames (replicate (SNat @257) low)
