module DcFifo0 where

import Clash.Cores.Xilinx.DcFifo
import Clash.Explicit.Prelude

import DcFifo.Abstract

topEntity :: ConfiguredFifo Dom3 Dom2
topEntity = dcFifo defConfig
{-# NOINLINE topEntity #-}

testBench :: Signal Dom3 Bool
testBench = mkTestBench topEntity
{-# NOINLINE testBench #-}
