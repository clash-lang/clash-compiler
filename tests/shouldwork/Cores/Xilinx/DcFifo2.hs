module DcFifo2 where

import Clash.Cores.Xilinx.DcFifo
import Clash.Explicit.Prelude

import DcFifo.Abstract

topEntity :: ConfiguredFifo (Unsigned 16) Dom2 Dom2
topEntity = dcFifo defConfig
{-# NOINLINE topEntity #-}

testBench :: Signal Dom2 Bool
testBench = mkTestBench topEntity
{-# NOINLINE testBench #-}
