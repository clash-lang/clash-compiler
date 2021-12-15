module RMultiTop where

import Clash.Explicit.Prelude

import RMulti

topEntity
  :: Clock P20
  -> Clock P10
  -> Signal P10 (Unsigned 1)
  -> Signal P20 (Maybe (Unsigned 1, Unsigned 2))
  -> Signal P10 (Unsigned 2)
topEntity = ram
{-# NOINLINE topEntity #-}

testBench
  :: Signal P10 Bool
testBench = tb $(listToVecTH $ sampleN 20 $ tbOutput clockGen clockGen)
{-# NOINLINE testBench #-}
