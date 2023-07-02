{-# LANGUAGE CPP #-}

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
-- See: https://github.com/clash-lang/clash-compiler/pull/2511
{-# CLASH_OPAQUE topEntity #-}

testBench
  :: Signal P10 Bool
testBench = tb topEntity
               $(listToVecTH $ sampleN 20 $ tbOutput ram clockGen clockGen)
-- See: https://github.com/clash-lang/clash-compiler/pull/2511
{-# CLASH_OPAQUE testBench #-}
