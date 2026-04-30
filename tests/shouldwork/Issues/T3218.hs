{-# OPTIONS_GHC -O0 #-}
{-# LANGUAGE TemplateHaskell #-}

module T3218 where

import Clash.Prelude
import Clash.Annotations.SynthesisAttributes

topEntity ::
  Clock System ->
  Reset System ->
  Enable System ->
  Signal System (BitVector 8) ->
  Signal System (BitVector 8) `Annotate` 'StringAttr "foo" "B1"
topEntity clk rst ena i = leds
 where
  leds :: Signal System (BitVector 8) `Annotate` 'StringAttr "foo" "B2"
  leds = withClockResetEnable clk rst ena $ register 0 (liftA2 xor i next)

  next = withClockResetEnable clk rst ena $ register 0 leds
{-# OPAQUE topEntity #-}
