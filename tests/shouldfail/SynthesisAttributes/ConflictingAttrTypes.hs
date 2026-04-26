{-# OPTIONS_GHC -O0 #-}
{-# LANGUAGE TemplateHaskell #-}

module ConflictingAttrTypes where

import Clash.Prelude
import Clash.Annotations.SynthesisAttributes

topEntity ::
  Clock System ->
  Reset System ->
  Enable System ->
  Signal System (BitVector 8) ->
  ( Signal System (BitVector 8) `Annotate` 'StringAttr "foo" "B1"
  , Signal System (BitVector 8)
  )
topEntity clk rst ena i = (leds, leds2)
 where
  leds = withClockResetEnable clk rst ena $ register 0 (liftA2 xor i next)

  leds2 = withClockResetEnable clk rst ena $ register 0 (liftA2 xor i next)

  next :: Signal System (BitVector 8) `Annotate` 'BoolAttr "foo" True
  next = withClockResetEnable clk rst ena $ register 0 leds
{-# OPAQUE topEntity #-}
