{-# LANGUAGE FlexibleInstances #-}
module BlockRamTest where

import Clash.Prelude

topEntity
  :: Clock System
  -> Enable System
  -> Signal System (Unsigned 7)
  -> Signal System (Maybe (Unsigned 7, Vec 4 Bit))
  -> Signal System (Vec 4 Bit)
topEntity clk en =
  exposeEnable (exposeClock (blockRam (replicate d128 (repeat high))) clk) en
