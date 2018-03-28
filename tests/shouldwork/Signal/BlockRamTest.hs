{-# LANGUAGE FlexibleInstances #-}
module BlockRamTest where

import Clash.Prelude

topEntity
  :: Clock System Source
  -> Signal System (Unsigned 7)
  -> Signal System (Maybe (Unsigned 7, Vec 4 Bit))
  -> Signal System (Vec 4 Bit)
topEntity = exposeClock (blockRam (replicate d128 (repeat high)))
