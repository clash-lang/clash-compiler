{-# LANGUAGE FlexibleInstances #-}
module BlockRamTest where

import CLaSH.Prelude

topEntity :: Signal (Unsigned 7)
          -> Signal (Maybe (Unsigned 7, Vec 4 Bit))
          -> Signal (Vec 4 Bit)
topEntity = blockRam (replicate d128 (repeat high))
