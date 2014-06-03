{-# LANGUAGE FlexibleInstances #-}
module BlockRamTest where

import CLaSH.Prelude

topEntity :: Signal (Unsigned 7)
          -> Signal (Unsigned 7)
          -> Signal (Bool)
          -> Signal (Vec 4 Bit)
          -> Signal (Vec 4 Bit)
topEntity = blockRam (snat :: SNat 128)
