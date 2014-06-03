{-# LANGUAGE FlexibleInstances #-}
module BlockRamTest where

import CLaSH.Prelude

instance Default (Vec 4 Bit) where def = vcopyI H

topEntity :: Signal (Unsigned 7)
          -> Signal (Unsigned 7)
          -> Signal (Bool)
          -> Signal (Vec 4 Bit)
          -> Signal (Vec 4 Bit)
topEntity = blockRam (snat :: SNat 128)
