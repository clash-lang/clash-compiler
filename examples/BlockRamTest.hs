{-# LANGUAGE DataKinds #-}
module BlockRamTest where

import CLaSH.Prelude

topEntity :: Signal (Unsigned 7)
          -> Signal (Unsigned 7)
          -> Signal (Bool)
          -> Signal (Unsigned 4)
          -> Signal (Unsigned 4)
topEntity = blockRamPow2 (snat :: SNat 128)
