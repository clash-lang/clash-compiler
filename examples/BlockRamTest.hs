module BlockRamTest where

import CLaSH.Prelude

topEntity :: Signal (Unsigned 7)
          -> Signal (Maybe (Unsigned 7,Unsigned 4))
          -> Signal (Unsigned 4)
topEntity = blockRamPow2 (repeat 0)
