module T347 where

import Clash.Prelude

topEntity
    :: (dom ~ System)
    => Clock dom
    -> Signal dom Bit
    -> Signal dom Bit
topEntity = exposeClock board
  where
    board = id

