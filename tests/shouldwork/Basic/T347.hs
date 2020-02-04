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

-- type equality is symmetrical so this should also work:
topEntity2
    :: (System ~ dom)
    => Clock dom
    -> Signal dom Bit
    -> Signal dom Bit
topEntity2 = exposeClock board
  where
    board = id
{-# ANN topEntity2 (defSyn "topEntity2") #-}
