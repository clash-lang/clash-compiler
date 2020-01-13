module T701 where

import Clash.Prelude
{-# NOINLINE myNot #-}
{-# ANN myNot (defSyn "mynot") #-}
myNot = not

topEntity
  :: Signal System Bool
  -> Signal System Bool
topEntity inp = myNot <$> inp
