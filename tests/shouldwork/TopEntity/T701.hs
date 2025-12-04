{-# LANGUAGE CPP #-}

module T701 where

import Clash.Prelude
{-# OPAQUE myNot #-}
{-# ANN myNot (defSyn "mynot") #-}
myNot = not

topEntity
  :: Signal System Bool
  -> Signal System Bool
topEntity inp = myNot <$> inp
