{-# LANGUAGE CPP #-}

module T701 where

import Clash.Prelude
-- See: https://github.com/clash-lang/clash-compiler/pull/2511
{-# CLASH_OPAQUE myNot #-}
{-# ANN myNot (defSyn "mynot") #-}
myNot = not

topEntity
  :: Signal System Bool
  -> Signal System Bool
topEntity inp = myNot <$> inp
