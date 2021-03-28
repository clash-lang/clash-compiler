{-# OPTIONS_GHC -O0 #-}
module T1606B where

import Clash.Prelude

f :: Vec 8 (Clock System)
  -> Vec 8 (Clock System)
f = id
{-# NOINLINE f #-}

g :: Vec 8 (Clock System)
  -> Vec 8 (Clock System)
g = f
{-# NOINLINE g #-}

topEntity ::
     Vec 8 (Clock System)
  -> Vec 8 (Clock System)
topEntity = g
