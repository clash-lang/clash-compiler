{-# LANGUAGE CPP #-}

{-# OPTIONS_GHC -O0 #-}
module T1606B where

import Clash.Prelude

f :: Vec 8 (Clock System)
  -> Vec 8 (Clock System)
f = id
{-# OPAQUE f #-}

g :: Vec 8 (Clock System)
  -> Vec 8 (Clock System)
g = f
{-# OPAQUE g #-}

topEntity ::
     Vec 8 (Clock System)
  -> Vec 8 (Clock System)
topEntity = g
