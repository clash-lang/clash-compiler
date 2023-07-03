{-# LANGUAGE CPP #-}

{-# OPTIONS_GHC -O0 #-}
module T1606B where

import Clash.Prelude

f :: Vec 8 (Clock System)
  -> Vec 8 (Clock System)
f = id
-- See: https://github.com/clash-lang/clash-compiler/pull/2511
{-# CLASH_OPAQUE f #-}

g :: Vec 8 (Clock System)
  -> Vec 8 (Clock System)
g = f
-- See: https://github.com/clash-lang/clash-compiler/pull/2511
{-# CLASH_OPAQUE g #-}

topEntity ::
     Vec 8 (Clock System)
  -> Vec 8 (Clock System)
topEntity = g
