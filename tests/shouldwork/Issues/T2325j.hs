
{-# LANGUAGE CPP #-}

module T2325j where

import Clash.Prelude

j :: Unsigned 8 -> Unsigned 8
j = (+ 10)
-- See: https://github.com/clash-lang/clash-compiler/pull/2511
{-# CLASH_OPAQUE j #-}
{-# ANN j (defSyn "j") #-}
