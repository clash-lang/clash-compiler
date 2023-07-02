{-# LANGUAGE CPP #-}

module T2325g where

import Clash.Prelude

import T2325h
import T2325j

g :: Unsigned 8 -> Unsigned 8
g = h . j
-- See: https://github.com/clash-lang/clash-compiler/pull/2511
{-# CLASH_OPAQUE g #-}
{-# ANN g (defSyn "g") #-}
