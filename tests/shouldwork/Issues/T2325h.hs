{-# LANGUAGE CPP #-}

module T2325h where

import Clash.Prelude

h :: Unsigned 8 -> Unsigned 8
h = (+ 5)
-- See: https://github.com/clash-lang/clash-compiler/pull/2511
{-# CLASH_OPAQUE h #-}
{-# ANN h (defSyn "h") #-}
