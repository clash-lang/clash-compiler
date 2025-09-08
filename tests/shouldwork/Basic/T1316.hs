{-# LANGUAGE CPP #-}

module T1316 where

import Clash.Prelude

incr :: Index 2 -> Index 2
incr i = if i == maxBound then 0 else i + 1
-- See: https://github.com/clash-lang/clash-compiler/pull/2511
{-# CLASH_OPAQUE incr #-}

topEntity :: Index 10 -> Index 2
topEntity j = case j < 1 of
  False ->
    let xs = init (1 :> (incr (head xs) :> Nil)) in last xs
  True ->
    let ys = init (2 :> (incr (last ys) :> Nil)) in head ys
