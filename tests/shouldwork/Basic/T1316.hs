module T1316 where

import Clash.Prelude

incr :: Index 2 -> Index 2
incr i = if i == maxBound then 0 else i + 1
{-# NOINLINE incr #-}

topEntity :: Index 10 -> Index 2
topEntity j = case j < 1 of
  False ->
    let xs = init (Cons 1 (Cons (incr (head xs)) Nil)) in last xs
  True ->
    let ys = init (Cons 2 (Cons (incr (last ys)) Nil)) in head ys
