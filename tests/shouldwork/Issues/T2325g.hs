module T2325g where

import Clash.Prelude

import T2325h
import T2325j

g :: Unsigned 8 -> Unsigned 8
g = h . j
{-# NOINLINE g #-}
{-# ANN g (defSyn "g") #-}
