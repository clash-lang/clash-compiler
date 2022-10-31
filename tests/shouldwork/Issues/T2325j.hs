
module T2325j where

import Clash.Prelude

j :: Unsigned 8 -> Unsigned 8
j = (+ 10)
{-# NOINLINE j #-}
{-# ANN j (defSyn "j") #-}
