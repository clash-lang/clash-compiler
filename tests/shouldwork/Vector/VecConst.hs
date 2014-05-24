module VecConst where

import CLaSH.Prelude

topEntity :: Vec 4 Int -> Vec 2 (Vec 4 Int)
topEntity a = $(v [1::Int,2,3,4]) :> a :> Nil
