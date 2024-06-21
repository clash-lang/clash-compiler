{-# LANGUAGE TemplateHaskell #-}

module VecConst where

import Clash.Prelude

topEntity :: Vec 4 Int -> Vec 2 (Vec 4 Int)
topEntity a = $(listToVecTH [1::Int,2,3,4]) :> a :> Nil
