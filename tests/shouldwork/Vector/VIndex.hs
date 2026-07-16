{-# OPTIONS_GHC -Wno-deprecations #-}

module VIndex where

import Clash.Prelude

topEntity :: (Integer,Vec 8 (Vec 8 (Maybe Int))) -> Vec 8 (Maybe Int)
topEntity (i,as) = zipWith indexEnum as (iterateI (+1) i)
