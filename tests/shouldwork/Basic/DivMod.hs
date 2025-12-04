{-# LANGUAGE CPP #-}

module DivMod where

import Clash.Prelude

topEntity :: (Integer,Integer)
topEntity = topEntity1 height depthInput filterHeight stride cycles
  where
    height = SNat @5
    filterHeight = SNat @3
    stride = SNat @2
    depthInput = SNat @2
    cycles = SNat @800

topEntity1 height depthInput filterHeight stride cycles = snatToNum cycles `divMod` pools
  where
    pools :: Integer
    pools =
      rows *
      (((snatToNum height - snatToNum filterHeight) `div` snatToNum stride) + 1) *
      snatToNum depthInput

    rows :: Integer
    rows = ((snatToNum height - snatToNum filterHeight) `div` snatToNum stride) + 1
{-# OPAQUE topEntity1 #-}
