{-# LANGUAGE ViewPatterns #-}
module XToError where

import Clash.Prelude

topEntity :: Bit -> BitVector 8 -> BitVector 8
topEntity (xToError -> a) (xToError -> b) = slice d7 d0 (pack a ++# b)
