{-# LANGUAGE StrictData #-}
module BangData where

import Clash.Prelude

data BangData
  = BangData
  { d1 :: !(SatIndex 'SatError 15)
  , d2 :: !(Unsigned 8)
  }

topEntity (BangData x y) = BangData (x+1) (2*y)
