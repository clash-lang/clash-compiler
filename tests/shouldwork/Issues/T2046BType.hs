module T2046BType where

import Clash.Prelude

type T2046B =
  ( (Index 1, Index 2)
  , (Unsigned 1, Unsigned 2)
  , (Signed 1, Signed 2)
  , (BitVector 1, BitVector 2)
  , Bit
  )

type T2046C =
  ( (Int, Int)
  , (Int, Int)
  , (Int, Int)
  , (Int, Int)
  , Int
  )

