module HalfAsBlackboxArg where

import Clash.Explicit.Testbench
import Clash.Prelude
import Foreign.C.Types
import Numeric.Half

g :: (KnownNat n) => Vec n Half -> Half
g v =
  case v of
    Nil -> Half 5
    Cons a as -> a

topEntity =
  ( g (replicate d3 (Half 7))
  , g (replace 0 (Half 8) (replicate d3 (Half 7)))
  )
