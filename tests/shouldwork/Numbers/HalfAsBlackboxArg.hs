module HalfAsBlackboxArg where

import Clash.Prelude
import Numeric.Half
import Clash.Explicit.Testbench
import Foreign.C.Types

g :: KnownNat n => Vec n Half -> Half
g v =
  case v of
    Nil -> Half 5
    a :> as -> a

topEntity =
  ( g (replicate d3 (Half 7))
  , g (replace 0 (Half 8) (replicate d3 (Half 7)))
  )
