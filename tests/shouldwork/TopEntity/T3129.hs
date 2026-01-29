module T3129 where

import Clash.Prelude

import GHC.Base        (isTrue#)
import GHC.Num.Natural (Natural (NS), naturalLogBase#)
import GHC.Prim        (eqWord#, plusWord#)

naturalCLogBase :: Natural -> Natural -> Natural
naturalCLogBase x y =
             let z1 = naturalLogBase# x y
                 z2 = naturalLogBase# x (y-1)
             in  case y of
                    1 -> 0
                    _ | isTrue# (z1 `eqWord#` z2) -> NS (z1 `plusWord#` 1##)
                      | otherwise                 -> NS z1

f :: Natural -> Natural
f m = 2 * (naturalCLogBase 2 m)

g :: Natural -> Natural
g m =
  let i = f m
      calc 0 _   _   = 1
      calc 1 val tmp = (val * tmp) `mod` m
      calc n val tmp = calc
          (if n `mod` 2 == 0 then n `div` 2         else n - 1              )
          (if n `mod` 2 == 0 then val * val `mod` m else val                )
          (if n `mod` 2 == 0 then tmp `mod` m       else (tmp * val) `mod` m)
   in calc (m - i - 1) 2 1

topEntity :: Natural
topEntity = g 0xffffffff00000001000000000000000000000000ffffffffffffffffffffffff
