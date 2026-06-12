module LiftRecursiveGroup where

import Clash.Prelude

topEntity x y z
  = let g p q k v = k (p + q) * v
        h = ((3 :: Int) *) . (g x y f)
        f = ((4 :: Int) *) . (g x y h)
    in  f (h z) + h (f z)
