module T1615 where

import Clash.Prelude

data T = A | B | C

f A = (8 :: Unsigned 4)
f B = 6
f _ = 2

topEntity = map @_ @_ @2 f
