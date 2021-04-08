module T1756 where

import Clash.Prelude

topEntity a = register @System (unconcat d2 (replicate d8 False)) a
