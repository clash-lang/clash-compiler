module T967c where

import Clash.Prelude

topEntity x = let (y :: Vec 2 Bool) = tail (lazyV (x:>y)) in y
