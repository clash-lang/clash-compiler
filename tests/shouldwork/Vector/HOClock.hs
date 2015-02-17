module HOClock where

import CLaSH.Prelude

topEntity :: Vec 8 (Signal (Int,Int)) -> Vec 8 (Signal (Int,Int))
topEntity = map (register (0,0))
