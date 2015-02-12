module HOClock where

import CLaSH.Prelude

topEntity :: Vec 8 (Signal Int) -> Vec 8 (Signal Int)
topEntity = map (register 0)
