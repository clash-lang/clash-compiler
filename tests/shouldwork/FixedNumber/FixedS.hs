module FixedS where

import CLaSH.Prelude

type SF = SFixed 16 20

co = $$(fLit 4.578) :: SF

topEntity :: SF -> SF
topEntity x = x + co
