module FixedS where

import CLaSH.Prelude

type SF = SFixed 16 20

co = $$(fLit 4.578) :: SFixed 16 20

topEntity :: SFixed 16 20 -> SFixed 16 20
topEntity x = x + co
