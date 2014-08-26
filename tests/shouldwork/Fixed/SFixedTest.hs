module SFixedTest where

import CLaSH.Prelude

type SF = SFixed 20 18

topEntity :: SF -> SF
topEntity x = x * 3.56
