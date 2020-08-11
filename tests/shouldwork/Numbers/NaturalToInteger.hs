module NaturalToInteger where

import Clash.Prelude
import GHC.Natural

topEntity :: Natural
topEntity = snatToNum $ clogBaseSNat d2 d1024

