module SatMult where

import CLaSH.Prelude

topEntity :: Signed 3 -> Signed 3 -> Signed 3
topEntity = satMult SatSymmetric
