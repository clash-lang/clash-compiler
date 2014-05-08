module Box where

import CLaSH.Prelude

topEntity :: Vec 16 Bit -> Vec 16 Bit
topEntity vec = toBV tup
  where
    tup :: (Vec 8 Bit, Vec 8 Bit)
    tup = fromBV vec

