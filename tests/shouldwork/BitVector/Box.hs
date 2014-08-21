module Box where

import CLaSH.Prelude

topEntity :: BitVector 16 -> BitVector 16
topEntity vec = pack tup
  where
    tup :: (Vec 8 Bit, Vec 8 Bit)
    tup = unpack vec

