module FloatPack where

import Clash.Prelude

topEntity :: (Float,Double,BitVector 32,BitVector 64)
          -> (BitVector 32, BitVector 64, Float, Double)
topEntity (f,d,bv32,bv64) = (pack f, pack d, unpack bv32, unpack bv64)
