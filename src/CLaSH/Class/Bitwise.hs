module CLaSH.Class.Bitwise where

import CLaSH.Sized.BitVector

class Bitwise a where
  (.&.)      :: a -> a -> a
  (.|.)      :: a -> a -> a
  xor        :: a -> a -> a
  xnor       :: a -> a -> a
  complement :: a -> a
  shiftL     :: Integral i => a -> i -> a
  shiftR     :: Integral i => a -> i -> a
  rotateL    :: Integral i => a -> i -> a
  rotateR    :: Integral i => a -> i -> a
  msb        :: a -> Bit
  lsb        :: a -> Bit
