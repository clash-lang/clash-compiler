{-# LANGUAGE MagicHash #-}
module CLaSH.Class.Bitwise where

import GHC.TypeLits          (KnownNat)
import CLaSH.Sized.BitVector (BitVector, Bit, (!#), and#, complement#, lsb#,
                              msb#, or#, rotateL#, rotateR#, setBit#, shiftL#,
                              shiftR#, xor#)

class Bitwise a where
  (.&.)      :: a -> a -> a
  (.|.)      :: a -> a -> a
  xor        :: a -> a -> a
  complement :: a -> a
  -- | Assert the bit at the specified index
  --
  -- __NB:__ Uses a /descending/ index
  (!)        :: Integral i => a -> i -> Bit
  -- | Set the bit at the specified index
  --
  -- __NB:__ Uses a /descending/ index
  setBit     :: Integral i => a -> i -> a
  shiftL     :: Integral i => a -> i -> a
  shiftR     :: Integral i => a -> i -> a
  rotateL    :: Integral i => a -> i -> a
  rotateR    :: Integral i => a -> i -> a
  msb        :: a -> Bit
  lsb        :: a -> Bit

instance KnownNat n => Bitwise (BitVector n) where
  (.&.)       = and#
  (.|.)       = or#
  xor         = xor#
  complement  = complement#
  (!) v i     = (!#) v (fromIntegral i)
  setBit v i  = setBit# v (fromIntegral i)
  shiftL v i  = shiftL# v (fromIntegral i)
  shiftR v i  = shiftR# v (fromIntegral i)
  rotateL v i = rotateL# v (fromIntegral i)
  rotateR v i = rotateR# v (fromIntegral i)
  msb         = msb#
  lsb         = lsb#
