{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE KindSignatures   #-}
{-# LANGUAGE MagicHash        #-}
{-# LANGUAGE TypeOperators    #-}
module CLaSH.Class.BitIndex
  ( BitIndex (..)
  , bit
  )
where

import GHC.TypeLits                   (KnownNat, Nat, type (+), type (-))

import CLaSH.Promoted.Nat             (SNat)
import CLaSH.Sized.Internal.BitVector (BitVector, Bit, (!#), lsb#, msb#, slice#,
                                       setBit#, setSlice#, split#)

class BitIndex (a :: Nat -> *) where
  -- | Get the bit at the specified bit index.
  --
  -- __NB:__ Bit indices are __DESCENDING__.
  (!)      :: (KnownNat n, Integral i) => a n -> i -> Bit
  -- | Get a slice between bit index @m@ and and bit index @n@.
  --
  -- __NB:__ Bit indices are __DESCENDING__.
  slice    :: a (m + 1 + i) -> SNat m -> SNat n -> a (m + 1 - n)
  -- | Split a value of a bit size @m + n@ into a tuple of values with size @m@
  -- and size @n@.
  split    :: KnownNat n => a (m + n) -> (a m, a n)
  -- | Set the bit at the specified index
  --
  -- __NB:__ Bit indices are __DESCENDING__.
  setBit   :: (KnownNat n, Integral i) => a n -> i -> a n
  -- | Set the bits between bit index @m@ and bit index @n@.
  --
  -- __NB:__ Bit indices are __DESCENDING__.
  setSlice :: a (m + 1 + i) -> SNat m -> SNat n -> a (m + 1 - n)
           -> a (m + 1 + i)
  -- | Get the most significant bit.
  msb      :: KnownNat n => a n -> Bit
  -- | Get the least significant bit.
  lsb      :: KnownNat n => a n -> Bit

instance BitIndex BitVector where
  (!) v i     = (!#) v (fromIntegral i)
  slice       = slice#
  split       = split#
  setBit v i  = setBit# v (fromIntegral i)
  setSlice    = setSlice#
  msb         = msb#
  lsb         = lsb#

bit :: (KnownNat n, Num (a n), BitIndex a, Integral i) => i -> a n
bit i = setBit 0 i
