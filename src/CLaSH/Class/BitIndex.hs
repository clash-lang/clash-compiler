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

import GHC.TypeLits          (KnownNat, Nat, type (+), type (-))

import CLaSH.Promoted.Nat    (SNat)
import CLaSH.Sized.BitVector (BitVector, Bit, (!#), lsb#, msb#, slice#, setBit#,
                              setSlice#)

class BitIndex (a :: Nat -> *) where
  -- | Assert the bit at the specified index
  --
  -- __NB:__ Uses a /descending/ index
  (!)      :: (KnownNat n, Integral i) => a n -> i -> Bit
  slice    :: a (m + 1 + i) -> SNat m -> SNat n -> a (m + 1 - n)
  -- | Set the bit at the specified index
  --
  -- __NB:__ Uses a /descending/ index
  setBit   :: (KnownNat n, Integral i) => a n -> i -> a n
  setSlice :: a (m + 1 + i) -> SNat m -> SNat n -> a (m + 1 - n)
           -> a (m + 1 + i)
  msb      :: KnownNat n => a n -> Bit
  lsb      :: KnownNat n => a n -> Bit

instance BitIndex BitVector where
  (!) v i     = (!#) v (fromIntegral i)
  slice       = slice#
  setBit v i  = setBit# v (fromIntegral i)
  setSlice    = setSlice#
  msb         = msb#
  lsb         = lsb#

bit :: (KnownNat n, Num (a n), BitIndex a, Integral i) => i -> a n
bit i = setBit 0 i
