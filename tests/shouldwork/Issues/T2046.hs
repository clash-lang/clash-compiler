module T2046 where

import Clash.Prelude
import Data.Proxy

topGeneric
  :: forall ix n
   . Enum ix
  => KnownNat n
  => Proxy ix
  -> Vec n Int
  -> Int
  -> Int
topGeneric Proxy x i =
  x !! toEnum @ix i

topBit
  :: Vec 2 Int
  -> Int
  -> Int
topBit = topGeneric (Proxy @Bit)
{-# NOINLINE topBit #-}
{-# ANN topBit (defSyn "top_bit") #-}

topBitVector
  :: Vec 5 Int
  -> Int
  -> Int
topBitVector = topGeneric (Proxy @(BitVector 3))
{-# NOINLINE topBitVector #-}
{-# ANN topBitVector (defSyn "top_bitvector") #-}

topIndex
  :: Vec 5 Int
  -> Int
  -> Int
topIndex = topGeneric (Proxy @(Index 5))
{-# NOINLINE topIndex #-}
{-# ANN topIndex (defSyn "top_index") #-}

topSigned
  :: Vec 5 Int
  -> Int
  -> Int
topSigned = topGeneric (Proxy @(Signed 4))
{-# NOINLINE topSigned #-}
{-# ANN topSigned (defSyn "top_signed") #-}

topUnsigned
  :: Vec 5 Int
  -> Int
  -> Int
topUnsigned = topGeneric (Proxy @(Unsigned 3))
{-# NOINLINE topUnsigned #-}
{-# ANN topUnsigned (defSyn "top_unsigned") #-}
