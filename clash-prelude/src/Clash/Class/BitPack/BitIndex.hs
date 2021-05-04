{-|
Copyright  :  (C) 2013-2016, University of Twente
                       2021, QBayLogic B.V.
License    :  BSD2 (see the file LICENSE)
Maintainer :  QBayLogic B.V. <devops@qbaylogic.com>
-}

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

{-# LANGUAGE Trustworthy #-}

{-# OPTIONS_HADDOCK show-extensions #-}

module Clash.Class.BitPack.BitIndex where

import GHC.TypeLits                   (KnownNat, type (+), type (-))

import Clash.Class.BitPack.Internal   (BitPack (..))
import Clash.Promoted.Nat             (SNat (..))
import Clash.Sized.Internal.BitVector
  (BitVector, Bit, index#, lsb#, msb#, replaceBit#, setSlice#, slice#, split#)

{- $setup
>>> :set -XDataKinds
>>> import Clash.Prelude
-}

{-# INLINE (!) #-}
-- | Get the bit at the specified bit index.
--
-- __NB:__ Bit indices are __DESCENDING__.
--
-- >>> pack (7 :: Unsigned 6)
-- 0b00_0111
-- >>> (7 :: Unsigned 6) ! 1
-- 1
-- >>> (7 :: Unsigned 6) ! 5
-- 0
-- >>> (7 :: Unsigned 6) ! 6
-- *** Exception: (!): 6 is out of range [5..0]
-- ...
(!) :: (BitPack a, Enum i) => a -> i -> Bit
(!) v i = index# (pack v) (fromEnum i)

{-# INLINE slice #-}
-- | Get a slice between bit index @m@ and and bit index @n@.
--
-- __NB:__ Bit indices are __DESCENDING__.
--
-- >>> pack (7 :: Unsigned 6)
-- 0b00_0111
-- >>> slice d4 d2 (7 :: Unsigned 6)
-- 0b001
-- >>> slice d6 d4 (7 :: Unsigned 6)
-- <BLANKLINE>
-- <interactive>:...
--     • Couldn't match type ‘7 + i0’ with ‘6’
--         arising from a use of ‘slice’
--       The type variable ‘i0’ is ambiguous
--     • In the expression: slice d6 d4 (7 :: Unsigned 6)
--       In an equation for ‘it’: it = slice d6 d4 (7 :: Unsigned 6)
slice
  :: (BitPack a, BitSize a ~ ((m + 1) + i))
  => SNat m
  -> SNat n
  -> a
  -> BitVector (m + 1 - n)
slice m n v = slice# (pack v) m n

{-# INLINE split #-}
-- | Split a value of a bit size @m + n@ into a tuple of values with size @m@
-- and size @n@.
--
-- >>> pack (7 :: Unsigned 6)
-- 0b00_0111
-- >>> split (7 :: Unsigned 6) :: (BitVector 2, BitVector 4)
-- (0b00,0b0111)
split
  :: (BitPack a, BitSize a ~ (m + n), KnownNat n)
  => a
  -> (BitVector m, BitVector n)
split v = split# (pack v)

{-# INLINE replaceBit #-}
-- | Set the bit at the specified index
--
-- __NB:__ Bit indices are __DESCENDING__.
--
-- >>> pack (-5 :: Signed 6)
-- 0b11_1011
-- >>> replaceBit 4 0 (-5 :: Signed 6)
-- -21
-- >>> pack (-21 :: Signed 6)
-- 0b10_1011
-- >>> replaceBit 5 0 (-5 :: Signed 6)
-- 27
-- >>> pack (27 :: Signed 6)
-- 0b01_1011
-- >>> replaceBit 6 0 (-5 :: Signed 6)
-- *** Exception: replaceBit: 6 is out of range [5..0]
-- ...
replaceBit :: (BitPack a, Enum i) => i -> Bit -> a -> a
replaceBit i b v = unpack (replaceBit# (pack v) (fromEnum i) b)

{-# INLINE setSlice #-}
-- | Set the bits between bit index @m@ and bit index @n@.
--
-- __NB:__ Bit indices are __DESCENDING__.
--
-- >>> pack (-5 :: Signed 6)
-- 0b11_1011
-- >>> setSlice d4 d3 0 (-5 :: Signed 6)
-- -29
-- >>> pack (-29 :: Signed 6)
-- 0b10_0011
-- >>> setSlice d6 d5 0 (-5 :: Signed 6)
-- <BLANKLINE>
-- <interactive>:...
--     • Couldn't match type ‘7 + i0’ with ‘6’
--         arising from a use of ‘setSlice’
--       The type variable ‘i0’ is ambiguous
--     • In the expression: setSlice d6 d5 0 (- 5 :: Signed 6)
--       In an equation for ‘it’: it = setSlice d6 d5 0 (- 5 :: Signed 6)
setSlice
  :: (BitPack a, BitSize a ~ ((m + 1) + i))
  => SNat m
  -> SNat n
  -> BitVector (m + 1 - n)
  -> a
  -> a
setSlice m n w v = unpack (setSlice# SNat (pack v) m n w)

{-# INLINE msb #-}
-- | Get the most significant bit.
--
-- >>> pack (-4 :: Signed 6)
-- 0b11_1100
-- >>> msb (-4 :: Signed 6)
-- 1
-- >>> pack (4 :: Signed 6)
-- 0b00_0100
-- >>> msb (4 :: Signed 6)
-- 0
msb :: BitPack a => a -> Bit
msb v = msb# (pack v)

{-# INLINE lsb #-}
-- | Get the least significant bit.
--
-- >>> pack (-9 :: Signed 6)
-- 0b11_0111
-- >>> lsb (-9 :: Signed 6)
-- 1
-- >>> pack (-8 :: Signed 6)
-- 0b11_1000
-- >>> lsb (-8 :: Signed 6)
-- 0
lsb :: BitPack a => a -> Bit
lsb v = lsb# (pack v)
