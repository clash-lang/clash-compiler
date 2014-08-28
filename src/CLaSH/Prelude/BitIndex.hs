{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}

module CLaSH.Prelude.BitIndex where

import GHC.TypeLits                   (KnownNat, type (+), type (-))

import CLaSH.Class.BitConvert         (BitConvert (..))
import CLaSH.Promoted.Nat             (SNat)
import CLaSH.Sized.Internal.BitVector (BitVector, Bit, index#, lsb#, msb#,
                                       replaceBit#, setSlice#, slice#, split#)

{-# INLINE (!) #-}
-- | Get the bit at the specified bit index.
--
-- __NB:__ Bit indices are __DESCENDING__.
--
-- >>> pack (7 :: Unsigned 6)
-- 000111
-- >>> (7 :: Unsigned 6) ! 1
-- 1
-- >>> (7 :: Unsigned 6) ! 5
-- 0
-- >>> (7 :: Unsigned 6) ! 6
-- *** Exception: (!): 6 is out of range [5..0]
(!) :: (BitConvert a, KnownNat (BitSize a), Integral i) => a -> i -> Bit
(!) v i = index# (pack v) (fromIntegral i)

{-# INLINE slice #-}
-- | Get a slice between bit index @m@ and and bit index @n@.
--
-- __NB:__ Bit indices are __DESCENDING__.
--
-- >>> pack (7 :: Unsigned 6)
-- 000111
-- >>> slice (7 :: Unsigned 6) d4 d2
-- 001
-- >>> slice (7 :: Unsigned 6) d6 d4
--   <interactive>
--       Couldn't match type ‘7 + i0’ with ‘6’
--       The type variable ‘i0’ is ambiguous
--       Expected type: (6 + 1) + i0
--         Actual type: BitSize (Unsigned 6)
--       In the expression: slice (7 :: Unsigned 6) d6 d4
--       In an equation for ‘it’: it = slice (7 :: Unsigned 6) d6 d4
slice :: (BitConvert a, BitSize a ~ ((m + 1) + i)) => a -> SNat m -> SNat n
      -> BitVector (m + 1 - n)
slice v m n = slice# (pack v) m n

{-# INLINE split #-}
-- | Split a value of a bit size @m + n@ into a tuple of values with size @m@
-- and size @n@.
--
-- >>> pack (7 :: Unsigned 6)
-- 000111
-- >>> split (7 :: Unsigned 6) :: (BitVector 2, BitVector 4)
-- (00,0111)
split :: (BitConvert a, BitSize a ~ (m + n), KnownNat n) => a
      -> (BitVector m, BitVector n)
split v = split# (pack v)

{-# INLINE replaceBit #-}
-- | Set the bit at the specified index
--
-- __NB:__ Bit indices are __DESCENDING__.
--
-- >>> pack (-5 :: Signed 6)
-- 111011
-- >>> replaceBit (-5 :: Signed 6) 4 0
-- -21
-- >>> pack (-21 :: Signed 6)
-- 101011
-- >>> replaceBit (-5 :: Signed 6) 5 0
-- 27
-- >>> pack (27 :: Signed 6)
-- 011011
-- >>> replaceBit (-5 :: Signed 6) 6 0
-- *** Exception: replaceBit: 6 is out of range [5..0]
replaceBit :: (BitConvert a, KnownNat (BitSize a), Integral i) => a -> i -> Bit
           -> a
replaceBit v i b = unpack (replaceBit# (pack v) (fromIntegral i) b)

{-# INLINE setSlice #-}
-- | Set the bits between bit index @m@ and bit index @n@.
--
-- __NB:__ Bit indices are __DESCENDING__.
--
-- >>> pack (-5 :: Signed 6)
-- 111011
-- >>> setSlice (-5 :: Signed 6) d4 d3 0
-- -29
-- >>> pack (-29 :: Signed 6)
-- 100011
-- >>> setSlice (-5 :: Signed 6) d6 d5 0
--   <interactive>:25:1:
--       Couldn't match type ‘7 + i0’ with ‘6’
--       The type variable ‘i0’ is ambiguous
--       Expected type: (6 + 1) + i0
--         Actual type: BitSize (Signed 6)
--       In the expression: setSlice (- 5 :: Signed 6) d6 d5 0
--       In an equation for ‘it’: it = setSlice (- 5 :: Signed 6) d6 d5 0
setSlice :: (BitConvert a, BitSize a ~ ((m + 1) + i)) => a -> SNat m -> SNat n
         -> BitVector (m + 1 - n) -> a
setSlice v m n w = unpack (setSlice# (pack v) m n w)

{-# INLINE msb #-}
-- | Get the most significant bit.
--
-- >>> pack (-4 :: Signed 6)
-- 111100
-- >>> msb (-4 :: Signed 6)
-- 1
-- >>> pack (4 :: Signed 6)
-- 000100
-- >>> msb (4 :: Signed 6)
-- 0
msb :: (BitConvert a, KnownNat (BitSize a)) => a -> Bit
msb v = msb# (pack v)

{-# INLINE lsb #-}
-- | Get the least significant bit.
--
-- >>> pack (-9 :: Signed 6)
-- 110111
-- >>> lsb (-9 :: Signed 6)
-- 1
-- >>> pack (-8 :: Signed 6)
-- 111000
-- >>> lsb (-8 :: Signed 6)
-- 0
lsb :: BitConvert a => a -> Bit
lsb v = lsb# (pack v)
