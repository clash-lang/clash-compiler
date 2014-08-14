{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}

module CLaSH.Prelude.BitIndex where

import GHC.TypeLits                   (KnownNat, type (+), type (-), natVal)

import CLaSH.Class.BitConvert         (BitConvert (..))
import CLaSH.Promoted.Nat             (SNat)
import CLaSH.Sized.Internal.BitVector (BitVector, Bit, index#, lsb#, msb#,
                                       replaceBit#, setSlice#, slice#, split#)

-- | Get the bit at the specified bit index.
--
-- __NB:__ Bit indices are __DESCENDING__.
(!) :: (BitConvert a, KnownNat (BitSize a), Integral i) => a -> i -> Bit
(!) v i = index# (natVal v') v' (toInteger i)
  where
    v' = pack v

-- | Get a slice between bit index @m@ and and bit index @n@.
--
-- __NB:__ Bit indices are __DESCENDING__.
slice :: (BitConvert a, BitSize a ~ ((m + 1) + i)) => a -> SNat m -> SNat n
      -> BitVector (m + 1 - n)
slice v m n = slice# (pack v) m n

-- | Split a value of a bit size @m + n@ into a tuple of values with size @m@
-- and size @n@.
split :: (BitConvert a, BitSize a ~ (m + n), KnownNat n) => a
      -> (BitVector m, BitVector n)
split v = split# (pack v)

-- | Set the bit at the specified index
--
-- __NB:__ Bit indices are __DESCENDING__.
replaceBit :: (BitConvert a, KnownNat (BitSize a), Integral i) => a -> i -> Bit
           -> a
replaceBit v i = unpack (replaceBit# (natVal v') v' (toInteger i))
  where
    v' = pack v

-- | Set the bits between bit index @m@ and bit index @n@.
--
-- __NB:__ Bit indices are __DESCENDING__.
setSlice :: (BitConvert a, BitSize a ~ ((m + 1) + i)) => a -> SNat m -> SNat n
         -> BitVector (m + 1 - n) -> a
setSlice v m n w = unpack (setSlice# (pack v) m n w)

-- | Get the most significant bit.
msb :: (BitConvert a, KnownNat (BitSize a)) => a -> Bit
msb v = msb# (pack v)

-- | Get the least significant bit.
lsb :: BitConvert a => a -> Bit
lsb v = lsb# (pack v)
