{-|
Copyright  :  (C) 2013-2016, University of Twente
                  2022     , Google Inc.
License    :  BSD2 (see the file LICENSE)
Maintainer :  Christiaan Baaij <christiaan.baaij@gmail.com>
-}

{-# LANGUAGE Trustworthy #-}

{-# OPTIONS_HADDOCK show-extensions #-}

module Clash.Sized.BitVector
  ( -- * Bit
    Bit
    -- ** Construction
    -- *** Initialisation
  , high
  , low
    -- * BitVector
  , BitVector
    -- ** Accessors
    -- *** Length information
  , size#
  , maxIndex#
    -- ** Construction
  , bLit
  , hLit
  , oLit
    -- ** Concatenation
  , (++#)
    -- * Modification
  , (+>>.)
  , (.<<+)
    -- ** Pattern matching
  , bitPattern
  )
where

import Clash.Sized.Internal.BitVector
import Clash.Promoted.Nat (natToNum)
import Data.Bits (shiftL, shiftR)
import GHC.TypeNats (KnownNat)

{- $setup
>>> :set -XNumericUnderscores
-}

infixr 4 +>>.
-- | Shift in a bit from the MSB side of a 'BitVector'. Equal to right shifting
-- the 'BitVector' by one and replacing the MSB with the bit to be shifted in.
--
-- >>> 1 +>>. 0b1111_0000 :: BitVector 8
-- 0b1111_1000
-- >>> 0 +>>. 0b1111_0000 :: BitVector 8
-- 0b0111_1000
--
(+>>.) :: forall n. KnownNat n => Bit -> BitVector n -> BitVector n
b +>>. bv = replaceBit# (shiftR bv 1) (natToNum @n - 1) b

infixr 4 .<<+
-- | Shift in a bit from the LSB side of a 'BitVector'. Equal to left shifting
-- the 'BitVector' by one and replacing the LSB with the bit to be shifted in.
--
-- >>> 0b1111_0000 .<<+ 0 :: BitVector 8
-- 0b1110_0000
-- >>> 0b1111_0000 .<<+ 1 :: BitVector 8
-- 0b1110_0001
--
(.<<+) :: forall n. KnownNat n => BitVector n -> Bit -> BitVector n
bv .<<+ b = replaceBit# (shiftL bv 1) 0 b
