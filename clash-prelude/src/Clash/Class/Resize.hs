{-|
Copyright  :  (C) 2013-2016, University of Twente
License    :  BSD2 (see the file LICENSE)
Maintainer :  Christiaan Baaij <christiaan.baaij@gmail.com>
-}

{-# LANGUAGE Safe #-}

{-# OPTIONS_GHC -fplugin GHC.TypeLits.KnownNat.Solver #-}
{-# OPTIONS_HADDOCK show-extensions #-}

module Clash.Class.Resize where

import Data.Kind (Type)
import GHC.TypeLits (Nat, KnownNat, type (+))

-- | Coerce a value to be represented by a different number of bits
class Resize (f :: Nat -> Type) where
  -- | A sign-preserving resize operation
  --
  -- * For signed datatypes: Increasing the size of the number replicates the
  -- sign bit to the left. Truncating a number to length L keeps the sign bit
  -- and the rightmost L-1 bits.
  --
  -- * For unsigned datatypes: Increasing the size of the number extends with
  -- zeros to the left. Truncating a number of length N to a length L just
  -- removes the left (most significant) N-L bits.
  resize :: (KnownNat a, KnownNat b) => f a -> f b
  -- | Perform a 'zeroExtend' for unsigned datatypes, and 'signExtend' for a
  -- signed datatypes
  extend :: (KnownNat a, KnownNat b) => f a -> f (b + a)
  extend = resize
  -- | Add extra zero bits in front of the MSB
  zeroExtend :: (KnownNat a, KnownNat b) => f a -> f (b + a)
  -- | Add extra sign bits in front of the MSB
  signExtend :: (KnownNat a, KnownNat b) => f a -> f (b + a)
  signExtend = resize
  -- | Remove bits from the MSB
  truncateB :: KnownNat a => f (a + b) -> f a
