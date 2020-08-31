{-|
Copyright  :  (C) 2013-2016, University of Twente
License    :  BSD2 (see the file LICENSE)
Maintainer :  Christiaan Baaij <christiaan.baaij@gmail.com>
-}

{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

{-# LANGUAGE Safe #-}

{-# OPTIONS_HADDOCK show-extensions #-}

module Clash.Class.Num
  ( -- * Arithmetic functions for arguments and results of different precision
    ExtendingNum (..)
    -- * Saturating arithmetic functions
  , SaturationMode (..)
  , SaturatingNum (..)
  , boundedAdd
  , boundedSub
  , boundedMul
  )
where

-- * Arithmetic functions for arguments and results of different precision

-- | Adding, subtracting, and multiplying values of two different (sub-)types.
class ExtendingNum a b where
  -- | Type of the result of the addition or subtraction
  type AResult a b
  -- | Add values of different (sub-)types, return a value of a (sub-)type
  -- that is potentially different from either argument.
  add  :: a -> b -> AResult a b
  -- | Subtract values of different (sub-)types, return a value of a (sub-)type
  -- that is potentially different from either argument.
  sub :: a -> b -> AResult a b
  -- | Type of the result of the multiplication
  type MResult a b
  -- | Multiply values of different (sub-)types, return a value of a (sub-)type
  -- that is potentially different from either argument.
  mul :: a -> b -> MResult a b

-- * Saturating arithmetic functions

-- | Determine how overflow and underflow are handled by the functions in
-- 'SaturatingNum'
data SaturationMode
  = SatWrap  -- ^ Wrap around on overflow and underflow
  | SatBound -- ^ Become 'maxBound' on overflow, and 'minBound' on underflow
  | SatZero  -- ^ Become @0@ on overflow and underflow
  | SatSymmetric -- ^ Become 'maxBound' on overflow, and (@'minBound' + 1@) on
                 -- underflow for signed numbers, and 'minBound' for unsigned
                 -- numbers.
  deriving (Show, Eq, Enum, Bounded)

-- | 'Num' operators in which overflow and underflow behavior can be specified
-- using 'SaturationMode'.
class (Bounded a, Num a) => SaturatingNum a where
  -- | Addition with parameterizable over- and underflow behavior
  satAdd :: SaturationMode -> a -> a -> a
  -- | Subtraction with parameterizable over- and underflow behavior
  satSub  :: SaturationMode -> a -> a -> a
  -- | Multiplication with parameterizable over- and underflow behavior
  satMul :: SaturationMode -> a -> a -> a
  -- | Get successor of (or in other words, add 1 to) given number
  satSucc :: SaturationMode -> a -> a
  -- Default method suitable for types that can represent the number 1
  satSucc s n = satAdd s n 1
  {-# INLINE satSucc #-}
  -- | Get predecessor of (or in other words, subtract 1 from) given number
  satPred :: SaturationMode -> a -> a
  -- Default method suitable for types that can represent the number 1
  satPred s n = satSub s n 1
  {-# INLINE satPred #-}

-- | Addition that clips to 'maxBound' on overflow, and 'minBound' on underflow
boundedAdd :: SaturatingNum a => a -> a -> a
boundedAdd = satAdd SatBound
{-# INLINE boundedAdd #-}

-- | Subtraction that clips to 'maxBound' on overflow, and 'minBound' on
-- underflow
boundedSub  :: SaturatingNum a => a -> a -> a
boundedSub = satSub SatBound
{-# INLINE boundedSub #-}

-- | Multiplication that clips to 'maxBound' on overflow, and 'minBound' on
-- underflow
boundedMul :: SaturatingNum a => a -> a -> a
boundedMul = satMul SatBound
{-# INLINE boundedMul #-}
