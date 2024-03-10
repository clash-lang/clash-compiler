{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Clash.Class.Counter.Internal where

import Clash.CPP (maxTupleSize)

import Clash.Class.Counter.TH (genTupleInstances)
import Clash.Sized.BitVector (BitVector, Bit)
import Clash.Sized.Index (Index)
import Clash.Sized.Signed (Signed)
import Clash.Sized.Unsigned (Unsigned)

import Data.Bifunctor (bimap)
import Data.Functor.Identity (Identity(..))
import Data.Int (Int8, Int16, Int32, Int64)
import Data.Word (Word8, Word16, Word32, Word64)
import GHC.TypeLits (KnownNat, type (<=))

-- $setup
-- >>> import Clash.Class.Counter
-- >>> import Clash.Sized.BitVector (BitVector)
-- >>> import Clash.Sized.Index (Index)
-- >>> import Clash.Sized.Signed (Signed)
-- >>> import Clash.Sized.Unsigned (Unsigned)

-- | t'Clash.Class.Counter.Counter' is a class that composes multiple counters
-- into a single one. It is similar to odometers found in olds cars,
-- once all counters reach their maximum they reset to zero - i.e. odometer
-- rollover. See 'Clash.Class.Counter.countSucc' and 'Clash.Class.Counter.countPred'
-- for API usage examples.
--
-- Example use case: when driving a monitor through VGA you would like to keep
-- track at least two counters: one counting a horizontal position, and one
-- vertical. Perhaps a fancy VGA driver would also like to keep track of the
-- number of drawn frames. To do so, the three counters are setup with different
-- types. On each /round/ of the horizontal counter the vertical counter should
-- be increased. On each /round/ of the vertical counter the frame counter should
-- be increased. With this class you could simply use the type:
--
-- @
-- (FrameCount, VerticalCount, HorizontalCount)
-- @
--
-- and have 'Clash.Class.Counter.countSucc' work as described.
--
-- __NB__: This class exposes four functions 'countMin', 'countMax',
-- 'countSuccOverflow', and 'countPredOverflow'. These functions are considered
-- an internal API. Users are encouraged to use 'Clash.Class.Counter.countSucc'
-- and 'Clash.Class.Counter.countPred'.
--
class Counter a where
  -- | Value counter wraps around to on a 'countSuccOverflow' overflow
  countMin :: a
  default countMin :: Bounded a => a
  countMin = minBound

  -- | Value counter wraps around to on a 'countPredOverflow' overflow
  countMax :: a
  default countMax :: Bounded a => a
  countMax = maxBound

  -- | Gets the successor of @a@. If it overflows, the first part of the tuple
  -- will be set to True and the second part wraps around to `countMin`.
  countSuccOverflow :: a -> (Bool, a)
  default countSuccOverflow :: (Eq a, Enum a, Bounded a) => a -> (Bool, a)
  countSuccOverflow a
    | a == maxBound = (True, countMin)
    | otherwise = (False, succ a)

  -- | Gets the predecessor of @a@. If it underflows, the first part of the tuple
  -- will be set to True and the second part wraps around to `countMax`.
  countPredOverflow :: a -> (Bool, a)
  default countPredOverflow :: (Eq a, Enum a, Bounded a) => a -> (Bool, a)
  countPredOverflow a
    | a == minBound = (True, countMax)
    | otherwise = (False, pred a)

instance (1 <= n, KnownNat n) => Counter (Index n)
instance KnownNat n => Counter (Unsigned n)
instance KnownNat n => Counter (Signed n)
instance KnownNat n => Counter (BitVector n)

-- | @since 1.8.2
instance Counter Bool
-- | @since 1.8.2
instance Counter Bit
-- | @since 1.8.2
instance Counter Int
-- | @since 1.8.2
instance Counter Int8
-- | @since 1.8.2
instance Counter Int16
-- | @since 1.8.2
instance Counter Int32
-- | @since 1.8.2
instance Counter Int64
-- | @since 1.8.2
instance Counter Word
-- | @since 1.8.2
instance Counter Word8
-- | @since 1.8.2
instance Counter Word16
-- | @since 1.8.2
instance Counter Word32
-- | @since 1.8.2
instance Counter Word64

-- | @since 1.8.2
deriving newtype instance Counter a => Counter (Identity a)

-- | 'Nothing' is considered the minimum value, while @'Just' 'countMax'@ is
-- considered the maximum value.
--
-- @since 1.8.2
instance Counter a => Counter (Maybe a) where
  countMin = Nothing
  countMax = Just countMax

  countSuccOverflow = \case
    Nothing -> (False, Just countMin)
    Just a0 ->
      case countSuccOverflow a0 of
        (True, _) -> (True, Nothing)
        (False, a1) -> (False, Just a1)

  countPredOverflow = \case
    Nothing -> (True, Just countMax)
    Just a0 ->
      case countPredOverflow a0 of
        (True, _) -> (False, Nothing)
        (False, a1) -> (False, Just a1)

-- | Counter instance that flip-flops between 'Left' and 'Right'. Examples:
--
-- >>> type T = Either (Index 2) (Unsigned 2)
-- >>> countSucc @T (Left 0)
-- Left 1
-- >>> countSucc @T (Left 1)
-- Right 0
-- >>> countSucc @T (Right 0)
-- Right 1
instance (Counter a, Counter b) => Counter (Either a b) where
  countMin = Left countMin
  countMax = Right countMax

  countSuccOverflow e =
    case bimap countSuccOverflow countSuccOverflow e of
      Left (overflow, a)  -> (False, if overflow then Right countMin else Left a)
      Right (overflow, b) -> (overflow, if overflow then Left countMin else Right b)

  countPredOverflow e =
    case bimap countPredOverflow countPredOverflow e of
      Left (overflow, a)  -> (overflow, if overflow then Right countMax else Left a)
      Right (overflow, b) -> (False, if overflow then Left countMax else Right b)

-- | Counters on tuples increment from right-to-left. This makes sense from the
-- perspective of LSB/MSB; MSB is on the left-hand-side and LSB is on the
-- right-hand-side in other Clash types.
--
-- >>> type T = (Unsigned 2, Index 2, Index 2)
-- >>> countSucc @T (0, 0, 0)
-- (0,0,1)
-- >>> countSucc @T (0, 0, 1)
-- (0,1,0)
-- >>> countSucc @T (0, 1, 0)
-- (0,1,1)
-- >>> countSucc @T (0, 1, 1)
-- (1,0,0)
--
-- __NB__: The documentation only shows the instances up to /3/-tuples. By
-- default, instances up to and including /12/-tuples will exist. If the flag
-- @large-tuples@ is set instances up to the GHC imposed limit will exist. The
-- GHC imposed limit is either 62 or 64 depending on the GHC version.
instance (Counter a0, Counter a1) => Counter (a0, a1) where
  -- a0/a1 instead of a/b to be consistent with TH generated instances
  countMin = (countMin, countMin)
  countMax = (countMax, countMax)

  countSuccOverflow (a0, b0) =
    if overflowB
    then (overflowA, (a1, b1))
    else (overflowB, (a0, b1))
   where
    (overflowB, b1) = countSuccOverflow b0
    (overflowA, a1) = countSuccOverflow a0

  countPredOverflow (a0, b0) =
    if overflowB
    then (overflowA, (a1, b1))
    else (overflowB, (a0, b1))
   where
    (overflowB, b1) = countPredOverflow b0
    (overflowA, a1) = countPredOverflow a0

genTupleInstances maxTupleSize
