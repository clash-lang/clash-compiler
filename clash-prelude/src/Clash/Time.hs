{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Clash.Time (
  Time(TimeFS, TimePS, TimeNS, TimeUS, TimeMS, TimeS),
  timeInFS, timeInPS, timeInNS, timeInUS, timeInMS, timeInS,
  clockCycles, clockPeriodTime,
  timeUntil,
  mulTime,
  AtOrForTime(..), absTime
) where

import qualified Data.Foldable
import Data.List (findIndex)
import Data.Maybe (fromMaybe)
import Text.Read

import Clash.Promoted.Nat (snatToNum)
import Clash.Signal (KnownDomain(..), Signal, SDomainConfiguration(..))


newtype Time = TimeFS Integer
  deriving (Eq, Ord)

pattern TimePS :: Integer -> Time
pattern TimePS t <- (getTimePat 3 -> Just t) where
  TimePS t = TimeFS (t * 10^(3::Integer))
pattern TimeNS :: Integer -> Time
pattern TimeNS t <- (getTimePat 6 -> Just t) where
  TimeNS t = TimeFS (t * 10^(6::Integer))
pattern TimeUS :: Integer -> Time
pattern TimeUS t <- (getTimePat 9 -> Just t) where
  TimeUS t = TimeFS (t * 10^(9::Integer))
pattern TimeMS :: Integer -> Time
pattern TimeMS t <- (getTimePat 12 -> Just t) where
  TimeMS t = TimeFS (t * 10^(12::Integer))
pattern TimeS :: Integer -> Time
pattern TimeS t <- (getTimePat 15 -> Just t) where
  TimeS  t = TimeFS (t * 10^(15::Integer))

-- | This is an internal function.
getTimePat :: Int -> Time -> Maybe Integer
getTimePat k (TimeFS t) = if t `rem` (10^k) == 0 then Just (t `div` (10^k)) else Nothing

instance Show Time where
  show (TimeS  t) = show t <> "s"
  show (TimeMS t) = show t <> "ms"
  show (TimeUS t) = show t <> "us"
  show (TimeNS t) = show t <> "ns"
  show (TimePS t) = show t <> "ps"
  show (TimeFS t) = show t <> "fs"

  -- showsPrec d t = showParen (d > app_prec) $ showString (unit <> show value)
  --  where
  --   (unit,value) = split t
  --   split (TimeS  t) = ("TimeS " ,t)
  --   split (TimeMS t) = ("TimeMS ",t)
  --   split (TimeUS t) = ("TimeUS ",t)
  --   split (TimeNS t) = ("TimeNS ",t)
  --   split (TimePS t) = ("TimePS ",t)
  --   split (TimeFS t) = ("TimeFS ",t)
  --   app_prec = 10

instance Read Time where
  readPrec =
    parens
      $   (prec app_prec $ do
              Ident unitCons <- lexP
              t <- step readPrec
              case unitCons of
                "TimeFS" -> return (TimeFS t)
                "TimePS" -> return (TimePS t)
                "TimeNS" -> return (TimeNS t)
                "TimeUS" -> return (TimeUS t)
                "TimeMS" -> return (TimeMS t)
                "TimeS"  -> return (TimeS  t)
                _        -> pfail )

      +++ (prec pfix_prec $ do
              t <- step readPrec
              Ident unit <- lexP
              case unit of
                "fs" -> return (TimeFS t)
                "ps" -> return (TimePS t)
                "ns" -> return (TimeNS t)
                "us" -> return (TimeUS t)
                "ms" -> return (TimeMS t)
                "s"  -> return (TimeS  t)
                _    -> pfail )
   where
    app_prec = 10
    pfix_prec = 11

instance Num Time where
  (+) (TimeFS a) (TimeFS b) = TimeFS (a+b)
  negate (TimeFS a) = TimeFS (-a)
  abs (TimeFS a) = TimeFS (abs a)
  (*) _ _ = error "Time values cannot be multiplied"
  signum _ = error "signum is undefined for Time, because it must be a dimensionless quantity"
  fromInteger 0 = TimeFS 0
  fromInteger _ = error "Non-zero Time values must always be created with a unit"

-- | Class for multiplying time and integers
class MulTime a b where
  mulTime :: a -> b -> Time
infixl 7 `mulTime` -- same as (*)

instance MulTime Time Integer where
  mulTime (TimeFS t) m = TimeFS (t*m)
instance MulTime Integer Time where
  mulTime = flip mulTime

-- | Time in femtoseconds.
timeInFS :: Time -> Integer
timeInFS (TimeFS t) = t
-- | Time in picoseconds, round to nearest, ties away from zero.
timeInPS :: Time -> Integer
timeInPS (TimeFS t) = roundDiv 3 t
-- | Time in nanoseconds, round to nearest, ties away from zero.
timeInNS :: Time -> Integer
timeInNS (TimeFS t) = roundDiv 6 t
-- | Time in microseconds, round to nearest, ties away from zero.
timeInUS :: Time -> Integer
timeInUS (TimeFS t) = roundDiv 9 t
-- | Time in milliseconds, round to nearest, ties away from zero.
timeInMS :: Time -> Integer
timeInMS (TimeFS t) = roundDiv 12 t
-- | Time in seconds, round to nearest, ties away from zero.
timeInS :: Time -> Integer
timeInS (TimeFS t) = roundDiv 15 t

-- | Internal; round integer to it's nearest multiple of 10^k
roundDiv :: Int -> Integer -> Integer
roundDiv k x = (x + (10^k `div` 2)) `div` 10^k

-- | The duration of @n@ clock cycles in domain @dom@.
-- Negative multiples are allowed.
clockCycles ::
  forall dom .
  KnownDomain dom =>
  Integer ->
  Time
clockCycles i = i `mulTime` clockPeriodTime @dom

-- | The period of a clock domain.
clockPeriodTime ::
  forall dom.
  KnownDomain dom =>
  Time
clockPeriodTime =
  case knownDomain @dom of
    SDomainConfiguration{sPeriod} -> TimePS $ snatToNum sPeriod

-- | Used to indicate a specific moment or a duration
data AtOrForTime = At Time | For Time
  deriving (Show)

-- | Compute the 'Time' that elapses from the first clock edge of the simulation
-- to the edge at which the signal satisfies the predicate.
-- Note that the first value of a signal is the initial value before the first clock edge,
-- and is ignored by this function.
timeUntil ::
  forall dom a.
  KnownDomain dom =>
  (a -> Bool) ->
  Signal dom a ->
  Time
timeUntil cond sig = clockCycles @dom
  $ toInteger
  $ fromMaybe undefined
  $ findIndex cond
  $ unsafeTail -- drop reset value
  $ Data.Foldable.foldr (:) [] sig
 where
  unsafeTail (_:t) = t
  unsafeTail [] = error "Empty signal"

-- | Go from an absolute or relative time and starting point to an absolute time.
absTime ::
  Time ->
  AtOrForTime ->
  Time
absTime _ (At t) = t
absTime t (For d) = t + d
