
module Clash.Time (
  Time(TimeFS),
  TimePS, TimeNS, TimeUS, TimeMS, TimeS,
  timeInFS, timeInPs, timeInNS, timeInUS, timeInMS, timeInS,
  clockCycles, clockPeriod,
  timeUntil,
  AtOrForTime(..),
) where

newtype Time = TimeFS Integer
  deriving (Eq, Ord)

pattern TimePS t <- (getTimePat 3 -> Just t) where
  TimePS t = TimeFS (t * 10^3)
pattern TimeNS t <- (getTimePat 6 -> Just t) where
  TimeNS t = TimeFS (t * 10^6)
pattern TimeUS t <- (getTimePat 9 -> Just t) where
  TimeUS t = TimeFS (t * 10^9)
pattern TimeMS t <- (getTimePat 12 -> Just t) where
  TimeMS t = TimeFS (t * 10^12)
pattern TimeS t <- (getTimePat 15 -> Just t) where
  TimeS  t = TimeFS (t * 10^15)

-- | This is an internal function
getTimePat k t = if t % (10^k) == 0 then Just (t `div` (10^k)) else Nothing

instance Show Time where
  -- show (TimeS  t) = show t <> "s"
  -- show (TimeMS t) = show t <> "ms"
  -- show (TimeUS t) = show t <> "us"
  -- show (TimeNS t) = show t <> "ns"
  -- show (TimePS t) = show t <> "ps"
  -- show (TimeFS t) = show t <> "fs"

  showsPrec d t = showParen (d > app_prec) $ showString (unit <> show value)
   where
    (unit,value) = split t
    split (TimeS  t) = ("TimeS " ,t)
    split (TimeMS t) = ("TimeMS ",t)
    split (TimeUS t) = ("TimeUS ",t)
    split (TimeNS t) = ("TimeNS ",t)
    split (TimePS t) = ("TimePS ",t)
    split (TimeFS t) = ("TimeFS ",t)
    app_prec = 10

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
clockCycles i = TimeFS $ i * t
 where
  TimeFS t = domainPeriod @dom

-- | The period of a clock domain.
clockPeriod ::
  forall dom.
  KnownDomain dom =>
  Time
clockPeriod =
  case knownDomain @dom of
    SDomainConfiguration{sPeriod} -> TimePS $ snatToNum sPeriod

-- | Used to indicate a specific moment or a duration
data AtOrForTime = At Time | For Time

-- | Compute the 'Time' from the first clock edge at which a signal satisfies a predicate.
-- Note that the first value of a signal is the reset value before the first clock edge,
-- and is ignored by this function.
timeUntil ::
  forall dom a.
  KnownDomain dom =>
  (a -> Bool) ->
  Signal dom a ->
  Time
timeUntil cond sig = clockCycles
  $ toInteger
  $ fromMaybe undefined
  $ findIndex cond
  $ tail -- drop reset value
  $ toList sig
