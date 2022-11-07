{-|
Copyright  :  (C) 2022,      Google Inc.,
License    :  BSD2 (see the file LICENSE)
Maintainer :  QBayLogic B.V. <devops@qbaylogic.com>

Utilities and definitions to deal with Verilog's time unit. These definitions
are here mostly to deal with varying @`timescale@ defintions, see:

  https://www.chipverify.com/verilog/verilog-timescale

-}

{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeFamilies #-}

module Clash.Backend.Verilog.Time where

import Clash.Class.HasDomain.HasSingleDomain
  (TryDomain, TryDomainResult(NotFound))

import Control.DeepSeq (NFData)
import Data.Char (toLower, isDigit)
import Data.Hashable (Hashable)
import Data.List (find)
import Data.Word (Word64)
import GHC.Generics (Generic)
import Text.Read (readMaybe)

-- | Verilog time units
data Unit = Fs | Ps | Ns | Us | Ms | S
  deriving (Show, Enum, Bounded, Eq, Ord, Generic, Hashable, NFData)

type instance TryDomain t Unit = 'NotFound

-- | Verilog time period. A combination of a length and a unit.
data Period = Period Word64 Unit
  deriving (Show, Generic, Hashable, Eq, NFData)

-- | Verilog timescale. Influences simulation precision.
data Scale = Scale
  { -- | Time step in wait statements, e.g. `#1`.
    step :: Period

    -- | Simulator precision - all units will get rounded to this period.
  , precision :: Period
  }
  deriving (Show, Generic, Hashable, Eq, NFData)

-- | Pretty print 'Scale' to Verilog `timescale
--
-- >>> scaleToString (Scale (Period 100 Ps) (Period 10 Fs))
-- "`timescale 100ps/10fs"
--
scaleToString :: Scale -> String
scaleToString (Scale{step, precision}) =
  "`timescale " <> periodToString step <> "/" <> periodToString precision

-- | Convert 'Unit' to Verilog time unit
--
-- >>> periodToString (Period 100 Fs)
-- "100fs"
--
periodToString :: Period -> String
periodToString (Period len unit) = show len <> unitToString unit

-- | Convert 'Unit' to Verilog time unit
--
-- >>> unitToString Ms
-- "ms"
--
unitToString :: Unit -> String
unitToString = map toLower . show

-- | Parse string representing a Verilog time unit to 'Unit'.
--
-- >>> parseUnit "ms"
-- Just Ms
-- >>> parseUnit "xs"
-- Nothing
--
parseUnit :: String -> Maybe Unit
parseUnit s = find tryUnit [minBound..]
 where
  tryUnit :: Unit -> Bool
  tryUnit u = unitToString u == s

-- | Parse a Verilog
--
-- >>> parsePeriod "100ms"
-- Just (Period 100 Ms)
-- >>> parsePeriod "100xs"
-- Nothing
-- >>> parsePeriod "100"
-- Nothing
-- >>> parsePeriod "ms"
-- Nothing
--
parsePeriod :: String -> Maybe Period
parsePeriod s =
  case span isDigit s of
    (len0, unit0) -> do
      len1 <- readMaybe len0
      unit1 <- parseUnit unit0
      pure (Period len1 unit1)

-- | Convert a period to a specific time unit. Will always output a minimum
-- of 1, even if the given 'Period' is already of the right 'Unit'.
--
-- >>> convertUnit Ps (Period 100 Ps)
-- 100
-- >>> convertUnit Fs (Period 100 Ps)
-- 100000
-- >>> convertUnit Ns (Period 100 Ps)
-- 1
-- >>> convertUnit Ms (Period 0 Ms)
-- 1
--
convertUnit :: Unit -> Period -> Word64
convertUnit targetUnit = go
 where
  go (Period len unit) =
    case compare unit targetUnit of
      LT -> go (Period (len `div` 1000) (succ unit))
      EQ -> max 1 len
      GT -> go (Period (len * 1000) (pred unit))
