module InvalidClockPort where

import Clash.Explicit.Prelude

data Populated = Populated
instance KnownDomain Populated where
  type ClockDomainOf Populated = System

populatedMarker :: Clock Populated -> Bool -> Bool
populatedMarker _ = not
