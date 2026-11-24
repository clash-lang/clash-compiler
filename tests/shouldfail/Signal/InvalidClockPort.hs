{-# LANGUAGE UndecidableInstances #-}

module InvalidClockPort where

import Clash.Explicit.Prelude
import Clash.Signal.Internal (KnownFraction)
import GHC.Real (Ratio((:%)))

data Physical (whole :: Nat) (frac :: Nat)
data Virtual (whole :: Nat) (frac :: Nat)

type instance ClockDomainOf (Virtual whole frac) = Physical whole frac
type instance ClockDomainOf (Physical whole frac) = Project whole frac
type family Project whole frac where
  Project 2 frac = System
  Project whole frac = Physical whole frac

instance
  ( KnownNat whole, KnownNat frac, KnownNat (whole - 1)
  , KnownFraction (frac :% 2), Project whole frac ~ Physical whole frac
  ) => KnownClockDomain (Physical whole frac) where
  type ClockPeriod (Physical whole frac) = whole
  type ClockPeriodFraction (Physical whole frac) = frac :% 2
  type ClockActiveEdge (Physical whole frac) = 'Rising
  type ClockResetKind (Physical whole frac) = 'Asynchronous
  type ClockInitBehavior (Physical whole frac) = 'Defined
  type ClockResetPolarity (Physical whole frac) = 'ActiveHigh

-- A bare port does not ask GHC to solve the physical instance's context.
improperFraction :: Clock (Physical 1 3) -> Bool -> Bool
improperFraction _ = not

indirectProjection :: Clock (Virtual 2 0) -> Bool -> Bool
indirectProjection _ = not

data Populated = Populated
type instance ClockDomainOf Populated = System
instance KnownDomain Populated

populatedMarker :: Clock Populated -> Bool -> Bool
populatedMarker _ = not
