{-# LANGUAGE UndecidableInstances #-}

module InvalidClockPort where

import Clash.Explicit.Prelude

data Physical (whole :: Nat)
data Virtual (whole :: Nat)

type instance ClockDomainOf (Virtual whole) = Physical whole
type instance ClockDomainOf (Physical whole) = Project whole
type family Project whole where
  Project 2 = System
  Project whole = Physical whole

instance
  ( KnownNat whole, KnownNat (whole - 1)
  , Project whole ~ Physical whole
  ) => KnownClockDomain (Physical whole) where
  type ClockPeriod (Physical whole) = whole
  type ClockActiveEdge (Physical whole) = 'Rising
  type ClockResetKind (Physical whole) = 'Asynchronous
  type ClockInitBehavior (Physical whole) = 'Defined
  type ClockResetPolarity (Physical whole) = 'ActiveHigh

-- A bare port does not ask GHC to solve the physical instance's context.
indirectProjection :: Clock (Virtual 2) -> Bool -> Bool
indirectProjection _ = not

data Populated = Populated
type instance ClockDomainOf Populated = System
instance KnownDomain Populated

populatedMarker :: Clock Populated -> Bool -> Bool
populatedMarker _ = not
