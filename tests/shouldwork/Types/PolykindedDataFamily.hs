{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE PolyKinds #-}

module PolykindedDataFamily where

import Clash.Prelude
import Clash.Explicit.Testbench
import Data.Kind (Type)

data family Tagged (label :: k) :: Type
data instance Tagged (label :: Bool)
newtype instance Tagged (label :: Nat) = NatTagged (Unsigned 8)

type family Width (a :: Type) :: Nat
type instance Width (Tagged label) = 8

type family WidthOf (f :: k -> Type) :: Nat
type instance WidthOf (Tagged @k) = 8

-- Keep the helper polymorphic until Clash specializes it. The Width equation
-- must still match the data-family argument after specialization (#3548).
bump
  :: forall a
   . KnownNat (Width a)
  => Unsigned (Width a)
  -> Unsigned (Width a)
bump x = x + 1
{-# OPAQUE bump #-}

-- The representation must also match before its label argument is supplied.
bumpHigher
  :: forall (f :: Bool -> Type)
   . KnownNat (WidthOf f)
  => Unsigned (WidthOf f)
  -> Unsigned (WidthOf f)
bumpHigher x = x + 1
{-# OPAQUE bumpHigher #-}

topEntity :: Unsigned 8 -> (Unsigned 8, Unsigned 8, Unsigned 8)
topEntity x =
  ( bump @(Tagged 'True) x
  , bump @(Tagged 0) x
  , bumpHigher @(Tagged @Bool) x
  )

testBench :: Signal System Bool
testBench = done
 where
  input = stimuliGenerator clk rst (0 :> 1 :> 41 :> 255 :> Nil)
  expected = (1,1,1) :> (2,2,2) :> (42,42,42) :> (0,0,0) :> Nil
  done = outputVerifier' clk rst expected (topEntity <$> input)
  clk = tbSystemClockGen (not <$> done)
  rst = systemResetGen
