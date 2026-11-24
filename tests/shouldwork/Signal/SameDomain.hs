{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE UndecidableInstances #-}

module SameDomain where

import Clash.Annotations.SynthesisAttributes (Annotate, Attr(..))
import Clash.Explicit.Prelude
import Clash.Explicit.Testbench
import Clash.Signal (sameDomain)
import Data.Kind (Type)
import Type.Reflection (Typeable)
import Data.Type.Equality ((:~:)(Refl))

createDomain vSystem{vName="OtherSystem"}

type SystemAlias = System

newtype WrappedSystem = WrappedSystem System

data family FamilyTag (a :: Type)
data instance FamilyTag Bool
type instance ClockDomainOf (FamilyTag Bool) = System
instance KnownDomain (FamilyTag Bool)

data Tagged (a :: Type)
type instance ClockDomainOf (Tagged a) = System
instance Typeable a => KnownDomain (Tagged a)

checkDomains
  :: forall domA domB
   . (KnownDomain domA, KnownDomain domB)
  => Bool
  -> Bool
checkDomains input = case sameDomain @domA @domB of
  Just Refl -> input
  Nothing -> not input
{-# OPAQUE checkDomains #-}

-- Return the equality witness across a component boundary.
witness :: Bool -> Maybe (System :~: System)
witness x = if x then sameDomain @System @System else Nothing
{-# OPAQUE witness #-}

-- A clock dictionary can supply a value to an opaque component.
periodValue :: forall dom. KnownDomain dom => Bool -> Unsigned 16
periodValue x = if x then snatToNum (clockPeriod @dom) else 7
{-# OPAQUE periodValue #-}

topEntity :: Bool -> Vec 12 Bool
topEntity input =
     checkDomains @System @System input
  :> not (checkDomains @System @OtherSystem input)
  :> not (checkDomains @OtherSystem @System input)
  :> checkDomains @System @SystemAlias input
  :> not (checkDomains @(Tagged System) @(Tagged OtherSystem) input)
  :> not (checkDomains @(Tagged System) @(Tagged WrappedSystem) input)
  :> not (checkDomains @(Tagged (Int %1 -> Int)) @(Tagged (Int -> Int)) input)
  :> checkDomains @(Tagged (Int `Annotate` 'BoolAttr "keep" 'True)) @(Tagged Int) input
  :> not (checkDomains @(Tagged (FamilyTag Int)) @(Tagged (FamilyTag Bool)) input)
  :> checkDomains @(FamilyTag Bool) @(FamilyTag Bool) input
  :> (case witness input of
        Just Refl -> True
        Nothing -> False)
  :> (periodValue @System input == 10000)
  :> Nil
{-# OPAQUE topEntity #-}

testBench :: Signal System Bool
testBench = done
 where
  input = stimuliGenerator clk rst (False :> True :> Nil)
  expected = repeat False :> repeat True :> Nil
  done = outputVerifier' clk rst expected (topEntity <$> input)
  clk = tbSystemClockGen (not <$> done)
  rst = systemResetGen
{-# OPAQUE testBench #-}
