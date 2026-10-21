{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE GADTs #-}

module SameDomain where

import Clash.Explicit.Prelude
import Clash.Explicit.Testbench
import Clash.Signal (sameDomain)
import Data.Type.Equality ((:~:)(Refl))

createDomain vSystem{vName="OtherSystem"}

type SystemAlias = System

newtype WrappedSystem = WrappedSystem System
createDomain vSystem{vName="WrappedSystem"}

type FunDom = Int -> Int
instance KnownDomain FunDom where
  type DomainPeriod        FunDom = 10000
  type DomainActiveEdge    FunDom = 'Rising
  type DomainResetKind     FunDom = 'Asynchronous
  type DomainInitBehavior  FunDom = 'Defined
  type DomainResetPolarity FunDom = 'ActiveHigh

type AppDom = Int -> Int -> String
instance KnownDomain AppDom where
  type DomainPeriod        AppDom = 10000
  type DomainActiveEdge    AppDom = 'Rising
  type DomainResetKind     AppDom = 'Asynchronous
  type DomainInitBehavior  AppDom = 'Defined
  type DomainResetPolarity AppDom = 'ActiveHigh

checkDomains
  :: forall domA domB
   . (KnownDomain domA, KnownDomain domB)
  => Bool
  -> Bool
checkDomains input = case sameDomain @domA @domB of
  Just Refl -> input
  Nothing -> not input

topEntity :: Bool -> Vec 7 Bool
topEntity input =
     checkDomains @System @System input
  :> checkDomains @System @OtherSystem input
  :> checkDomains @OtherSystem @System input
  :> checkDomains @System @SystemAlias input
  :> checkDomains @System @WrappedSystem input
  :> checkDomains @System @FunDom input
  :> checkDomains @FunDom @AppDom input
  :> Nil
{-# OPAQUE topEntity #-}

testBench :: Signal System Bool
testBench = done
 where
  input = stimuliGenerator clk rst (False :> True :> Nil)
  expected =
       (False :> True :> True :> False :> True :> True :> True :> Nil)
    :> (True :> False :> False :> True :> False :> False :> False :> Nil)
    :> Nil
  done = outputVerifier' clk rst expected (topEntity <$> input)
  clk = tbSystemClockGen (not <$> done)
  rst = systemResetGen
{-# OPAQUE testBench #-}
