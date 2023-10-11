{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module Temporal where

import           Data.Coerce

import qualified Prelude                         as P

import           Clash.Explicit.Prelude          hiding (assert, (&&), (||), not)
import           Clash.Explicit.Testbench        hiding (assert, (&&), (||))
import qualified Clash.Explicit.Verification     as V
import           Clash.Explicit.Verification
import           Clash.Verification.DSL
import           Clash.Verification.Internal
import           Clash.XException                (hwSeqX)

assertCvResult
  :: forall n dom
   . (Bounded n, Enum n, Eq n, NFDataX n)
  => Clock dom -> Reset dom -> Enable dom
  -> n
  -> Signal dom AssertionResult
  -> Signal dom Bool
assertCvResult clk rst gen max results = done
 where
  counter = register clk rst gen (minBound :: n) (succ <$> counter)
  done = (\(cvRes, c) -> cvRes `hwSeqX` c == max) <$> bundle (results, counter)
{-# INLINE assertCvResult #-}

binaryTest
  :: ( dom ~ System
     , AssertionValue dom x
     , AssertionValue dom y
     )
  => RenderAs
  -> x
  -> y
  -> (forall a b. (AssertionValue dom a, AssertionValue dom b) => a -> b -> Assertion dom)
  -> Signal dom Bool
binaryTest t x y f = done
 where
  done = outputVerifier' clk rst (False :> True :> Nil) asserted
  asserted = assertCvResult clk rst enableGen (0 :: Index 3) assertion
  assertion = check clk rst "propName" t (assert (f x y))
  clk = tbSystemClockGen (P.not <$> done)
  rst = resetGen
{-# INLINE binaryTest #-}

-- Tests below are single cycle non-temporal tests testing basic operators such
-- as "&&", "||", "->", ..


-- == |-> ==
fails1 :: Signal System Bool
fails1 = binaryTest PSL (lit True) (fromList [True, False]) (|->)
{-# ANN fails1 (defSyn "fails1") #-}
