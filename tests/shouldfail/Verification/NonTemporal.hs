{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module NonTemporal where

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
  done = hideAssertion results (counter .==. pure maxBound)
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
  asserted = assertCvResult clk rst enableGen (1 :: Index 2) assertion
  assertion = check clk rst "propName" t (assert (f x y))
  clk = tbSystemClockGen (P.not <$> done)
  rst = resetGen
{-# INLINE binaryTest #-}

-- Tests below are single cycle non-temporal tests testing basic operators such
-- as "&&", "||", "->", ..


-- == && ==
fails1 :: RenderAs -> Signal System Bool
fails1 t = binaryTest t (lit False) (lit True) (|&|)

fails2 :: RenderAs -> Signal System Bool
fails2 t = binaryTest t (lit True) (lit False) (|&|)

fails3 :: RenderAs -> Signal System Bool
fails3 t = binaryTest t (lit False) (lit False) (|&|)

fails4 :: RenderAs -> Signal System Bool
fails4 t = binaryTest t (lit True) (lit True) (\a b -> not (a |&| b))

-- == || ==
fails5 :: RenderAs -> Signal System Bool
fails5 t = binaryTest t (lit False) (lit False) (|||)

fails6 :: RenderAs -> Signal System Bool
fails6 t = binaryTest t (lit True) (lit True) (\a b -> not (a ||| b))

fails7 :: RenderAs -> Signal System Bool
fails7 t = binaryTest t (lit False) (lit True) (\a b -> not (a ||| b))

fails8 :: RenderAs -> Signal System Bool
fails8 t = binaryTest t (lit True) (lit False) (\a b -> not (a ||| b))

-- == ~> ==
fails9 :: RenderAs -> Signal System Bool
fails9 t = binaryTest t (lit True) (lit False) (~>)

-- Not supported by GHDL:
fails10 :: RenderAs -> Signal System Bool
fails10 t = binaryTest t (lit False) (lit False) (\a b -> not (a ~> b))

fails11 :: RenderAs -> Signal System Bool
fails11 t = binaryTest t (lit False) (lit True) (\a b -> not (a ~> b))

fails12 :: RenderAs -> Signal System Bool
fails12 t = binaryTest t (lit True) (lit True) (\a b -> not (a ~> b))

fails13 :: RenderAs -> Signal System Bool
fails13 t = binaryTest t (lit True) (lit False) (\a b -> a ~> b)
