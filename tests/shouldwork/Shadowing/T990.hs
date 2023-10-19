{-# LANGUAGE Arrows #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE RankNTypes #-}

module T990 (topEntity, testBench) where

import qualified Control.Category as CC
import Control.Arrow

import Clash.Prelude
import Clash.Class.HasDomain
import Clash.Explicit.Testbench

data SignalA dom a b = SA { runSA :: Signal dom a -> Signal dom b }

instance CC.Category (SignalA dom) where
    id = SA id
    SA f . SA g = SA (f . g)

instance Arrow (SignalA dom) where
    arr f = SA $ fmap f
    first (SA f) = SA $ unbundle >>> f *** id >>> bundle
    second (SA f) = SA $ unbundle >>> id *** f >>> bundle
    (SA f) *** (SA g) = SA $ unbundle >>> f *** g >>> bundle
    (SA f) &&& (SA g) = SA $ f &&& g >>> bundle


alwaysEnable :: (WithSingleDomain dom a) => (HiddenEnable dom => a) -> a
alwaysEnable a = exposeEnable a enableGen

alwaysDelayA ::
    (HiddenClock dom, NFDataX a) =>
    a -> SignalA dom a a
alwaysDelayA a = SA $ alwaysEnable $ delay a

risingEdgeA :: HiddenClock dom => SignalA dom Bool Bool
risingEdgeA = proc x -> do
    x' <- alwaysDelayA False -< x
    returnA -< x && not x'

topEntity
    :: Clock System
    -> Signal System Bool
    -> Signal System Bool
topEntity = exposeClock $ runSA risingEdgeA
-- See: https://github.com/clash-lang/clash-compiler/pull/2511
{-# CLASH_OPAQUE topEntity #-}

testBench :: Signal System Bool
testBench = done
  where
    testInput      = stimuliGenerator clk rst (False:>False:>True:>True:>Nil)
    expectedOutput = outputVerifier'  clk rst (False:>False:>True:>False:>Nil)
    done           = expectedOutput (topEntity clk testInput)
    clk            = tbSystemClockGen (not <$> done)
    rst            = systemResetGen
