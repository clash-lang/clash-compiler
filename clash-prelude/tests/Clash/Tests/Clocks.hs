{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Clash.Tests.Clocks(tests) where

import qualified Prelude as P

import Test.Tasty
import Test.Tasty.HUnit

import Clash.Explicit.Prelude
import Clash.Intel.ClockGen (altpll)

-- Ratio of clock periods in 'createDomain' and 'resetLen' are chosen, rest is
-- derived from that

createDomain vSystem{vName="ClocksSlow", vPeriod=3 * vPeriod vSystem}

resetLen :: SNat 10
resetLen = SNat

lockResampled :: Assertion
lockResampled =
  unlockedLenSeen @?= unlockedLen
 where
  pll ::
    Clock ClocksSlow ->
    Reset ClocksSlow ->
    (Clock System, Signal System Bool)
  pll = altpll (SSymbol @"pll")

  unlockedLenSeen =
    P.length . P.takeWhile not .
    -- Arbitrary cut-off so simulation always ends
    sampleN (unlockedLen + 100) .
    snd $ pll clockGen (resetGenN resetLen)

clockRatio :: Int
clockRatio = fromIntegral $ snatToNatural (clockPeriod @ClocksSlow) `div`
                            snatToNatural (clockPeriod @System)

unlockedLen :: Int
unlockedLen = snatToNum resetLen * clockRatio - clockRatio + 1

tests :: TestTree
tests =
  testGroup "Clocks class"
    [ testCase "Lock is resampled from reset" lockResampled ]
