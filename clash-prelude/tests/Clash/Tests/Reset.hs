{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Clash.Tests.Reset where

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.TH
import Clash.Explicit.Prelude

import qualified Prelude as P

type AsyncLow = ("AsyncLow" :: Domain)
createDomain vSystem{vName="AsyncLow", vResetPolarity=ActiveLow}

type SyncHigh = ("SyncHigh" :: Domain)
createDomain vSystem{vName="SyncHigh", vResetKind=Synchronous, vResetPolarity=ActiveHigh}

type SyncLow = ("SyncLow" :: Domain)
createDomain vSystem{vName="SyncLow", vResetKind=Synchronous, vResetPolarity=ActiveLow}

sampleResetN :: KnownDomain dom => Int -> Reset dom -> [Bool]
sampleResetN n = sampleN n . unsafeToActiveHigh

resetFromList :: KnownDomain dom => [Bool] -> Reset dom
resetFromList = unsafeFromActiveHigh . fromList

onePeriodGlitchReset :: KnownDomain dom => Reset dom
onePeriodGlitchReset =
  resetFromList [True,True,False,False,True,False,False,True,True,False,False,False]

-- | Introduce a glitch of one period, and see if it's filtered out
--
-- Note that since 'System' is a domain with asynchronous resets,
-- 'resetGlitchFilter' first synchronizes the incoming reset. This leads to an
-- additional delay of two cycles with respect to the output.
case_onePeriodGlitch :: Assertion
case_onePeriodGlitch =
      [True,True,True,True,True,True,False,False,False,False,False,True,True,False]
  @=? sampleResetN 14 (resetGlitchFilter d2 systemClockGen onePeriodGlitchReset)

-- | Same as 'case_onePeriodGlitch' but on a domain with active low resets
case_onePeriodGlitch_LowPolarity :: Assertion
case_onePeriodGlitch_LowPolarity =
      [True,True,True,True,True,True,False,False,False,False,False,True,True,False]
  @=? sampleResetN 14 (resetGlitchFilter d2 (clockGen @AsyncLow) onePeriodGlitchReset)

-- Check that the meaning of @Reset@ is maintained when converting from
-- active-low to active-high. Both domains have asynchronous resets, so no
-- glitch filter is inserted. The reset is asserted immediately and
-- deasserted after the two cycles of latency of @resetSynchronizer@.
case_convertReset_polarity_change_async :: Assertion
case_convertReset_polarity_change_async =
      P.replicate 3 True <> P.replicate 7 False
  @=? sampleResetN 10 (convertReset (clockGen @AsyncLow) (clockGen @System)
                                    (resetFromList $ True : P.repeat False))

-- Same as 'case_convertReset_polarity_change_async', but with synchronous
-- resets. The reset passes through three flip-flops: a glitch filter in
-- the source domain and two in @resetSynchronizer@. In domains with synchronous
-- resets and defined initial values, @resetSynchronizer@ will start with an
-- asserted reset.
case_convertReset_polarity_change_sync :: Assertion
case_convertReset_polarity_change_sync =
      P.replicate 4 True <> P.replicate 6 False
  @=? sampleResetN 10 (convertReset (clockGen @SyncLow) (clockGen @SyncHigh)
                                    (resetFromList $ True : P.repeat False))

tests :: TestTree
tests = testGroup "Reset"
  [ $(testGroupGenerator)
  ]

main :: IO ()
main = defaultMain tests
