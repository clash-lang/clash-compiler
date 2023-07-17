{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Clash.Tests.Reset where

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.TH
import Clash.Explicit.Prelude

import qualified Prelude as P

-- Testing with explicit declaration of the Low type alias
type Low = ("Low" :: Domain)
createDomain vSystem{vName="Low", vResetPolarity=ActiveLow}

sampleResetN :: KnownDomain dom => Int -> Reset dom -> [Bool]
sampleResetN n = sampleN n . unsafeToActiveHigh

resetFromList :: KnownDomain dom => [Bool] -> Reset dom
resetFromList = unsafeFromActiveHigh . fromList

onePeriodGlitchReset :: KnownDomain dom => Reset dom
onePeriodGlitchReset =
  resetFromList [True,True,False,False,True,False,False,True,True,False,False,False]

-- | Introduce a glitch of one period, and see if it's filtered out
case_onePeriodGlitch :: Assertion
case_onePeriodGlitch =
      [True,True,True,True,False,False,False,False,False,True,True,False]
  @=? sampleResetN 12 (resetGlitchFilter d2 systemClockGen onePeriodGlitchReset)

-- | Same as 'case_onePeriodGlitch' but on a domain with active low resets
case_onePeriodGlitch_LowPolarity :: Assertion
case_onePeriodGlitch_LowPolarity =
      [True,True,True,True,False,False,False,False,False,True,True,False]
  @=? sampleResetN 12 (resetGlitchFilter d2 (clockGen @Low) onePeriodGlitchReset)

-- Check that the meaning of @Reset@ is maintained when converting from
-- active-low to active-high.
case_convertReset_polarity_change :: Assertion
case_convertReset_polarity_change =
      -- In domains with synchronous resets and defined initial values,
      -- @resetSynchronizer@ will start with an asserted reset.
      True : True : P.replicate 8 False
  @=? sampleResetN 10 (convertReset (clockGen @Low) (clockGen @System)
                                    (resetFromList $ P.repeat False))

tests :: TestTree
tests = testGroup "Reset"
  [ $(testGroupGenerator)
  ]

main :: IO ()
main = defaultMain tests
