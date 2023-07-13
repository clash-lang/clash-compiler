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

-- Testing with explicit declaration of the Low type alias
type Low = ("Low" :: Domain)
createDomain vSystem{vName="Low", vResetPolarity=ActiveLow}

createDomain vSystem{vName="NoInit", vInitBehavior=Unknown}

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

tests :: TestTree
tests = testGroup "Reset"
  [ $(testGroupGenerator)
  ]

main :: IO ()
main = defaultMain tests
