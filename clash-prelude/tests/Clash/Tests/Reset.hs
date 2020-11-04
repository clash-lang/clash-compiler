{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeInType #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Clash.Tests.Reset where

import qualified Prelude as P

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.TH
import Clash.Explicit.Prelude

createDomain vSystem{vName="Low", vResetPolarity=ActiveLow}
createDomain vSystem{vName="NoInit", vInitBehavior=Unknown}

sampleResetN :: KnownDomain dom => Int -> Reset dom -> [Bool]
sampleResetN n = sampleN n . unsafeToHighPolarity

resetFromList :: KnownDomain dom => [Bool] -> Reset dom
resetFromList = unsafeFromHighPolarity . fromList

onePeriodGlitchReset :: KnownDomain dom => Reset dom
onePeriodGlitchReset =
  resetFromList [True,True,False,False,True,False,False,True,True,False,False]

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

-- | Same as 'case_onePeriodGlitch' but on a domain without initial values. This
-- tests whether the 'resetGlitchFilter' can recover from an unknown initial
-- state.
case_onePeriodGlitch_NoInit :: Assertion
case_onePeriodGlitch_NoInit =
      P.drop 2 [True,True,True,True,False,False,False,False,False,True,True,False]
  @=? P.drop 2 (sampleResetN 12 (resetGlitchFilter d2 (clockGen @NoInit) onePeriodGlitchReset))

tests :: TestTree
tests = testGroup "Reset"
  [ $(testGroupGenerator)
  ]

main :: IO ()
main = defaultMain tests
