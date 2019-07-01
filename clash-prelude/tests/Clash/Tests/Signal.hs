{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Clash.Tests.Signal where

import Test.Tasty
import Test.Tasty.HUnit
import Clash.Signal


tests :: TestTree
tests =
  testGroup
    "Signal"
    [ testGroup
        "Implicit"
        [ -- See: https://github.com/clash-lang/clash-compiler/pull/655
          let rst0 = fromList [True, True, False, False, True, True]
              rst1 = unsafeFromHighPolarity rst0
              reg  = register 'a' (pure 'b')
              sig = withReset0 rst1 reg
          in  testCase "withReset0" (sampleN @System 6 sig @?= "aaabaa")
        ]
    ]
