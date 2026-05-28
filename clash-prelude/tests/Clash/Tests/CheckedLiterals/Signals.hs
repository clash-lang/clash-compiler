{-|
Copyright  :  (C) 2026, QBayLogic B.V.
License    :  BSD2 (see the file LICENSE)
Maintainer :  QBayLogic B.V. <devops@qbaylogic.com>

Smoke tests for the passthrough @CheckedLiteral@ instances on 'Signal' and
'DSignal': they forward the check to the signal's element type.

The @toTestCases@ helper only injects one module import, so we rely on
'System' being re-exported from 'Clash.Signal' and 'Clash.Signal.Delayed'.
-}

module Clash.Tests.CheckedLiterals.Signals (tests) where

import Prelude

import Test.Tasty (TestTree, testGroup)
import Clash.Tests.CheckedLiterals.Common (toTestCases)

pMod :: String
pMod = "Clash.Prelude"

signalTests :: TestTree
signalTests = testGroup "Signal" $ toTestCases
  [ (pMod, "Signal System (Unsigned 2)", "3",  [])
  , (pMod, "Signal System (Unsigned 2)", "4",  ["Literal 4 is (potentially) out of bounds.", "Unsigned 2 has bounds: [0 .. 3]."])
  , (pMod, "Signal System (Signed 2)",   "-2", [])
  , (pMod, "Signal System (Signed 2)",   "2",  ["Signed 2 has bounds: [-2 .. 1]"])
  , (pMod, "Signal System (UFixed 1 2)", "0.75", [])
  , (pMod, "Signal System (UFixed 1 2)", "-1.0", ["Literal -1.0 is out of bounds, because UFixed cannot represent negative numbers."])
  ]

dsignalTests :: TestTree
dsignalTests = testGroup "DSignal" $ toTestCases
  [ (pMod, "DSignal System 0 (Unsigned 2)", "3", [])
  , (pMod, "DSignal System 0 (Unsigned 2)", "4", ["Literal 4 is (potentially) out of bounds.", "Unsigned 2 has bounds: [0 .. 3]."])
  , (pMod, "DSignal System 0 (Signed 2)", "-2",  [])
  , (pMod, "DSignal System 0 (Signed 2)", "2",   ["Signed 2 has bounds: [-2 .. 1]"])
  ]

tests :: TestTree
tests = testGroup "Signals" [signalTests, dsignalTests]
