{-|
Copyright  :  (C) 2026, QBayLogic B.V.
License    :  BSD2 (see the file LICENSE)
Maintainer :  QBayLogic B.V. <devops@qbaylogic.com>

Smoke tests for the passthrough @CheckedLiteral@ instances on the numeric
wrapper newtypes: they should forward the check to the underlying type —
good literals type-check, bad ones surface the underlying type's error.
-}

module Clash.Tests.CheckedLiterals.Wrappers (tests) where

import Prelude

import Test.Tasty (TestTree, testGroup)
import Clash.Tests.CheckedLiterals.Common (toTestCases)

wrapU, satS, ovU, zerS, errUF :: String
wrapU = "Clash.Num.Wrapping, Clash.Sized.Unsigned"
satS  = "Clash.Num.Saturating, Clash.Sized.Signed"
ovU   = "Clash.Num.Overflowing, Clash.Sized.Unsigned"
zerS  = "Clash.Num.Zeroing, Clash.Sized.Signed"
errUF = "Clash.Num.Erroring, Clash.Sized.Fixed"

tests :: TestTree
tests = testGroup "Wrappers" $ toTestCases
  [ (wrapU, "Wrapping (Unsigned 2)",   "3",    [])
  , (wrapU, "Wrapping (Unsigned 2)",   "4",    ["Literal 4 is (potentially) out of bounds.", "Unsigned 2 has bounds: [0 .. 3]."])
  , (wrapU, "Wrapping (Unsigned 2)",   "-1",   ["Literal -1 is out of bounds.", "Unsigned 2 has bounds: [0 .. 3]."])

  , (satS,  "Saturating (Signed 2)",   "-2",   [])
  , (satS,  "Saturating (Signed 2)",   "2",    ["Signed 2 has bounds: [-2 .. 1]"])

  , (ovU,   "Overflowing (Unsigned 2)", "3",   [])
  , (ovU,   "Overflowing (Unsigned 2)", "4",   ["Literal 4 is (potentially) out of bounds.", "Unsigned 2 has bounds: [0 .. 3]."])

  , (zerS,  "Zeroing (Signed 2)",      "-1",   [])
  , (zerS,  "Zeroing (Signed 2)",      "2",    ["Signed 2 has bounds: [-2 .. 1]"])

  , (errUF, "Erroring (UFixed 1 2)",   "0.75", [])
  , (errUF, "Erroring (UFixed 1 2)",   "-1.0", ["Literal -1.0 is out of bounds, because UFixed cannot represent negative numbers."])
  , (errUF, "Erroring (UFixed 1 2)",   "0.1",  ["Literal 0.1 cannot be represented exactly by", "The reduced denominator 10 is not a power of 2."])
  ]
