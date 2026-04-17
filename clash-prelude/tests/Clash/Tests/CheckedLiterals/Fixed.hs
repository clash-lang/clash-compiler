{-|
Copyright  :  (C) 2026, QBayLogic B.V.
License    :  BSD2 (see the file LICENSE)
Maintainer :  QBayLogic B.V. <devops@qbaylogic.com>
-}

module Clash.Tests.CheckedLiterals.Fixed (tests) where

import Prelude

import Test.Tasty (TestTree, testGroup)
import Clash.Tests.CheckedLiterals.Common (toTestCases)

fMod :: String
fMod = "Clash.Sized.Fixed"

integerTests :: TestTree
integerTests = testGroup "Integer" $ toTestCases
  [ (fMod, "(KnownNat f) => UFixed 0 f", "0",                      [])
  , (fMod, "(KnownNat f) => UFixed 0 f", "-1",                     ["Literal -1 is out of bounds, because UFixed cannot represent negative numbers."])
  , (fMod, "(KnownNat f) => UFixed 0 f", "1",                      ["Literal 1 is (potentially) out of bounds.", "Note: integer part needs at least 1 bit(s)."])
  , (fMod, "(KnownNat f) => UFixed 1 f", "0",                      [])
  , (fMod, "(KnownNat f) => UFixed 1 f", "-1",                     ["Literal -1 is out of bounds, because UFixed cannot represent negative numbers."])
  , (fMod, "(KnownNat f) => UFixed 1 f", "1",                      [])
  , (fMod, "(KnownNat f) => UFixed 1 f", "2",                      ["Literal 2 is (potentially) out of bounds.", "Note: integer part needs at least 2 bit(s)."])
  , (fMod, "(KnownNat f) => UFixed 2 f", "0",                      [])
  , (fMod, "(KnownNat f) => UFixed 2 f", "-1",                     ["Literal -1 is out of bounds, because UFixed cannot represent negative numbers."])
  , (fMod, "(KnownNat f) => UFixed 2 f", "1",                      [])
  , (fMod, "(KnownNat f) => UFixed 2 f", "2",                      [])
  , (fMod, "(KnownNat f) => UFixed 2 f", "3",                      [])
  , (fMod, "(KnownNat f) => UFixed 2 f", "4",                      ["Literal 4 is (potentially) out of bounds.", "Note: integer part needs at least 3 bit(s)."])
  , (fMod, "(KnownNat f, KnownNat n) => UFixed n f", "0",          [])
  , (fMod, "(KnownNat f, KnownNat n) => UFixed n f", "1",          ["Literal 1 is (potentially) out of bounds.", "Note: integer part needs at least 1 bit(s).", "Possible fix: add a constraint: 1 <= n."])
  , (fMod, "(KnownNat f, KnownNat n) => UFixed n f", "-1",         ["Literal -1 is out of bounds, because UFixed cannot represent negative numbers."])
  , (fMod, "(KnownNat f, KnownNat n, 1 <= n) => UFixed n f", "0",  [])
  , (fMod, "(KnownNat f, KnownNat n, 1 <= n) => UFixed n f", "1",  [])
  , (fMod, "(KnownNat f, KnownNat n, 1 <= n) => UFixed n f", "-1", ["Literal -1 is out of bounds, because UFixed cannot represent negative numbers."])
  , (fMod, "(KnownNat f, KnownNat n, 2 <= n) => UFixed n f", "0",  [])
  , (fMod, "(KnownNat f, KnownNat n, 2 <= n) => UFixed n f", "1",  [])
  , (fMod, "(KnownNat f, KnownNat n, 2 <= n) => UFixed n f", "-1", ["Literal -1 is out of bounds, because UFixed cannot represent negative numbers."])

  , (fMod, "(KnownNat f) => SFixed 0 f", "0",                      [])
  , (fMod, "(KnownNat f) => SFixed 0 f", "-1",                     ["Literal -1 is (potentially) out of bounds.", "Note: integer part needs at least 1 bit(s), including sign bit."])
  , (fMod, "(KnownNat f) => SFixed 0 f", "1",                      ["Literal 1 is (potentially) out of bounds.", "Note: integer part needs at least 2 bit(s), including sign bit."])
  , (fMod, "(KnownNat f) => SFixed 1 f", "0",                      [])
  , (fMod, "(KnownNat f) => SFixed 1 f", "-1",                     [])
  , (fMod, "(KnownNat f) => SFixed 1 f", "1",                      ["Literal 1 is (potentially) out of bounds.", "Note: integer part needs at least 2 bit(s), including sign bit."])
  , (fMod, "(KnownNat f) => SFixed 1 f", "2",                      ["Literal 2 is (potentially) out of bounds.", "Note: integer part needs at least 3 bit(s), including sign bit."])
  , (fMod, "(KnownNat f) => SFixed 2 f", "0",                      [])
  , (fMod, "(KnownNat f) => SFixed 2 f", "-1",                     [])
  , (fMod, "(KnownNat f) => SFixed 2 f", "-2",                     [])
  , (fMod, "(KnownNat f) => SFixed 2 f", "-3",                     ["Literal -3 is (potentially) out of bounds.", "Note: integer part needs at least 3 bit(s), including sign bit."])
  , (fMod, "(KnownNat f) => SFixed 2 f", "1",                      [])
  , (fMod, "(KnownNat f) => SFixed 2 f", "2",                      ["Literal 2 is (potentially) out of bounds.", "Note: integer part needs at least 3 bit(s), including sign bit."])
  , (fMod, "(KnownNat f) => SFixed 2 f", "4",                      ["Literal 4 is (potentially) out of bounds.", "Note: integer part needs at least 4 bit(s), including sign bit."])
  , (fMod, "(KnownNat f, KnownNat n) => SFixed n f", "0",          [])
  , (fMod, "(KnownNat f, KnownNat n) => SFixed n f", "1",          ["Literal 1 is (potentially) out of bounds.", "Note: integer part needs at least 2 bit(s), including sign bit.", "Possible fix: add a constraint: 2 <= n."])
  , (fMod, "(KnownNat f, KnownNat n) => SFixed n f", "-1",         ["Literal -1 is (potentially) out of bounds.", "Note: integer part needs at least 1 bit(s), including sign bit.", "Possible fix: add a constraint: 1 <= n."])
  , (fMod, "(KnownNat f, KnownNat n, 1 <= n) => SFixed n f", "0",  [])
  , (fMod, "(KnownNat f, KnownNat n, 1 <= n) => SFixed n f", "1",  ["Literal 1 is (potentially) out of bounds.", "Note: integer part needs at least 2 bit(s), including sign bit.", "Possible fix: add a constraint: 2 <= n."])
  , (fMod, "(KnownNat f, KnownNat n, 1 <= n) => SFixed n f", "-1", [])
  , (fMod, "(KnownNat f, KnownNat n, 2 <= n) => SFixed n f", "0",  [])
  , (fMod, "(KnownNat f, KnownNat n, 2 <= n) => SFixed n f", "1",  [])
  , (fMod, "(KnownNat f, KnownNat n, 2 <= n) => SFixed n f", "-1", [])
  ]

rationalTests :: TestTree
rationalTests = testGroup "Rational" $ toTestCases
  [ (fMod, "(KnownNat f) => UFixed 0 f", "0.0",                              [])
  , (fMod, "(KnownNat f) => UFixed 0 f", "-1.0",                             ["Literal -1.0 is out of bounds, because UFixed cannot represent negative numbers."])
  , (fMod, "(KnownNat f) => UFixed 0 f", "1.0",                              ["Literal 1.0 is (potentially) out of bounds.", "Note: integer part needs at least 1 bit(s)."])
  , (fMod, "(KnownNat f) => UFixed 1 f", "0.0",                              [])
  , (fMod, "(KnownNat f) => UFixed 1 f", "-1.0",                             ["Literal -1.0 is out of bounds, because UFixed cannot represent negative numbers."])
  , (fMod, "(KnownNat f) => UFixed 1 f", "1.0",                              [])
  , (fMod, "(KnownNat f) => UFixed 1 f", "2.0",                              ["Literal 2.0 is (potentially) out of bounds.", "Note: integer part needs at least 2 bit(s)."])
  , (fMod, "(KnownNat f) => UFixed 2 f", "0.0",                              [])
  , (fMod, "(KnownNat f) => UFixed 2 f", "-1.0",                             ["Literal -1.0 is out of bounds, because UFixed cannot represent negative numbers."])
  , (fMod, "(KnownNat f) => UFixed 2 f", "1.0",                              [])
  , (fMod, "(KnownNat f) => UFixed 2 f", "2.0",                              [])
  , (fMod, "(KnownNat f) => UFixed 2 f", "3.0",                              [])
  , (fMod, "(KnownNat f) => UFixed 2 f", "4.0",                              ["Literal 4.0 is (potentially) out of bounds.", "Note: integer part needs at least 3 bit(s)."])
  , (fMod, "(KnownNat f, KnownNat n) => UFixed n f", "0.0",                  [])
  , (fMod, "(KnownNat f, KnownNat n) => UFixed n f", "1.0",                  ["Literal 1.0 is (potentially) out of bounds.", "Note: integer part needs at least 1 bit(s).", "Possible fix: add a constraint: 1 <= n."])
  , (fMod, "(KnownNat f, KnownNat n) => UFixed n f", "-1.0",                 ["Literal -1.0 is out of bounds, because UFixed cannot represent negative numbers."])
  , (fMod, "(KnownNat f, KnownNat n, 1 <= n) => UFixed n f", "0.0",          [])
  , (fMod, "(KnownNat f, KnownNat n, 1 <= n) => UFixed n f", "1.0",          [])
  , (fMod, "(KnownNat f, KnownNat n, 1 <= n) => UFixed n f", "-1.0",         ["Literal -1.0 is out of bounds, because UFixed cannot represent negative numbers."])
  , (fMod, "(KnownNat f, KnownNat n, 2 <= n) => UFixed n f", "0.0",          [])
  , (fMod, "(KnownNat f, KnownNat n, 2 <= n) => UFixed n f", "1.0",          [])
  , (fMod, "(KnownNat f, KnownNat n, 2 <= n) => UFixed n f", "-1.0",         ["Literal -1.0 is out of bounds, because UFixed cannot represent negative numbers."])

  , (fMod, "(KnownNat f) => SFixed 0 f", "0.0",                              [])
  , (fMod, "(KnownNat f) => SFixed 0 f", "-1.0",                             ["Literal -1.0 is (potentially) out of bounds.", "Note: integer part needs at least 1 bit(s), including sign bit."])
  , (fMod, "(KnownNat f) => SFixed 0 f", "1.0",                              ["Literal 1.0 is (potentially) out of bounds.", "Note: integer part needs at least 2 bit(s), including sign bit."])
  , (fMod, "(KnownNat f) => SFixed 1 f", "0.0",                              [])
  , (fMod, "(KnownNat f) => SFixed 1 f", "-1.0",                             [])
  , (fMod, "(KnownNat f) => SFixed 1 f", "1.0",                              ["Literal 1.0 is (potentially) out of bounds.", "Note: integer part needs at least 2 bit(s), including sign bit."])
  , (fMod, "(KnownNat f) => SFixed 1 f", "2.0",                              ["Literal 2.0 is (potentially) out of bounds.", "Note: integer part needs at least 3 bit(s), including sign bit."])
  , (fMod, "(KnownNat f) => SFixed 2 f", "0.0",                              [])
  , (fMod, "(KnownNat f) => SFixed 2 f", "-1.0",                             [])
  , (fMod, "(KnownNat f) => SFixed 2 f", "-2.0",                             [])
  , (fMod, "(KnownNat f) => SFixed 2 f", "-3.0",                             ["Literal -3.0 is (potentially) out of bounds.", "Note: integer part needs at least 3 bit(s), including sign bit."])
  , (fMod, "(KnownNat f) => SFixed 2 f", "1.0",                              [])
  , (fMod, "(KnownNat f) => SFixed 2 f", "2.0",                              ["Literal 2.0 is (potentially) out of bounds.", "Note: integer part needs at least 3 bit(s), including sign bit."])
  , (fMod, "(KnownNat f) => SFixed 2 f", "4.0",                              ["Literal 4.0 is (potentially) out of bounds.", "Note: integer part needs at least 4 bit(s), including sign bit."])
  , (fMod, "(KnownNat f, KnownNat n) => SFixed n f", "0.0",                  [])
  , (fMod, "(KnownNat f, KnownNat n) => SFixed n f", "1.0",                  ["Literal 1.0 is (potentially) out of bounds.", "Note: integer part needs at least 2 bit(s), including sign bit.", "Possible fix: add a constraint: 2 <= n."])
  , (fMod, "(KnownNat f, KnownNat n) => SFixed n f", "-1.0",                 ["Literal -1.0 is (potentially) out of bounds.", "Note: integer part needs at least 1 bit(s), including sign bit.", "Possible fix: add a constraint: 1 <= n."])
  , (fMod, "(KnownNat f, KnownNat n, 1 <= n) => SFixed n f", "0.0",          [])
  , (fMod, "(KnownNat f, KnownNat n, 1 <= n) => SFixed n f", "1.0",          ["Literal 1.0 is (potentially) out of bounds.", "Note: integer part needs at least 2 bit(s), including sign bit.", "Possible fix: add a constraint: 2 <= n."])
  , (fMod, "(KnownNat f, KnownNat n, 1 <= n) => SFixed n f", "-1.0",         [])
  , (fMod, "(KnownNat f, KnownNat n, 2 <= n) => SFixed n f", "0.0",          [])
  , (fMod, "(KnownNat f, KnownNat n, 2 <= n) => SFixed n f", "1.0",          [])
  , (fMod, "(KnownNat f, KnownNat n, 2 <= n) => SFixed n f", "-1.0",         [])

  , (fMod, "UFixed 0 0", "0.5",                                              ["Literal 0.5 cannot be represented exactly by Fixed", "The fractional part needs at least 1 bit(s)."])
  , (fMod, "UFixed 0 1", "0.5",                                              [])
  , (fMod, "UFixed 0 1", "0.75",                                             ["Literal 0.75 cannot be represented exactly by Fixed", "The fractional part needs at least 2 bit(s)."])
  , (fMod, "UFixed 0 1", "0.1",                                              ["Literal 0.1 cannot be represented exactly by", "The reduced denominator 10 is not a power of 2."])
  , (fMod, "(KnownNat f, 1 <= f) => UFixed 0 f", "0.5",                      [])
  , (fMod, "(KnownNat f, 1 <= f) => UFixed 0 f", "0.75",                     ["Literal 0.75 cannot be represented exactly by Fixed", "The fractional part needs at least 2 bit(s).", "Possible fix: add a constraint: 2 <= f."])
  , (fMod, "(KnownNat f, 2 <= f) => UFixed 0 f", "0.75",                     [])
  , (fMod, "(KnownNat f, 3 <= f) => UFixed 0 f", "0.75",                     [])

  , (fMod, "SFixed 0 0", "0.5",                                              ["Literal 0.5 cannot be represented exactly by Fixed", "The fractional part needs at least 1 bit(s)."])
  , (fMod, "(KnownNat f, KnownNat n, 1 <= n, 1 <= f) => SFixed n f", "0.5",  [])
  , (fMod, "(KnownNat f, KnownNat n, 1 <= n, 1 <= f) => SFixed n f", "0.75", ["Literal 0.75 cannot be represented exactly by Fixed", "The fractional part needs at least 2 bit(s).", "Possible fix: add a constraint: 2 <= f."])
  , (fMod, "(KnownNat f, KnownNat n, 1 <= n, 2 <= f) => SFixed n f", "0.75", [])
  , (fMod, "(KnownNat f, KnownNat n, 1 <= n, 3 <= f) => SFixed n f", "0.75", [])

  , (fMod, "(KnownNat f, KnownNat n) => UFixed n f", "255.0",                ["Literal 255.0 is (potentially) out of bounds.", "Note: integer part needs at least 8 bit(s).", "Possible fix: add a constraint: 8 <= n."])
  , (fMod, "(KnownNat f, KnownNat n) => UFixed n f", "256.0",                ["Literal 256.0 is (potentially) out of bounds.", "Note: integer part needs at least 9 bit(s).", "Possible fix: add a constraint: 9 <= n."])
  ]

tests :: TestTree
tests = testGroup "Fixed" [integerTests, rationalTests]
