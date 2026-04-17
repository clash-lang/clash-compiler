{-|
Copyright  :  (C) 2026, QBayLogic B.V.
License    :  BSD2 (see the file LICENSE)
Maintainer :  QBayLogic B.V. <devops@qbaylogic.com>

The type argument of 'Index' is the exclusive upper bound, not a bit width,
so @Index 4@ has bounds @[0 .. 3]@ (the same range as @Unsigned 2@). The
polymorphic bounds message therefore reads @Index n has bounds: [0 .. n - 1]@
rather than @[0 .. (2 ^ n) - 1]@.
-}

module Clash.Tests.CheckedLiterals.Index (tests) where

import Prelude

import Test.Tasty (TestTree, testGroup)
import Clash.Tests.CheckedLiterals.Common (toTestCases)

iMod :: String
iMod = "Clash.Sized.Index"

tests :: TestTree
tests = testGroup "Index" $ toTestCases
  [ (iMod, "Index 1", "0",                            [])
  , (iMod, "Index 1", "-1",                           ["Literal -1 is out of bounds.", "Index 1 has bounds: [0 .. 0]."])
  , (iMod, "Index 1", "1",                            ["Literal 1 is (potentially) out of bounds.", "Index 1 has bounds: [0 .. 0]."])
  , (iMod, "Index 2", "0",                            [])
  , (iMod, "Index 2", "-1",                           ["Literal -1 is out of bounds.", "Index 2 has bounds: [0 .. 1]."])
  , (iMod, "Index 2", "1",                            [])
  , (iMod, "Index 2", "2",                            ["Literal 2 is (potentially) out of bounds.", "Index 2 has bounds: [0 .. 1]."])
  , (iMod, "Index 4", "0",                            [])
  , (iMod, "Index 4", "-1",                           ["Literal -1 is out of bounds.", "Index 4 has bounds: [0 .. 3]."])
  , (iMod, "Index 4", "1",                            [])
  , (iMod, "Index 4", "3",                            [])
  , (iMod, "Index 4", "4",                            ["Literal 4 is (potentially) out of bounds.", "Index 4 has bounds: [0 .. 3]."])
  , (iMod, "(KnownNat n) => Index n", "0",            ["Literal 0 is (potentially) out of bounds.", "Index n has bounds: [0 .. n - 1].", "Possible fix: add a constraint: 1 <= n."])
  , (iMod, "(KnownNat n) => Index n", "1",            ["Literal 1 is (potentially) out of bounds.", "Index n has bounds: [0 .. n - 1]", "Possible fix: add a constraint: 2 <= n."])
  , (iMod, "(KnownNat n) => Index n", "-1",           ["Literal -1 is out of bounds.", "Index n has bounds: [0 .. n - 1]."])
  , (iMod, "(KnownNat n, 1 <= n) => Index n", "0",    [])
  , (iMod, "(KnownNat n, 2 <= n) => Index n", "1",    [])
  , (iMod, "(KnownNat n, 1 <= n) => Index n", "-1",   ["Literal -1 is out of bounds.", "Index n has bounds: [0 .. n - 1]."])
  , (iMod, "(KnownNat n, 3 <= n) => Index n", "2",    [])
  , (iMod, "(KnownNat n, 3 <= n) => Index n", "3",    ["Literal 3 is (potentially) out of bounds.", "Index n has bounds: [0 .. n - 1].", "Possible fix: add a constraint: 4 <= n."])
  ]
