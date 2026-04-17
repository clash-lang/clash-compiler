{-|
Copyright  :  (C) 2026, QBayLogic B.V.
License    :  BSD2 (see the file LICENSE)
Maintainer :  QBayLogic B.V. <devops@qbaylogic.com>
-}

module Clash.Tests.CheckedLiterals.Signed (tests) where

import Prelude

import Test.Tasty (TestTree, testGroup)
import Clash.Tests.CheckedLiterals.Common (toTestCases)

sMod :: String
sMod = "Clash.Sized.Signed"

tests :: TestTree
tests = testGroup "Signed" $ toTestCases
  [ (sMod, "Signed 0", "0",                          [])
  , (sMod, "Signed 0", "-1",                         ["Literal -1 is (potentially) out of bounds."])
  , (sMod, "Signed 0", "1",                          ["Literal 1 is (potentially) out of bounds."])
  , (sMod, "Signed 1", "0",                          [])
  , (sMod, "Signed 1", "-1",                         [])
  , (sMod, "Signed 1", "1",                          ["Literal 1 is (potentially) out of bounds.", "Signed 1 has bounds: [-1 .. 0]"])
  , (sMod, "Signed 2", "0",                          [])
  , (sMod, "Signed 2", "-1",                         [])
  , (sMod, "Signed 2", "-2",                         [])
  , (sMod, "Signed 2", "-3",                         ["Literal -3 is (potentially) out of bounds.", "Signed 2 has bounds: [-2 .. 1]"])
  , (sMod, "Signed 2", "1",                          [])
  , (sMod, "Signed 2", "2",                          ["Signed 2 has bounds: [-2 .. 1]"])
  , (sMod, "(KnownNat n) => Signed n", "0",          [])
  , (sMod, "(KnownNat n) => Signed n", "1",          ["Signed n has bounds: [-2 ^ (n - 1) .. (2 ^ (n - 1)) - 1]", "Possible fix: add a constraint: 2 <= n."])
  , (sMod, "(KnownNat n) => Signed n", "-1",         ["Literal -1 is (potentially) out of bounds.", "Signed n has bounds: [-2 ^ (n - 1) .. (2 ^ (n - 1)) - 1]", "Possible fix: add a constraint: 1 <= n."])
  , (sMod, "(KnownNat n, 1 <= n) => Signed n", "0",  [])
  , (sMod, "(KnownNat n, 1 <= n) => Signed n", "1",  ["Signed n has bounds: [-2 ^ (n - 1) .. (2 ^ (n - 1)) - 1]", "Possible fix: add a constraint: 2 <= n."])
  , (sMod, "(KnownNat n, 1 <= n) => Signed n", "-1", [])
  , (sMod, "(KnownNat n, 2 <= n) => Signed n", "0",  [])
  , (sMod, "(KnownNat n, 2 <= n) => Signed n", "1",  [])
  , (sMod, "(KnownNat n, 2 <= n) => Signed n", "-1", [])
  , (sMod, "(KnownNat n, 3 <= n) => Signed n", "0",  [])
  , (sMod, "(KnownNat n, 3 <= n) => Signed n", "1",  [])
  , (sMod, "(KnownNat n, 3 <= n) => Signed n", "-1", [])
  ]
