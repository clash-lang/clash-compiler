{-|
Copyright  :  (C) 2026, QBayLogic B.V.
License    :  BSD2 (see the file LICENSE)
Maintainer :  QBayLogic B.V. <devops@qbaylogic.com>
-}

module Clash.Tests.CheckedLiterals.BitVector (tests) where

import Prelude

import Test.Tasty (TestTree, testGroup)
import Clash.Tests.CheckedLiterals.Common (toTestCases)

bvMod :: String
bvMod = "Clash.Sized.BitVector"

bitTests :: TestTree
bitTests = testGroup "Bit" $ toTestCases
  [ (bvMod, "Bit", "0",  [])
  , (bvMod, "Bit", "1",  [])
  , (bvMod, "Bit", "-1", ["Literal -1 is out of bounds.", "Bit has bounds: [0 .. 1]."])
  , (bvMod, "Bit", "2",  ["Literal 2 is out of bounds.", "Bit has bounds: [0 .. 1]."])
  ]

bitVectorTests :: TestTree
bitVectorTests = testGroup "BitVector" $ toTestCases
  [ (bvMod, "BitVector 0", "0",                            [])
  , (bvMod, "BitVector 0", "-1",                           ["Literal -1 is out of bounds.", "BitVector 0 has bounds: [0 .. 0]."])
  , (bvMod, "BitVector 0", "1",                            ["Literal 1 is (potentially) out of bounds.", "BitVector 0 has bounds: [0 .. 0]."])
  , (bvMod, "BitVector 1", "0",                            [])
  , (bvMod, "BitVector 1", "-1",                           ["Literal -1 is out of bounds.", "BitVector 1 has bounds: [0 .. 1]."])
  , (bvMod, "BitVector 1", "1",                            [])
  , (bvMod, "BitVector 1", "2",                            ["Literal 2 is (potentially) out of bounds.", "BitVector 1 has bounds: [0 .. 1]."])
  , (bvMod, "BitVector 2", "0",                            [])
  , (bvMod, "BitVector 2", "-1",                           ["Literal -1 is out of bounds.", "BitVector 2 has bounds: [0 .. 3]."])
  , (bvMod, "BitVector 2", "1",                            [])
  , (bvMod, "BitVector 2", "2",                            [])
  , (bvMod, "BitVector 2", "3",                            [])
  , (bvMod, "BitVector 2", "4",                            ["Literal 4 is (potentially) out of bounds.", "BitVector 2 has bounds: [0 .. 3]."])
  , (bvMod, "(KnownNat n) => BitVector n", "0",            [])
  , (bvMod, "(KnownNat n) => BitVector n", "1",            ["Literal 1 is (potentially) out of bounds.", "BitVector n has bounds: [0 .. (2 ^ n) - 1]", "Possible fix: add a constraint: 1 <= n."])
  , (bvMod, "(KnownNat n) => BitVector n", "-1",           ["Literal -1 is out of bounds.", "BitVector n has bounds: [0 .. (2 ^ n) - 1]."])
  , (bvMod, "(KnownNat n, 1 <= n) => BitVector n", "0",    [])
  , (bvMod, "(KnownNat n, 1 <= n) => BitVector n", "1",    [])
  , (bvMod, "(KnownNat n, 1 <= n) => BitVector n", "-1",   ["Literal -1 is out of bounds.", "BitVector n has bounds: [0 .. (2 ^ n) - 1]."])
  , (bvMod, "(KnownNat n, 2 <= n) => BitVector n", "0",    [])
  , (bvMod, "(KnownNat n, 2 <= n) => BitVector n", "1",    [])
  , (bvMod, "(KnownNat n, 2 <= n) => BitVector n", "-1",   ["Literal -1 is out of bounds.", "BitVector n has bounds: [0 .. (2 ^ n) - 1]."])
  , (bvMod, "(KnownNat n, 7 <= n) => BitVector n", "255",  ["Literal 255 is (potentially) out of bounds.", "BitVector n has bounds: [0 .. (2 ^ n) - 1].", "Possible fix: add a constraint: 8 <= n."])
  , (bvMod, "(KnownNat n, 8 <= n) => BitVector n", "256",  ["Literal 256 is (potentially) out of bounds.", "BitVector n has bounds: [0 .. (2 ^ n) - 1].", "Possible fix: add a constraint: 9 <= n."])
  ]

tests :: TestTree
tests = testGroup "BitVector" [bitTests, bitVectorTests]
