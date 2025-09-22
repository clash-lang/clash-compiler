module Clash.Tests.Laws.Num (tests) where

import Clash.Tests.Laws.SaturatingNum
  ( genBoundedIntegral
  , genUnsigned
  )

import Test.Tasty
import Test.Tasty.Hedgehog.Extra

import Clash.Sized.BitVector (Bit, BitVector)

import Control.DeepSeq (NFData)
import GHC.TypeLits (KnownNat)

import Hedgehog

genBit :: Gen Bit
genBit = genBoundedIntegral

genBitVector :: forall n. KnownNat n => Gen (BitVector n)
genBitVector = genBoundedIntegral

additiveInverse :: (Num a, Show a, Eq a) => Gen a -> TestTree
additiveInverse genA = testPropertyXXX "x + negate x == 0" $ property $ do
  a <- forAll genA
  a + negate a === 0

testAdditiveInverse :: (NFData a, Ord a, Show a, Eq a, Num a) => String -> Gen a -> TestTree
testAdditiveInverse typeName genA =
  testGroup typeName [testGroup "additiveInverse" [additiveInverse genA]]

tests :: TestTree
tests = testGroup "Num"
  [ testAdditiveInverse "Bit" genBit

  , testAdditiveInverse "Unsigned 0" (genUnsigned @0)
  , testAdditiveInverse "Unsigned 1" (genUnsigned @1)
  , testAdditiveInverse "Unsigned 32" (genUnsigned @32)
  , testAdditiveInverse "Unsigned 127" (genUnsigned @127)
  , testAdditiveInverse "Unsigned 128" (genUnsigned @128)

  , testAdditiveInverse "BitVector 0" (genBitVector @0)
  , testAdditiveInverse "BitVector 1" (genBitVector @1)
  , testAdditiveInverse "BitVector 32" (genBitVector @32)
  , testAdditiveInverse "BitVector 127" (genBitVector @127)
  , testAdditiveInverse "BitVector 128" (genBitVector @128)

  -- TODO: Index, Signed, UFixed, SFixed. See discussion in
  --       https://github.com/clash-lang/clash-compiler/issues/3015
  ]
