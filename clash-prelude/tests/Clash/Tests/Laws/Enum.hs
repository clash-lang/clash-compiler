{-# LANGUAGE RankNTypes #-}

module Clash.Tests.Laws.Enum (tests) where

import Control.DeepSeq (NFData)
import Data.Proxy
import Test.Tasty
import Test.Tasty.HUnit

import Clash.Sized.Index (Index)
import Clash.Sized.Signed (Signed)
import Clash.Sized.Unsigned (Unsigned)

import Test.Tasty.HUnit.Extra

succMaxBoundLaw ::
  forall a .
  (NFData a, Show a, Enum a, Bounded a) =>
  Proxy a ->
  Assertion
succMaxBoundLaw Proxy = expectException (succ @a maxBound)

predMinBoundLaw ::
  forall a .
  (NFData a, Show a, Enum a, Bounded a) =>
  Proxy a ->
  Assertion
predMinBoundLaw Proxy = expectException (pred @a minBound)

enumLaws ::
  (NFData a, Show a, Enum a, Bounded a) =>
  Proxy a ->
  [TestTree]
enumLaws proxy =
  [ testCase "succ maxBound ~ _|_" (succMaxBoundLaw proxy)
  , testCase "pred minBound ~ _|_" (predMinBoundLaw proxy)
  ]

testEnumLaws ::
  (NFData a, Show a, Enum a, Bounded a) =>
  String ->
  Proxy a ->
  TestTree
testEnumLaws typeName proxy = testGroup typeName (enumLaws proxy)

tests :: TestTree
tests = testGroup "Enum"
  [ testEnumLaws "Index 1" (Proxy @(Index 1))
  , testEnumLaws "Index 2" (Proxy @(Index 2))
  , testEnumLaws "Index 128" (Proxy @(Index 128))

  , testEnumLaws "Unsigned 0" (Proxy @(Unsigned 0))
  , testEnumLaws "Unsigned 1" (Proxy @(Unsigned 1))
  , testEnumLaws "Unsigned 32" (Proxy @(Unsigned 32))
  , testEnumLaws "Unsigned 127" (Proxy @(Unsigned 127))
  , testEnumLaws "Unsigned 128" (Proxy @(Unsigned 128))

  , testEnumLaws "Signed 0" (Proxy @(Signed 0))
  , testEnumLaws "Signed 1" (Proxy @(Signed 1))
  , testEnumLaws "Signed 32" (Proxy @(Signed 32))
  , testEnumLaws "Signed 127" (Proxy @(Signed 127))
  , testEnumLaws "Signed 128" (Proxy @(Signed 128))

  -- Note Fixed is tested elsewhere.
  ]
