{-# LANGUAGE RankNTypes #-}

module Clash.Tests.Laws.Enum (tests) where

import Control.DeepSeq (NFData)
import Data.Proxy
import Test.Tasty
import Test.Tasty.HUnit

import Clash.Sized.Index (Index)
import Clash.Sized.Signed (Signed)
import Clash.Sized.Fixed (SFixed, UFixed)
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

  -- TODO: SFixed and UFixed are partial for
  --
  --         succ (maxBound-1, maxBound]
  --         pred [minBound, minBound+1)
  --
  -- Tests only test minBound/maxBound though.
  --
  , testEnumLaws "SFixed 0 0" (Proxy @(SFixed 0 0))
  , testEnumLaws "SFixed 0 1" (Proxy @(SFixed 0 1))
  , testEnumLaws "SFixed 1 0" (Proxy @(SFixed 1 0))
  , testEnumLaws "SFixed 1 1" (Proxy @(SFixed 1 1))
  , testEnumLaws "SFixed 1 2" (Proxy @(SFixed 1 2))
  , testEnumLaws "SFixed 2 1" (Proxy @(SFixed 2 1))
  , testEnumLaws "SFixed 2 2" (Proxy @(SFixed 2 2))
  , testEnumLaws "SFixed 128 128" (Proxy @(SFixed 128 128))

  , testEnumLaws "UFixed 0 0" (Proxy @(UFixed 0 0))
  , testEnumLaws "UFixed 0 1" (Proxy @(UFixed 0 1))
  , testEnumLaws "UFixed 1 0" (Proxy @(UFixed 1 0))
  , testEnumLaws "UFixed 1 1" (Proxy @(UFixed 1 1))
  , testEnumLaws "UFixed 1 2" (Proxy @(UFixed 1 2))
  , testEnumLaws "UFixed 2 1" (Proxy @(UFixed 2 1))
  , testEnumLaws "UFixed 2 2" (Proxy @(UFixed 2 2))
  , testEnumLaws "UFixed 128 128" (Proxy @(UFixed 128 128))
  ]
