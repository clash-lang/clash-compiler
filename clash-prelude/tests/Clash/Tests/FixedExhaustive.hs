{-# LANGUAGE NegativeLiterals #-}
{-# LANGUAGE TypeFamilies #-}

{-# OPTIONS_GHC -fplugin=GHC.TypeLits.Extra.Solver #-}
{-# OPTIONS_GHC -fplugin=GHC.TypeLits.Normalise #-}
{-# OPTIONS_GHC -fplugin=GHC.TypeLits.KnownNat.Solver #-}

module Clash.Tests.FixedExhaustive (tests) where

import Data.Proxy (Proxy(..))
import Data.Typeable (typeRep)

import Test.Tasty
import Test.Tasty.HUnit

import Clash.Sized.Fixed (Fixed(..), FracFixedC, SFixed, UFixed)

listsEqual
  :: forall f rep int frac
   . ( FracFixedC rep int frac
     , f ~ Fixed rep int frac
     )
  => String
  -> [f]
  -> [Rational]
  -> Assertion
listsEqual prefix fs0 rs0 = do
  let limit = 1000
      minVal = toRational $ minBound @f
      maxVal = toRational $ maxBound @f
      fs = take limit (map toRational fs0)
      rs = take limit $ takeWhile (\r -> r >= minVal && r <= maxVal) rs0
  assertBool (prefix ++ "length rs > maxLength") (length rs < limit)
  assertBool (prefix ++ show fs ++ "\n/=\n" ++ show rs) (fs == rs)

forAllEnumFrom
  :: forall f rep int frac
   . ( FracFixedC rep int frac
     , f ~ Fixed rep int frac
     )
  => Proxy f
  -> Assertion
forAllEnumFrom Proxy = sequence_
  [ listsEqual ("x1 = Fixed " ++ show (toInteger $ unFixed x1))
               (enumFrom x1)
               (enumFrom (toRational x1))
  | x1 :: f <- map Fixed [minBound..]]

forAllEnumFromThen
  :: forall f rep int frac
   . ( FracFixedC rep int frac
     , f ~ Fixed rep int frac
     )
  => Proxy f
  -> Assertion
forAllEnumFromThen Proxy = sequence_
  [ let fs = enumFromThen x1 x2
        rs = enumFromThen (toRational x1) (toRational x2)
        prefix = unlines [ "x1 = Fixed " ++ show (toInteger $ unFixed x1)
                         , "x2 = Fixed " ++ show (toInteger $ unFixed x2)]
    in if (x1 == x2) then
         listsEqual prefix (take 10 fs) (take 10 rs)
       else
         listsEqual prefix fs rs
  | x1 :: f <- map Fixed [minBound..]
  , x2 <- map Fixed [minBound..]]

forAllEnumFromTo
  :: forall f rep int frac
   . ( FracFixedC rep int frac
     , f ~ Fixed rep int frac
     )
  => Proxy f
  -> Assertion
forAllEnumFromTo Proxy = sequence_
  [ listsEqual (unlines [ "x1 = Fixed " ++ show (toInteger $ unFixed x1)
                        , "y = Fixed " ++ show (toInteger $ unFixed y)])
               (enumFromTo x1 y)
               (enumFromTo (toRational x1) (toRational y))
  | x1 :: f <- map Fixed [minBound..]
  , y <- map Fixed [minBound..]]

forAllEnumFromThenTo
  :: forall f rep int frac
   . ( FracFixedC rep int frac
     , f ~ Fixed rep int frac
     )
  => Proxy f
  -> Assertion
forAllEnumFromThenTo Proxy = sequence_
  [ let fs = enumFromThenTo x1 x2 y
        rs = enumFromThenTo (toRational x1) (toRational x2) (toRational y)
        prefix = unlines [ "x1 = Fixed " ++ show (toInteger $ unFixed x1)
                         , "x2 = Fixed " ++ show (toInteger $ unFixed x2)
                         , "y = Fixed " ++ show (toInteger $ unFixed y)]
    in if (x1 == x2) then
         listsEqual prefix (take 10 fs) (take 10 rs)
       else
         listsEqual prefix fs rs
  | x1 :: f <- map Fixed [minBound..]
  , x2 <- map Fixed [minBound..]
  , y <- map Fixed [minBound..]]

enumTests
  :: forall f rep int frac
   . ( FracFixedC rep int frac
     , f ~ Fixed rep int frac
     )
  => Proxy f
  -> TestTree
enumTests pf =
  testGroup (show $ typeRep pf)
    [ testCase "enumFrom" $ forAllEnumFrom pf
    , testCase "enumFromThen" $ forAllEnumFromThen pf
    , testCase "enumFromTo" $ forAllEnumFromTo pf
    , testCase "enumFromThenTo" $ forAllEnumFromThenTo pf ]

tests :: TestTree
tests =
  testGroup "FixedExhaustive"
    [ enumTests (Proxy @(SFixed 0 0))
    , enumTests (Proxy @(SFixed 0 1))
    , enumTests (Proxy @(SFixed 1 0))
    , enumTests (Proxy @(SFixed 0 2))
    , enumTests (Proxy @(SFixed 1 1))
    , enumTests (Proxy @(SFixed 2 0))
    , enumTests (Proxy @(SFixed 0 3))
    , enumTests (Proxy @(SFixed 1 2))
    , enumTests (Proxy @(SFixed 2 1))
    , enumTests (Proxy @(SFixed 3 0))
    , enumTests (Proxy @(SFixed 0 4))
    , enumTests (Proxy @(SFixed 1 3))
    , enumTests (Proxy @(SFixed 2 2))
    , enumTests (Proxy @(SFixed 3 1))
    , enumTests (Proxy @(SFixed 4 0))
    , enumTests (Proxy @(UFixed 0 0))
    , enumTests (Proxy @(UFixed 0 1))
    , enumTests (Proxy @(UFixed 1 0))
    , enumTests (Proxy @(UFixed 0 2))
    , enumTests (Proxy @(UFixed 1 1))
    , enumTests (Proxy @(UFixed 2 0))
    , enumTests (Proxy @(UFixed 0 3))
    , enumTests (Proxy @(UFixed 1 2))
    , enumTests (Proxy @(UFixed 2 1))
    , enumTests (Proxy @(UFixed 3 0))
    , enumTests (Proxy @(UFixed 0 4))
    , enumTests (Proxy @(UFixed 1 3))
    , enumTests (Proxy @(UFixed 2 2))
    , enumTests (Proxy @(UFixed 3 1))
    , enumTests (Proxy @(UFixed 4 0))
    ]
