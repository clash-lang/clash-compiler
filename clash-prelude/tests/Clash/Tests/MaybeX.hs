{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}

module Clash.Tests.MaybeX where

import Clash.XException (errorX)
import Clash.XException.MaybeX (MaybeX(IsX, IsDefined), toMaybeX)

import Control.Applicative (liftA2)
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.TH

data ABC = A | B Int deriving Show

x :: MaybeX a
x = errorX "X"

isX :: MaybeX a -> Bool
isX = \case
  IsDefined {} -> False
  IsX {} -> True

isDefined :: MaybeX a -> Bool
isDefined = not . isX

case_showNoParens :: Assertion
case_showNoParens = show (toMaybeX A) @?= "IsDefined A"

case_showParens :: Assertion
case_showParens = show (toMaybeX (B 0)) @?= "IsDefined (B 0)"

case_pureDefined :: Assertion
case_pureDefined = assertBool "defined value resolves to IsDefined" (isDefined (pure 'a'))

case_pureX :: Assertion
case_pureX = assertBool "pure catches X" (isX (pure x))

case_Fmap :: Assertion
case_Fmap = assertBool "fmap" (isDefined (const () <$> pure 'a'))

case_strictFmap :: Assertion
case_strictFmap = assertBool "fmap is strict in X" (isX (const () <$> pure x))

case_liftA2 :: Assertion
case_liftA2 = assertBool "liftA2"  (isDefined (liftA2 (\_ _ -> ()) (pure 'a') (pure 'b')))

case_strictLiftA2 :: Assertion
case_strictLiftA2 = do
  assertBool "liftA2 is strict in X (left)"  (isX (liftA2 (\_ _ -> ()) (pure x) (pure 'b')))
  assertBool "liftA2 is strict in X (right)" (isX (liftA2 (\_ _ -> ()) (pure 'a') (pure x)))
  assertBool "liftA2 is strict in X (both)"  (isX (liftA2 (\_ _ -> ()) (pure x) (pure x)))

tests :: TestTree
tests = $(testGroupGenerator)
