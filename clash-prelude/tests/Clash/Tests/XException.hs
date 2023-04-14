{-# LANGUAGE TemplateHaskell #-}

module Clash.Tests.XException where

import Clash.XException

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.HUnit.Extra
import Test.Tasty.TH

expectLeft :: HasCallStack => Either a b -> Assertion
expectLeft (Left _) = pure ()
expectLeft (Right _) = assertFailure "Expected Left, got Right"

expectRight :: HasCallStack => Either a b -> Assertion
expectRight (Right _) = pure ()
expectRight (Left _) = assertFailure "Expected Right, got Left"

case_hasX :: Assertion
case_hasX = do
  expectRight        $ hasX @(Int, Int) (1, 2)
  expectLeft         $ hasX @(Int, Int) (x, 2)
  expectLeft         $ hasX @(Int, Int) (1, x)
  expectLeft         $ hasX @(Int, Int) (x, x)
  expectLeft         $ hasX @(Int, Int) x
  expectExceptionNoX $ hasX @(Int, Int) (e, 2)
  expectExceptionNoX $ hasX @(Int, Int) (1, e)
  expectExceptionNoX $ hasX @(Int, Int) (e, e)
  expectExceptionNoX $ hasX @(Int, Int) (x, e)
  expectExceptionNoX $ hasX @(Int, Int) (e, x)
  expectExceptionNoX $ hasX @(Int, Int) e
 where
  x = errorX "X"
  e = error "E"

tests :: TestTree
tests = $(testGroupGenerator)

-- Run with:
--
--    ./repld p:tests -T Clash.Tests.XException.main
--
-- Add -W if you want to run tests in spite of warnings
--
main :: IO ()
main = defaultMain tests
