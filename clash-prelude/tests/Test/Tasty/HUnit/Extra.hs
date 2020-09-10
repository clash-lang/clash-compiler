{-# LANGUAGE LambdaCase #-}

module Test.Tasty.HUnit.Extra
  ( expectException
  , expectXException
  , expectExceptionNoX
  ) where

import Control.DeepSeq (NFData)
import Control.Exception (SomeException, try, evaluate)
import Test.Tasty.HUnit

import Clash.XException (XException)

-- | Succeed if evaluating leads to an XException
expectXException :: (Show a, NFData a) => a -> Assertion
expectXException a0 =
  try @XException (evaluate a0) >>= \case
    Left _ -> pure ()
    Right a -> assertFailure ("Expected Exception, got: " <> show a)

-- | Succeed if evaluating leads to an Exception
expectException :: (Show a, NFData a) => a -> Assertion
expectException a0 =
  try @SomeException (evaluate a0) >>= \case
    Left _ -> pure ()
    Right a -> assertFailure ("Expected Exception, got: " <> show a)

-- | Succeed if evaluating leads to a non-XException Exception
expectExceptionNoX :: (Show a, NFData a) => a -> Assertion
expectExceptionNoX a0 =
  try @SomeException (try @XException (evaluate a0)) >>= \case
    Left _ -> pure ()
    Right a -> assertFailure ("Expected Exception, got: " <> show a)
