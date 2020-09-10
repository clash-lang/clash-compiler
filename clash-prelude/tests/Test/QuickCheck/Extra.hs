module Test.QuickCheck.Extra
  ( expectException
  , expectXException
  , expectExceptionNoX
  ) where

import Test.Tasty.QuickCheck
import Control.DeepSeq (NFData)
import Control.Exception (SomeException, try, evaluate)
import Data.Either (isLeft)

import Clash.XException (XException)

-- | Succeed if evaluating leads to an XException
expectXException :: (Show a, NFData a) => a -> Property
expectXException a0 = ioProperty $ do
  a1 <- try @XException (evaluate a0)
  pure $
    counterexample
      ("Expected Exception, got: " <> show a1)
      (isLeft a1)

-- | Succeed if evaluating leads to an Exception
expectException :: (Show a, NFData a) => a -> Property
expectException a0 = ioProperty $ do
  a1 <- try @SomeException (evaluate a0)
  pure $
    counterexample
      ("Expected Exception, got: " <> show a1)
      (isLeft a1)

-- | Succeed if evaluating leads to a non-XException Exception
expectExceptionNoX :: (Show a, NFData a) => a -> Property
expectExceptionNoX a0 = ioProperty $ do
  a1 <- try @SomeException (try @XException (evaluate a0))
  pure $
    counterexample
      ("Expected non-XException Exception, got: " <> show a1)
      (isLeft a1)
