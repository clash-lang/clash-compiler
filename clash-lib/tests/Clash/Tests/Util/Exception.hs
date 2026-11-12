{-# LANGUAGE CPP #-}
{-# LANGUAGE TypeApplications #-}

module Clash.Tests.Util.Exception (tests) where

import Control.Concurrent (newEmptyMVar, putMVar, takeMVar)
import qualified Control.Exception as Exception
#if MIN_VERSION_base(4,20,0)
import Control.Exception.Annotation (ExceptionAnnotation(..))
import Data.List (isInfixOf, isPrefixOf)
import GHC.Stack (HasCallStack)
#endif
import System.Exit (ExitCode(..), exitWith)
import System.IO.Error (ioeGetErrorString)
import System.Timeout (timeout)
import Test.Tasty
import Test.Tasty.HUnit (assertBool, assertFailure, testCase, (@?=))

import Clash.Util.Exception

tests :: TestTree
tests = testGroup "Clash.Tests.Util.Exception"
  [ testCase "worker failures reach typed IOException handlers" $ do
      result <- Exception.try @Exception.IOException $
        mapConcurrentlyWithBacktrace_ (\() -> Exception.throwIO (userError "worker")) [(), ()]
      case result of
        Left e -> ioeGetErrorString e @?= "worker"
        Right () -> assertFailure "Expected an IOException"

  , testCase "worker failures reach typed ErrorCall handlers" $ do
      result <- Exception.try @Exception.ErrorCall $
        mapConcurrentlyWithBacktrace_ (\() -> Exception.evaluate (error "worker" :: ())) [(), ()]
      case result of
        Left (Exception.ErrorCall msg) -> msg @?= "worker"
        Right () -> assertFailure "Expected an ErrorCall"

  , testCase "nested concurrent actions preserve exception types" $ do
      mapConcurrentlyWithBacktrace_ (\() -> do
        result <- Exception.try @Exception.IOException $
          mapConcurrentlyWithBacktrace_ (\() -> Exception.throwIO (userError "nested worker")) [()]
        case result of
          Left e -> ioeGetErrorString e @?= "nested worker"
          Right () -> assertFailure "Expected an IOException") [(), ()]

  , testCase "ExitCode passes through" $ do
      result <- Exception.try @ExitCode $
        mapConcurrentlyWithBacktrace_ (\() -> exitWith (ExitFailure 17)) [()]
      result @?= Left (ExitFailure 17)

  , testCase "asynchronous exceptions pass through" $ do
      result <- Exception.try @Exception.AsyncException $
        mapConcurrentlyWithBacktrace_ (\() -> Exception.throwIO Exception.ThreadKilled) [()]
      result @?= Left Exception.ThreadKilled

  , testCase "a worker failure cancels its sibling" $ do
      started <- newEmptyMVar
      cancelled <- newEmptyMVar
      never <- newEmptyMVar
      let worker False = takeMVar started >> Exception.throwIO (userError "worker")
          worker True = Exception.finally
            (putMVar started () >> takeMVar never)
            (putMVar cancelled ())
      result <- timeout 5000000 $ Exception.try @Exception.IOException $
        mapConcurrentlyWithBacktrace_ worker [False, True]
      case result of
        Just (Left e) -> ioeGetErrorString e @?= "worker"
        _ -> assertFailure "Expected the worker failure without hanging"
      cleanup <- timeout 5000000 (takeMVar cancelled)
      assertBool "Sibling finished its cleanup" (cleanup == Just ())

#if MIN_VERSION_base(4,20,0)
  , testCase "the original worker backtrace is rendered once" $ do
      result <- Exception.try @Exception.SomeException $
        mapConcurrentlyWithBacktrace_ (\() -> throwOriginal) [()]
      case result of
        Left e -> do
          let rendered = Exception.displayException e ++ "\n" ++ exceptionBacktrace e
          length (filter (isPrefixOf "HasCallStack backtrace:") (lines rendered)) @?= 1
          assertBool "Original throw site is present" ("throwOriginal" `isInfixOf` rendered)
          assertBool "No backtrace from async's rethrow"
            (not ("Control.Concurrent.Async" `isInfixOf` rendered))
        Right () -> assertFailure "Expected an IOException"

  , testCase "the original exception annotations are preserved" $ do
      result <- Exception.try @Exception.SomeException $
        mapConcurrentlyWithBacktrace_
          (\() -> Exception.annotateIO OriginalContext throwOriginal) [()]
      case result of
        Left e -> do
          let rendered = Exception.displayException e ++ "\n" ++ exceptionBacktrace e
          length (filter (== "original worker context") (lines rendered)) @?= 1
        Right () -> assertFailure "Expected an IOException"
#endif
  ]

#if MIN_VERSION_base(4,20,0)
throwOriginal :: HasCallStack => IO ()
throwOriginal = Exception.throwIO (userError "original worker failure")

data OriginalContext = OriginalContext deriving Show

instance ExceptionAnnotation OriginalContext where
  displayExceptionAnnotation _ = "original worker context"
#endif
