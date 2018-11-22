{-|
  Copyright   :  (C) 2014, Jan Stolarek,
                     2015-2016, University of Twente,
                     2017, QBayLogic
  License     :
    All rights reserved.

    Redistribution and use in source and binary forms, with or without
    modification, are permitted provided that the following conditions are met:

        * Redistributions of source code must retain the above copyright
          notice, this list of conditions and the following disclaimer.

        * Redistributions in binary form must reproduce the above
          copyright notice, this list of conditions and the following
          disclaimer in the documentation and/or other materials provided
          with the distribution.

        * Neither the name of Jan Stolarek nor the names of other
          contributors may be used to endorse or promote products derived
          from this software without specific prior written permission.

    THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
    "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
    LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
    A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
    OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
    SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
    LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
    DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
    THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
    (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
    OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
  Maintainer  :  Christiaan Baaij <christiaan.baaij@gmail.com>
-}

{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -fno-warn-unused-top-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- | This module provides a function that tests whether a program can
-- be run successfully. For example if you have 'foo.hs' source file:
--
-- > module Foo where
-- >
-- > foo :: Int
-- > foo = 5
--
-- you can test whether GHC can compile it:
--
-- > module Main (
-- >   main
-- >  ) where
-- >
-- > import Test.Tasty
-- > import Test.Tasty.Program
-- >
-- > main :: IO ()
-- > main = defaultMain $ testGroup "Compilation with GHC" $ [
-- >     testProgram "Foo" "ghc" ["-fforce-recomp", "foo.hs"] Nothing
-- >   ]
--
-- Program's output and error streams are ignored.

module Test.Tasty.Program (
   testProgram
 , testFailingProgram
 , PrintOutput(..)
 ) where

import Control.Exception       ( finally )
import Data.Typeable           ( Typeable                                 )
import System.Directory        ( findExecutable                           )
import System.Exit             ( ExitCode(..)                             )
import System.Process          ( cwd, readCreateProcessWithExitCode, proc )
import Test.Tasty.Providers    ( IsTest (..), Result, TestName, TestTree,
                                 singleTest, testPassed, testFailed       )
import Control.Concurrent.Lock ( Lock, acquire, release )

import NeatInterpolation    ( text )

import qualified Data.Text    as T


data PrintOutput
  = PrintBoth
  | PrintStdErr
  | PrintStdOut
  | PrintNeither


data TestProgram =
  TestProgram
    String
    -- ^ Executable
    [String]
    -- ^ Executable args
    PrintOutput
    -- ^ What output to print on test success
    Bool
    -- ^ Whether an empty stderr means test failure
    (IO FilePath)
    -- ^ Work directory
    (IO (Maybe Lock, Maybe Lock))
    -- ^ Locks to wait for / release when ready. This is a poor man's
    -- implementation of threaded tests, currently unsupported by
    -- Tasty. See: <https://github.com/feuerbach/tasty/issues/48>.
      deriving (Typeable)

data TestFailingProgram =
  TestFailingProgram
    String
    -- ^ Executable
    [String]
    -- ^ Executable args
    PrintOutput
    -- ^ What output to print on test success
    Bool
    -- ^ Whether an empty stderr means test failure
    (Maybe Int)
    -- ^ Expected return code
    (Maybe T.Text)
    -- ^ Expected string in stderr
    (IO FilePath)
    -- ^ Work directory
    (IO (Maybe Lock, Maybe Lock))
    -- ^ Locks to wait for / release when ready. This is a poor man's
    -- implementation of threaded tests, currently unsupported by
    -- Tasty. See: <https://github.com/feuerbach/tasty/issues/48>.
      deriving (Typeable)

testOutput
  :: PrintOutput
  -- ^ What output to return
  -> T.Text
  -- ^ Stderr
  -> T.Text
  -- ^ Stdout
  -> T.Text
testOutput PrintNeither _stderr _stdout = T.empty
testOutput PrintStdErr   stderr _stdout = stderr
testOutput PrintStdOut  _stderr  stdout = stdout
testOutput PrintBoth     stderr  stdout = [text|
  Stderr was:
  $stderr

  Stdout was:
  $stdout
  |]


-- | Create test that runs a program with given options. Test succeeds
-- if program terminates successfully.
testProgram
  :: TestName
  -- ^ Test name
  -> String
  -- ^ Program name
  -> [String]
  -- ^ Program options
  -> PrintOutput
  -- ^ What output to print on test success
  -> Bool
  -- ^ Whether a non-empty stdout means failure
  -> IO FilePath
  -- ^ Optional working directory
  -> IO (Maybe Lock, Maybe Lock)
  -- ^ Locks to wait for / release when ready. This is a poor man's
  -- implementation of threaded tests, currently unsupported by
  -- Tasty. See: <https://github.com/feuerbach/tasty/issues/48>.
  -> TestTree
testProgram testName program opts stdO stdF workDir locks =
  singleTest testName (TestProgram program opts stdO stdF workDir locks)

-- | Create test that runs a program with given options. Test succeeds
-- if program terminates with error
testFailingProgram
  :: TestName
  -- ^ Test name
  -> String
  -- ^ Program name
  -> [String]
  -- ^ Program options
  -> PrintOutput
  -- ^ Whether to print stdout or stderr on success
  -> Bool
  -- ^ Whether an empty stderr means failure
  -> Maybe Int
  -- ^ Expected error code. Test will *only* succeed if program fails and the
  -- returned error code is equal to the given one.
  -> (Maybe T.Text)
  -- ^ Expected string in stderr
  -> IO FilePath
  -- ^ Optional working directory
  -> IO (Maybe Lock, Maybe Lock)
  -- ^ Locks to wait for / release when ready. This is a poor man's
  -- implementation of threaded tests, currently unsupported by
  -- Tasty. See: <https://github.com/feuerbach/tasty/issues/48>.
  -> TestTree
testFailingProgram testName program opts stdO stdF errCode expectedOutput workDir locks =
  singleTest testName (TestFailingProgram program opts stdO stdF errCode expectedOutput workDir locks)

instance IsTest TestProgram where
  run opts (TestProgram program args stdO stdF workDir locks) _ = do
    (prevLock, nextLock) <- locks

    execFound <- findExecutable program

    -- Wait for previous test to finish
    maybe (return ()) acquire prevLock

    -- Execute program
    let result =
          case execFound of
            Nothing       -> return $ execNotFoundFailure program
            Just progPath -> runProgram progPath args stdO stdF =<< workDir

    finally
      -- Calculate result
      result
      -- Release lock of next test (if applicable), even if error was thrown
      (maybe (return ()) release nextLock)

  testOptions = return []

instance IsTest TestFailingProgram where
  run _opts (TestFailingProgram program args stdO stdF errCode expectedOutput workDir locks) _ = do
    (prevLock, nextLock) <- locks

    execFound <- findExecutable program

    -- Wait for previous test to finish
    maybe (return ()) acquire prevLock

    -- Execute program
    let result =
          case execFound of
            Nothing       -> return $ execNotFoundFailure program
            Just progPath -> runFailingProgram progPath args stdO stdF errCode expectedOutput =<< workDir

    finally
      -- Calculate result
      result
      -- Release lock of next test (if applicable), even if error was thrown
      (maybe (return ()) release nextLock)

  testOptions = return []

-- | Run a program with given options and optional working directory.
-- Return success if program exits with success code.
runProgram
  :: String
  -- ^ Program name
  -> [String]
  -- ^ Program options
  -> PrintOutput
  -- ^ Whether to print stdout or stderr on success
  -> Bool
  -- ^ Whether a non-empty stdout means failure
  -> FilePath
  -- ^ Optional working directory
  -> IO Result
runProgram program args stdO stdF workDir = do
  let cp = (proc program args) { cwd = Just workDir }
  (exitCode, stdout, stderr) <- readCreateProcessWithExitCode cp ""

  -- For debugging: Uncomment this to print executable and and its arguments
  --putStrLn $ show program ++ " " ++ concatMap (++ " ") args

  let stdoutT = T.pack stdout
      stderrT = T.pack stderr

  case exitCode of
    ExitSuccess ->
      if stdF && not (null stdout)
        then return (exitFailure program 1 stderrT stdoutT)
        else return (testPassed $ T.unpack $ testOutput stdO stderrT stdoutT)
    ExitFailure code ->
      return $ exitFailure program code stderrT stdoutT

-- | Run a program with given options and optional working directory.
-- Return success if program exists with error code. Fails if program does
-- not return (an expected) error code or if the program fails to execute at
-- all.
runFailingProgram
  :: String
  -- ^ Program name
  -> [String]
  -- ^ Program options
  -> PrintOutput
  -- ^ Whether to print stdout or stderr on test success
  -> Bool
  -- ^ Whether an empty stderr means test failure
  -> Maybe Int
  -- ^ Expected error code. Test will *only* succeed if program fails and the
  -- returned error code is equal to the given one.
  -> (Maybe T.Text)
  -- ^ Expected string in stderr (regular expression)
  -> FilePath
  -- ^ Optional working directory
  -> IO Result
runFailingProgram program args stdO errOnEmptyStderr expectedCode expectedStderr workDir = do
  let cp = (proc program args) { cwd = Just workDir }
  (exitCode, stdout, stderr) <- readCreateProcessWithExitCode cp ""

  -- For debugging: Uncomment this to print executable and and its arguments
  --putStrLn $ show program ++ " " ++ concatMap (++ " ") args

  let stdoutT = T.pack stdout
      stderrT = T.pack stderr

      passed = testPassed (T.unpack $ testOutput stdO stderrT stdoutT)

  return $ case exitCode of
    ExitSuccess ->
      unexpectedSuccess program stderrT stdoutT
    ExitFailure code ->
      if errOnEmptyStderr && null stderr
        then
          unexpectedEmptyStderr program code stdoutT
        else
          case expectedStderr of
            Just r | not (r `T.isInfixOf` stderrT) ->
              unexpectedStderr program code stderrT stdoutT r
            _ ->
              case expectedCode of
                Nothing -> passed
                Just n | n == code -> passed
                       | otherwise -> unexpectedCode program code n stderrT stdoutT


-- | Indicates that program does not exist in the path
execNotFoundFailure :: String -> Result
execNotFoundFailure file =
  testFailed $ "Cannot locate program " ++ file ++ " in the PATH"

-- | Indicates that program failed with an error code
exitFailure :: String -> Int -> T.Text -> T.Text -> Result
exitFailure (T.pack -> file) (showT -> code) stderr stdout =
  testFailed $ T.unpack $ [text|
    Program $file failed with error-code $code.

    Stderr was:
    $stderr

    Stdout was:
    $stdout
  |]

unexpectedStderr
  :: String
  -- ^ Program name
  -> Int
  -- ^ Code returned by program
  -> T.Text
  -- ^ stderr
  -> T.Text
  -- ^ stdout
  -> T.Text
  -- ^ Expected stderr
  -> Result
unexpectedStderr (T.pack -> file) (showT -> code) stderr stdout expectedStderr =
  testFailed $ T.unpack $ [text|
    Program $file did not print expected output to stderr. We expected:

       $expectedStderr

    Stderr was:
    $stderr

    Stdout was:
    $stdout
  |]

unexpectedEmptyStderr
  :: String
  -- ^ Program name
  -> Int
  -- ^ Code returned by program
  -> T.Text
  -- ^ stdout
  -> Result
unexpectedEmptyStderr (T.pack -> file) (showT -> code) stdout =
  testFailed $ T.unpack $ [text|
    Program $file (return code: $code) did not print anything
    to stderr unexpectedly.

    Stdout was:
    $stdout
  |]

unexpectedCode
  :: String
  -- ^ Program name
  -> Int
  -- ^ Error code returned by program
  -> Int
  -- ^ Expected code
  -> T.Text
  -- ^ stderr
  -> T.Text
  -- ^ stdout
  -> Result
unexpectedCode (T.pack -> file) (showT -> code) (showT -> expectedCode) stderr stdout =
  testFailed $ T.unpack $ [text|
    Program $file exited with code $code, but we expected $expectedCode.

    Stderr was:
    $stderr

    Stdout was:
    $stdout
  |]

unexpectedSuccess
  :: String
  -- ^ Program name
  -> T.Text
  -- stderr
  -> T.Text
  -- stdout
  -> Result
unexpectedSuccess (T.pack -> file) stderr stdout =
  testFailed $ T.unpack $ [text|
    Program exited $file succesfully, but we expected an error.

    Stderr was:
    $stderr

    Stdout was:
    $stdout
  |]

showT :: Show a => a -> T.Text
showT = T.pack . show
