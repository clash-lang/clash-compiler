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

import Control.DeepSeq      ( deepseq                                  )
import Data.Typeable        ( Typeable                                 )
import System.Directory     ( findExecutable                           )
import System.Exit          ( ExitCode(..)                             )
import System.Process       ( runInteractiveProcess, waitForProcess    )
import Test.Tasty.Providers ( IsTest (..), Result, TestName, TestTree,
                              singleTest, testPassed, testFailed       )
import NeatInterpolation    ( text )

import Text.Regex.PCRE ((=~))
import Text.Regex.PCRE.Text ()

import qualified Data.Text    as T
import qualified Data.Text.IO as T


data PrintOutput
  = PrintBoth
  | PrintStdErr
  | PrintStdOut
  | PrintNeither

data TestProgram =
  TestProgram String [String] (Maybe FilePath) PrintOutput Bool
     deriving (Typeable)

data TestFailingProgram =
  TestFailingProgram
    String
    -- ^ Executable
    [String]
    -- ^ Executable args
    (Maybe FilePath)
    -- ^ Working directory
    PrintOutput
    -- ^ What output to print on test success
    Bool
    -- ^ Whether an empty stderr means test failure
    (Maybe Int)
    -- ^ Expected return code
    (Maybe String)
    -- ^ Expected string in stderr (regular expression)
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
  -> Maybe FilePath
  -- ^ Optional working directory
  -> PrintOutput
  -- ^ Whether to print stdout or stderr on success
  -> Bool
  -- ^ Whether a non-empty stdout means failure
  -> TestTree
testProgram testName program opts workingDir stdO stdF =
  singleTest testName (TestProgram program opts workingDir stdO stdF)

-- | Create test that runs a program with given options. Test succeeds
-- if program terminates with error
testFailingProgram
  :: TestName
  -- ^ Test name
  -> String
  -- ^ Program name
  -> [String]
  -- ^ Program options
  -> Maybe FilePath
  -- ^ Optional working directory
  -> PrintOutput
  -- ^ Whether to print stdout or stderr on success
  -> Bool
  -- ^ Whether an empty stderr means failure
  -> Maybe Int
  -- ^ Expected error code. Test will *only* succeed if program fails and the
  -- returned error code is equal to the given one.
  -> (Maybe String)
  -- ^ Expected string in stderr (regular expression)
  -> TestTree
testFailingProgram testName program opts workingDir stdO stdF errCode expectedOutput=
  singleTest testName (TestFailingProgram program opts workingDir stdO stdF errCode expectedOutput)

instance IsTest TestProgram where
  run _opts (TestProgram program args workingDir stdO stdF) _ = do
    execFound <- findExecutable program

    case execFound of
      Nothing       -> return $ execNotFoundFailure program
      Just progPath -> runProgram progPath args workingDir stdO stdF

  testOptions = return []

instance IsTest TestFailingProgram where
  run _opts (TestFailingProgram program args workingDir stdO stdF errCode expectedOutput) _ = do
    execFound <- findExecutable program

    case execFound of
      Nothing       -> return $ execNotFoundFailure program
      Just progPath -> runFailingProgram progPath args workingDir stdO stdF errCode expectedOutput

  testOptions = return []

-- | Run a program with given options and optional working directory.
-- Return success if program exits with success code.
runProgram
  :: String
  -- ^ Program name
  -> [String]
  -- ^ Program options
  -> Maybe FilePath
  -- ^ Optional working directory
  -> PrintOutput
  -- ^ Whether to print stdout or stderr on success
  -> Bool
  -- ^ Whether a non-empty stdout means failure
  -> IO Result
runProgram program args workingDir stdO stdF = do
  (_, stdoutH, stderrH, pid) <- runInteractiveProcess program args workingDir Nothing

  stderr <- T.hGetContents stderrH
  stdout <- T.hGetContents stdoutH
  ecode  <- stdout `deepseq` stderr `deepseq` waitForProcess pid

  case ecode of
    ExitSuccess ->
      if stdF && not (T.null stdout)
        then return (exitFailure program 1 stderr stdout)
        else return (testPassed $ T.unpack $ testOutput stdO stderr stdout)
    ExitFailure code ->
      return $ exitFailure program code stderr stdout

-- | Run a program with given options and optional working directory.
-- Return success if program exists with error code. Fails if program does
-- not return (an expected) error code or if the program fails to execute at
-- all.
runFailingProgram
  :: String
  -- ^ Program name
  -> [String]
  -- ^ Program options
  -> Maybe FilePath
  -- ^ Optional working directory
  -> PrintOutput
  -- ^ Whether to print stdout or stderr on test success
  -> Bool
  -- ^ Whether an empty stderr means test failure
  -> Maybe Int
  -- ^ Expected error code. Test will *only* succeed if program fails and the
  -- returned error code is equal to the given one.
  -> (Maybe String)
  -- ^ Expected string in stderr (regular expression)
  -> IO Result
runFailingProgram program args workingDir stdO errOnEmptyStderr expectedCode expectedStderr = do
  (_, stdoutH, stderrH, pid) <- runInteractiveProcess program args workingDir Nothing

  stderr <- T.hGetContents stderrH
  stdout <- T.hGetContents stdoutH
  ecode  <- stdout `deepseq` stderr `deepseq` waitForProcess pid

  let passed = testPassed (T.unpack $ testOutput stdO stderr stdout)

  return $ case ecode of
    ExitSuccess ->
      unexpectedSuccess program stderr stdout
    ExitFailure code ->
      if errOnEmptyStderr && T.null stderr
        then
          unexpectedEmptyStderr program code stdout
        else
          case expectedStderr of
            Just r | not ((stderr =~ T.pack r) :: Bool) ->
              unexpectedStderr program code stderr stdout r
            _ ->
              case expectedCode of
                Nothing -> passed
                Just n | n == code -> passed
                       | otherwise -> unexpectedCode program code n stderr stdout


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
  -> String
  -- ^ Expected stderr (regular expression)
  -> Result
unexpectedStderr (T.pack -> file) (showT -> code) stderr stdout (T.pack -> expectedStderr) =
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