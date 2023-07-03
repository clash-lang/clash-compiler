{-|
  Copyright   :  (C) 2014, Jan Stolarek,
                     2015-2016, University of Twente,
                     2017-2022, QBayLogic
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

  This module provides a function that tests whether a program can
  be run successfully. For example if you have 'foo.hs' source file:

  > module Foo where
  >
  > foo :: Int
  > foo = 5

  you can test whether GHC can compile it:

  > module Main (
  >   main
  >  ) where
  >
  > import Test.Tasty
  > import Test.Tasty.Providers
  > import Test.Tasty.Program
  >
  > main :: IO ()
  > main =
  >   defaultMain $ testGroup "Compilation with GHC" [
  >       singleTest "Foo" $ TestProgram "ghc" ["-fforce-recomp", "Foo.hs"]
  >                            PrintNeither False Nothing []
  >     ]

  Program's output and error streams are ignored.
-}

{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE QuasiQuotes #-}

module Test.Tasty.Program (
   TestProgram(..)
 , TestFailingProgram(..)
 , PrintOutput(..)
 , ExpectOutput(..)
 ) where

import qualified Clash.Util.Interpolate  as I
import qualified Data.List as List

import Control.Applicative     ( Alternative (..) )
import Data.Typeable           ( Typeable                                 )
import Data.Maybe              ( isNothing, listToMaybe                   )
import System.FilePath.Posix   ( getSearchPath )
import System.Directory        ( findExecutable,
                                 findExecutablesInDirectories )
import System.Environment      ( getEnvironment,                          )
import System.Exit             ( ExitCode(..)                             )
import System.Process          ( cwd, env, readCreateProcessWithExitCode,
                                 proc                                     )
import Test.Tasty.Providers    ( IsTest (..), Result, TestName, TestTree,
                                 singleTest, testPassed, testFailed       )

import Data.String.Interpolate ( __i )
import Text.Regex.TDFA.Text ( Regex, execute )

import qualified Data.Text    as T

data ExpectOutput a
  = ExpectStdOut a
  | ExpectStdErr a
  | ExpectEither a
  | ExpectNotStdErr a
  | ExpectMatchStdOut !Regex
  | ExpectNotMatchStdOut !Regex
  | ExpectNothing
  deriving Functor

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
    -- ^ Whether a non-empty stdout means failure
    (Maybe FilePath)
    -- ^ Work directory
    [(String, String)]
    -- ^ Additional environment variables
      deriving (Typeable)

data TestFailingProgram =
  TestFailingProgram
    Bool
    -- ^ Test exit code?
    String
    -- ^ Executable
    [String]
    -- ^ Executable args
    PrintOutput
    -- ^ What output to print on test success
    Bool
    -- ^ Whether an empty stderr means test failure
    (Maybe Int)
    -- ^ Expected error code. Test will *only* succeed if program fails and the
    -- returned error code is equal to the given one.
    (ExpectOutput T.Text)
    -- ^ Expected output
    (ExpectOutput T.Text)
    -- ^ SILENCE the test (which doesn't count as a failure) if the output
    -- matches this. Vivado sometimes fails to run the test, but at other times
    -- it works. By distinguishing specific failures, we still get the test when
    -- it does work.
    (Maybe FilePath)
    -- ^ Optional working directory
    [(String, String)]
    -- ^ Additional environment variables
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
testOutput PrintBoth     stderr  stdout = [__i|
  Stderr was:
  #{stderr}

  Stdout was:
  #{stdout}
  |]

cleanNewlines :: T.Text -> T.Text
cleanNewlines = T.replace "  " " " . T.replace "\n" " "

-- | Find the location of a program.
--
-- On Windows, 'findExecutable' uses Windows native search locations (things
-- like the @Program Files@ directory) and the System PATH variable. This
-- System PATH variable is distinct from the User PATH variable, so when the
-- User PATH contains more search directories than the System PATH,
-- 'findExecutable' won't look in those additional directories.
--
-- This function does look in the User PATH when a program isn't found using
-- the native system way.
--
-- On Linux, this function behaves exactly like 'findExecutable'.
findExecutableAlt :: String -> IO (Maybe FilePath)
findExecutableAlt program = do
  execFoundSystem <- findExecutable program
  path <- getSearchPath
  execFoundPath <- listToMaybe <$> (findExecutablesInDirectories path program)
  return (execFoundSystem <|> execFoundPath)

instance IsTest TestProgram where
  run _opts (TestProgram program args stdO stdF workDir addEnv) _ = do
    execFound <- findExecutableAlt program

    -- Execute program
    case execFound of
      Nothing       -> return $ execNotFoundFailure program
      Just progPath -> runProgram progPath args stdO stdF workDir addEnv

  testOptions = return []

instance IsTest TestFailingProgram where
  run _opts (TestFailingProgram testExitCode program args stdO stdF errCode expectedOutput silencedOutput workDir addEnv) _ = do
    execFound <- findExecutableAlt program

    -- Execute program
    case execFound of
      Nothing       -> return $ execNotFoundFailure program
      Just progPath -> runFailingProgram testExitCode progPath args stdO stdF errCode expectedOutput silencedOutput workDir addEnv

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
  -> Maybe FilePath
  -- ^ Optional working directory
  -> [(String, String)]
  -- ^ Additional environment variables
  -> IO Result
runProgram program args stdO stdF workDir addEnv = do
  e <- getEnvironment
  let cp = (proc program args) { cwd = workDir, env = Just (addEnv ++ e) }
  (exitCode, stdout, stderr) <- readCreateProcessWithExitCode cp ""

  -- For debugging: Uncomment this to print executable and and its arguments
  --putStrLn $ show program ++ " " ++ concatMap (++ " ") args

  let stdoutT = T.pack stdout
      stderrT = T.pack stderr

  case exitCode of
    ExitSuccess ->
      if stdF && not (null stdout)
        then return (unexpectedNonEmptyStdout program args 0 stderrT stdoutT)
        else return (testPassed $ T.unpack $ testOutput stdO stderrT stdoutT)
    ExitFailure code ->
      return $ exitFailure program args code stderrT stdoutT

-- | Run a program with given options and optional working directory.
-- Return success if program exists with error code. Fails if program does
-- not return (an expected) error code or if the program fails to execute at
-- all.
runFailingProgram
  :: Bool
  -- ^ Test exit code?
  -> String
  -- ^ Executable
  -> [String]
  -- ^ Executable args
  -> PrintOutput
  -- ^ What output to print on test success
  -> Bool
  -- ^ Whether an empty stderr means test failure
  -> Maybe Int
  -- ^ Expected error code. Test will *only* succeed if program fails and the
  -- returned error code is equal to the given one.
  -> ExpectOutput T.Text
  -- ^ Expected output
  -> ExpectOutput T.Text
  -- ^ SILENCE the test (which doesn't count as a failure) if the output matches
  -- this. Vivado sometimes causes spurious failures. By distinguishing specific
  -- failures, we still get to run the test when it does work.
  -> Maybe FilePath
  -- ^ Optional working directory
  -> [(String, String)]
  -- ^ Additional environment variables
  -> IO Result
runFailingProgram testExitCode program args stdO errOnEmptyStderr expectedCode expectedOutput silencedOutput workDir addEnv = do
  e <- getEnvironment
  let cp = (proc program args) { cwd = workDir, env = Just (addEnv ++ e) }
  (exitCode0, stdout, stderr) <- readCreateProcessWithExitCode cp ""

  -- For debugging: Uncomment this to print executable and and its arguments
  --putStrLn $ show program ++ " " ++ concatMap (++ " ") args

  let stdoutT = T.pack stdout
      stderrT = T.pack stderr

      passed = testPassed (T.unpack $ testOutput stdO stderrT stdoutT)

  return (go (stdoutT, stderrT, passed) exitCode0)

 where
  go e@(stdoutT, stderrT, passed) exitCode1 =
    case exitCode1 of
      ExitSuccess ->
        if (testExitCode && isNothing expectedCode) then
          unexpectedSuccess program stderrT stdoutT
        else
          go e (ExitFailure 0)
      ExitFailure code
        | errOnEmptyStderr && T.null stderr
        -> unexpectedEmptyStderr program code stdoutT
        | Just result <- checkSilenced
        -> result
        | otherwise
        -> case expectedOutput of
             ExpectStdErr r | not (cleanNewlines r `T.isInfixOf` cleanNewlines stderrT) ->
               unexpectedStd "stderr" program args code stderrT stdoutT r
             ExpectStdOut r | not (cleanNewlines r `T.isInfixOf` cleanNewlines stdoutT) ->
               unexpectedStd "stdout" program args code stderrT stdoutT r
             ExpectEither r
               |  not (cleanNewlines r `T.isInfixOf` cleanNewlines stdoutT)
               && not (cleanNewlines r `T.isInfixOf` cleanNewlines stderrT)
               ->
               unexpectedStd "stdout or stderr" program args code stderrT stdoutT r
             ExpectNotStdErr r | cleanNewlines r `T.isInfixOf` cleanNewlines stderrT ->
               unexpectedNonEmptyStderr program args code stderrT stdoutT
             ExpectMatchStdOut re | Right Nothing <- execute re stdoutT ->
               unmatchedStd "stdout" program args code stderrT stdoutT
             ExpectMatchStdOut re | Left err <- execute re stdoutT ->
               testFailed err
             ExpectNotMatchStdOut re | Right (Just{}) <- execute re stdoutT ->
               unexpectedNonEmptyStdout program args code stderrT stdoutT
             ExpectNotMatchStdOut re | Left err <- execute re stdoutT ->
               testFailed err
             _ ->
               if testExitCode then
                 case expectedCode of
                   Nothing -> passed
                   Just n | n == code -> passed
                          | otherwise -> unexpectedCode program code n stderrT stdoutT
               else
                 passed
   where
    testOutput test stdoutT stderrT = case test of
      ExpectStdErr r = Right $
        not $ cleanNewlines r `T.isInfixOf` cleanNewlines stderrT
      ExpectStdOut r = Right $
        not $ cleanNewlines r `T.isInfixOf` cleanNewlines stdoutT
      ExpectEither r = Right $
           not (cleanNewlines r `T.isInfixOf` cleanNewlines stdoutT)
        && not (cleanNewlines r `T.isInfixOf` cleanNewlines stderrT)
      ExpectNotStdErr r = Right $
        cleanNewlines r `T.isInfixOf` cleanNewlines stderrT
      ExpectMatchStdOut re = either id isNothing $ execute re stdoutT
      ExpectNotMatchStdOut re = either id isJust $ execute re stdoutT
      _ -> Right False

    failExpected expected stdoutT stderrT = case expected of

    checkSilenced = case silencedOutput of
      -- TODO
      ExpectStdErr r | not (cleanNewlines r `T.isInfixOf` cleanNewlines stderrT) ->
        unexpectedStd "stderr" program args code stderrT stdoutT r
      ExpectStdOut r | not (cleanNewlines r `T.isInfixOf` cleanNewlines stdoutT) ->
        unexpectedStd "stdout" program args code stderrT stdoutT r
      ExpectEither r
        |  not (cleanNewlines r `T.isInfixOf` cleanNewlines stdoutT)
        && not (cleanNewlines r `T.isInfixOf` cleanNewlines stderrT)
        ->
        unexpectedStd "stdout or stderr" program args code stderrT stdoutT r
      ExpectNotStdErr r | cleanNewlines r `T.isInfixOf` cleanNewlines stderrT ->
        unexpectedNonEmptyStd "stderr" program args code stderrT stdoutT
      ExpectMatchStdOut re | Right Nothing <- execute re stdoutT ->
        unmatchedStd "stdout" program args code stderrT stdoutT
      ExpectMatchStdOut re | Left err <- execute re stdoutT ->
        testFailed err
      ExpectNotMatchStdOut re | Right (Just{}) <- execute re stdoutT ->
        unexpectedNonEmptyStd "stdout" program args code stderrT stdoutT
      ExpectNotMatchStdOut re | Left err <- execute re stdoutT ->
        testFailed err
      _ ->
        if testExitCode then
          case expectedCode of
            Nothing -> passed
            Just n | n == code -> passed
                   | otherwise -> unexpectedCode program code n stderrT stdoutT
        else
          passed

-- | Indicates that program does not exist in the path
execNotFoundFailure :: String -> Result
execNotFoundFailure file =
  testFailed $ "Cannot locate program " ++ file ++ " in the PATH"

testFailureVerbose
  :: T.Text
  -- ^ Error message
  -> String
  -- ^ Executable
  -> [String]
  -- ^ Executable arguments
  -> Maybe T.Text
  -- ^ stderr (@Nothing@ means the error message already says it is empty)
  -> Maybe T.Text
  -- ^ stdo (@Nothing@ means the error message already says it is empty)
-- | Indicates that program failed with an error code
exitFailure :: String -> [String] -> Int -> T.Text -> T.Text -> Result
exitFailure cmd args code stderr stdout =
  testFailed [I.i|
    Program #{cmd} failed with error-code #{code}.

    Full invocation:

      #{cmd} #{List.intercalate " " args}

    Stderr was:

      #{stderr}

    Stdout was:

      #{stdout}
  |]

unexpectedNonEmptyStd
  :: T.Text
  -- ^ Expected output name
  -> String
  -- ^ Program name
  -> [String]
  -- ^ Program arguments
  -> Int
  -- ^ Code returned by program
  -> T.Text
  -- ^ stderr
  -> T.Text
  -- ^ stdout
  -> Result
unexpectedNonEmptyStdout expectedOut cmd args code stderr stdout =
  testFailed [I.i|
    Program #{cmd} (return code: #{code}) printed to #{expectedOut} unexpectedly.

    Full invocation:

      #{cmd} #{List.intercalate " " args}

    Stderr was:

      #{stderr}

    Stdout was:

      #{stdout}
  |]

unexpectedStd
  :: T.Text
  -- ^ Expected output name
  -> String
  -- ^ Program name
  -> [String]
  -- ^ Program arguments
  -> Int
  -- ^ Code returned by program
  -> T.Text
  -- ^ stderr
  -> T.Text
  -- ^ stdout
  -> T.Text
  -- ^ Expected stderr
  -> Result
unexpectedStd expectedOut cmd args code stderr stdout expected =
  testFailed [I.i|
    Program #{cmd} (return code #{code}) did not print expected output to #{expectedOut}. We expected:

       #{expected}

    Full invocation:

      #{cmd} #{List.intercalate " " args}

    Stderr was:

      #{stderr}

    Stdout was:

      #{stdout}
  |]

unmatchedStd
  :: T.Text
  -- ^ Expected output name
  -> String
  -- ^ Program name
  -> [String]
  -- ^ Program arguments
  -> Int
  -- ^ Code returned by program
  -> T.Text
  -- ^ stderr
  -> T.Text
  -- ^ stdout
  -> Result
unexpectedStd expectedOut cmd args code stderr stdout =
  testFailed [I.i|
    The #{expectedOut} of program #{cmd} (return code #{code}) did not match the expected regular expression.

    Full invocation:

      #{cmd} #{List.intercalate " " args}

    Stderr was:

      #{stderr}

    Stdout was:

      #{stdout}
  |]

unexpectedEmptyStderr
  :: String
  -- ^ Program name
  -> Int
  -- ^ Code returned by program
  -> T.Text
  -- ^ stdout
  -> Result
unexpectedEmptyStderr file code stdout =
  testFailed [__i|
    Program #{file} (return code: #{code}) did not print anything
    to stderr unexpectedly.

    Stdout was:
    #{stdout}
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
unexpectedCode file code expectedCode stderr stdout =
  testFailed [__i|
    Program #{file} exited with code #{code}, but we expected #{expectedCode}.

    Stderr was:
    #{stderr}

    Stdout was:
    #{stdout}
  |]

unexpectedSuccess
  :: String
  -- ^ Program name
  -> T.Text
  -- stderr
  -> T.Text
  -- stdout
  -> Result
unexpectedSuccess file stderr stdout =
  testFailed [__i|
    Program #{file} exited succesfully, but we expected an error.

    Stderr was:
    #{stderr}

    Stdout was:
    #{stdout}
  |]
