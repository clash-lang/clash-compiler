{-|
  Copyright   :  (C) 2014, Jan Stolarek,
                     2015-2016, University of Twente,
                     2017-2023, QBayLogic
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
{-# LANGUAGE LambdaCase #-}
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
import Data.Bifunctor          ( second                                   )
import Data.Typeable           ( Typeable                                 )
import Data.Maybe              ( catMaybes, fromMaybe, isJust, isNothing,
                                 listToMaybe                              )
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
        then return (unexpectedNonEmptyStd "stdout" program args 0 stderrT stdoutT)
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
        | errOnEmptyStderr && T.null stderrT
        -> unexpectedEmptyStdErr stdoutT program args code
        | Just result <- checkSilenced code
        -> result
        | Just result <- checkExpected code
        -> result
        | testExitCode
        -> case expectedCode of
             Nothing -> passed
             Just n | n == code
                    -> passed
                    | otherwise
                    -> unexpectedCode n stderrT stdoutT program args code
        | otherwise
        -> passed
   where
    checkSilenced code = case checkOutput silencedOutput of
      Left err -> Just $ testFailed err
      Right True -> Just $ failSilenced code
      Right False -> Nothing

    checkExpected code = case checkOutput expectedOutput of
      Left err -> Just $ testFailed err
      Right True -> Just $ failExpected code
      Right False -> Nothing

    checkOutput = \case
      ExpectStdOut r -> Right $
        not $ cleanNewlines r `T.isInfixOf` cleanNewlines stdoutT
      ExpectStdErr r -> Right $
        not $ cleanNewlines r `T.isInfixOf` cleanNewlines stderrT
      ExpectEither r -> Right $
           not (cleanNewlines r `T.isInfixOf` cleanNewlines stdoutT)
        && not (cleanNewlines r `T.isInfixOf` cleanNewlines stderrT)
      ExpectNotStdErr r -> Right $
        cleanNewlines r `T.isInfixOf` cleanNewlines stderrT
      ExpectMatchStdOut re -> second isNothing $ execute re stdoutT
      ExpectNotMatchStdOut re -> second isJust $ execute re stdoutT
      ExpectNothing -> Right False

    failSilenced = undefined -- TODO

    failExpected code = case expectedOutput of
      ExpectStdOut r ->
        unexpectedStd "stdout" r stderrT stdoutT program args code
      ExpectStdErr r ->
        unexpectedStd "stderr" r stderrT stdoutT program args code
      ExpectEither r ->
        unexpectedStd "stdout or stderr" r stderrT stdoutT program args code
      ExpectNotStdErr r ->
        unexpectedNotStd "stderr" r stderrT stdoutT program args code
      ExpectMatchStdOut _ ->
        unmatchedStd "stdout" stderrT stdoutT program args code
      ExpectNotMatchStdOut _ ->
        matchedStd "stdout" stderrT stdoutT program args code
      ExpectNothing ->
        error "Cannot fail with ExpectNothing"

-- | Indicates that program does not exist in the path
execNotFoundFailure :: String -> Result
execNotFoundFailure file =
  testFailed $ "Cannot locate program " ++ file ++ " in the PATH"

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
unexpectedNonEmptyStd expectedOut cmd args code stderr stdout =
  testFailed [I.i|
    Program #{cmd} (return code: #{code}) printed to #{expectedOut} unexpectedly.

    Full invocation:

      #{cmd} #{List.intercalate " " args}

    Stderr was:

      #{stderr}

    Stdout was:

      #{stdout}
  |]

unexpectedEmptyStdErr
  :: T.Text
  -- ^ stdout
  -> String
  -- ^ Program name
  -> [String]
  -- ^ Program arguments
  -> Int
  -- ^ Code returned by program
  -> Result
unexpectedEmptyStdErr stdout =
  testFailureVerbose [__i|
    Program did not print anything to stderr unexpectedly.
    |]
    Nothing (Just stdout)

unexpectedCode
  :: Int
  -- ^ Expected code
  -> T.Text
  -- ^ stderr
  -> T.Text
  -- ^ stdout
  -> String
  -- ^ Program name
  -> [String]
  -- ^ Program arguments
  -> Int
  -- ^ Code returned by program
  -> Result
unexpectedCode expectedCode stderr stdout =
  testFailureVerbose [I.i|
    Program did not exit with expected code #{expectedCode}.
    |]
    (Just stderr) (Just stdout)

unexpectedStd, unexpectedNotStd
  :: T.Text
  -- ^ Expected output name
  -> T.Text
  -- ^ Expected message
  -> T.Text
  -- ^ stderr
  -> T.Text
  -- ^ stdout
  -> String
  -- ^ Program name
  -> [String]
  -- ^ Program arguments
  -> Int
  -- ^ Code returned by program
  -> Result

unexpectedStd expectedOut expected stderr stdout =
  testFailureVerbose [I.i|
    Program did not print expected output to #{expectedOut}. We expected:

       #{expected}
    |]
    (Just stderr) (Just stdout)

unexpectedNotStd expectedOut expected stderr stdout =
  testFailureVerbose [I.i|
    Program printed failure message to #{expectedOut}. Failure message found:

       #{expected}
    |]
    (Just stderr) (Just stdout)

unmatchedStd, matchedStd
  :: T.Text
  -- ^ Expected output name
  -> T.Text
  -- ^ stderr
  -> T.Text
  -- ^ stdout
  -> String
  -- ^ Program name
  -> [String]
  -- ^ Program arguments
  -> Int
  -- ^ Code returned by program
  -> Result

unmatchedStd expectedOut stderr stdout =
  testFailureVerbose [I.i|
    Program output on #{expectedOut} did not match expected regular expression.
    |]
    (Just stderr) (Just stdout)

matchedStd expectedOut stderr stdout =
  testFailureVerbose [I.i|
    Program output on #{expectedOut} matched regular expression indicating failure.
    |]
    (Just stderr) (Just stdout)

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

testFailureVerbose
  :: String
  -- ^ Error message
  -> Maybe T.Text
  -- ^ stderr (@Nothing@ means the error message already says it is empty)
  -> Maybe T.Text
  -- ^ stdout (@Nothing@ means the error message already says it is empty)
  -> String
  -- ^ Executable
  -> [String]
  -- ^ Executable arguments
  -> Int
  -- ^ Error code returned by executable
  -> Result
testFailureVerbose msg stderrM stdoutM program args code =
  testFailed $ details <> fromMaybe "" (mconcat
    [ fmap (stdReport "Stderr") stderrM
    , fmap (stdReport "Stdout") stdoutM
    ])
 where
  details = [I.i|
    #{msg}

    Full invocation:

      #{program} #{List.intercalate " " args}

    Return code: #{code}
    |]
  stdReport :: String -> T.Text -> String
  stdReport whichStd std = "\n\n" <> [I.i|
    #{whichStd} was:

      #{std}
    |]
