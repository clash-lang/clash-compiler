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
  > import Test.Tasty.Program
  >
  > main :: IO ()
  > main = defaultMain $ testGroup "Compilation with GHC" $ [
  >     testProgram "Foo" "ghc" ["-fforce-recomp", "foo.hs"] Nothing
  >   ]

  Program's output and error streams are ignored.
-}

{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -fno-warn-unused-top-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

module Test.Tasty.Program (
   testProgram
 , testFailingProgram
 , PrintOutput(..)
 , GlobArgs(..)
 , ExpectOutput(..)
 , TestProgram(..)
 , TestFailingProgram(..)
 , TestHeisenbugProgram(..)
 , TestFailingHeisenbugProgram(..)
 ) where

import qualified Clash.Util.Interpolate  as I
import qualified Data.List as List

import Control.Applicative     ( Alternative (..) )
import Control.Monad.Extra     ( firstJustM, unless                       )
import Data.Typeable           ( Typeable                                 )
import Data.Maybe              ( fromMaybe, isNothing, listToMaybe        )
import System.FilePath.Glob    ( globDir1, compile                        )
import System.FilePath.Posix   ( getSearchPath )
import System.Directory        ( findExecutable,
                                 findExecutablesInDirectories,
                                 getCurrentDirectory )
import System.Environment      ( getEnvironment,                          )
import System.Exit             ( ExitCode(..)                             )
import System.Process          ( cwd, env, readCreateProcessWithExitCode,
                                 proc                                     )
import Test.Tasty.Providers    ( IsTest (..), Result, TestName, TestTree,
                                 singleTest, testPassed, testFailed       )

import Data.String.Interpolate ( __i )
import Text.Regex.TDFA.Text ( Regex, execute )

import qualified Data.Text    as T

import Debug.Trace (traceIO)

data ExpectOutput a
  = ExpectStdOut a
  | ExpectStdErr a
  | ExpectEither a
  | ExpectNotStdErr a
  | ExpectNotMatchStdOut !Regex
  | ExpectNothing
  deriving Functor


data GlobArgs
  = GlobStar
  -- ^ Glob all argument with a star (*) in them
  | NoGlob
  -- ^ No globbing here, mister


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
    GlobArgs
    -- ^ Whether to interpret glob patterns in arguments
    PrintOutput
    -- ^ What output to print on test success
    Bool
    -- ^ Whether a non-empty stdout means failure
    (Maybe FilePath)
    -- ^ Work directory
    [(String, String)]
    -- ^ Additional environment variables
      deriving (Typeable)

data TestHeisenbugProgram =
  TestHeisenbugProgram
    String
    -- ^ Executable
    [String]
    -- ^ Executable args
    GlobArgs
    -- ^ Whether to interpret glob patterns in arguments
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
    -- ^ Test exit code
    String
    -- ^ Executable
    [String]
    -- ^ Executable args
    GlobArgs
    -- ^ Whether to interpret glob patterns in arguments
    PrintOutput
    -- ^ What output to print on test success
    Bool
    -- ^ Whether an empty stderr means test failure
    (Maybe Int)
    -- ^ Expected return code
    (ExpectOutput T.Text)
    -- ^ Expected string in stderr
    (Maybe FilePath)
    -- ^ Work directory
    [(String, String)]
    -- ^ Additional environment variables
      deriving (Typeable)

data TestFailingHeisenbugProgram =
  TestFailingHeisenbugProgram
    Bool
    -- ^ Test exit code
    String
    -- ^ Executable
    [String]
    -- ^ Executable args
    GlobArgs
    -- ^ Whether to interpret glob patterns in arguments
    PrintOutput
    -- ^ What output to print on test success
    Bool
    -- ^ Whether an empty stderr means test failure
    (Maybe Int)
    -- ^ Expected return code
    (ExpectOutput T.Text)
    -- ^ Expected string in stderr
    (Maybe FilePath)
    -- ^ Work directory
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

globArgs
  :: GlobArgs
  -> Maybe FilePath
  -> [String]
  -> IO [String]
globArgs NoGlob _dir args = return args
globArgs GlobStar dir args = do
  cwd0 <- getCurrentDirectory
  concat <$> mapM (globArg' cwd0) args
  where
    globArg' cwd1 arg
      | '*' `elem` arg = globDir1 (compile arg) (fromMaybe cwd1 dir)
      | otherwise      = return [arg]

-- | Create test that runs a program with given options. Test succeeds
-- if program terminates successfully.
testProgram
  :: TestName
  -- ^ Test name
  -> String
  -- ^ Program name
  -> [String]
  -- ^ Program arguments
  -> GlobArgs
  -- ^ Whether to interpret glob patterns in arguments
  -> PrintOutput
  -- ^ What output to print on test success
  -> Bool
  -- ^ Whether a non-empty stdout means failure
  -> Maybe FilePath
  -- ^ Optional working directory
  -> TestTree
testProgram testName program opts glob stdO stdF workDir =
  singleTest testName (TestProgram program opts glob stdO stdF workDir [])

cleanNewlines :: T.Text -> T.Text
cleanNewlines = T.replace "  " " " . T.replace "\n" " "

-- | Create test that runs a program with given options. Test succeeds
-- if program terminates with error
testFailingProgram
  :: Bool
  -- ^ Test exit code?
  -> TestName
  -- ^ Test name
  -> String
  -- ^ Program name
  -> [String]
  -- ^ Program options
  -> GlobArgs
  -- ^ Whether to interpret glob patterns in arguments
  -> PrintOutput
  -- ^ Whether to print stdout or stderr on success
  -> Bool
  -- ^ Whether an empty stderr means failure
  -> Maybe Int
  -- ^ Expected error code. Test will *only* succeed if program fails and the
  -- returned error code is equal to the given one.
  -> ExpectOutput T.Text
  -- ^ Expected string in stderr
  -> Maybe FilePath
  -- ^ Optional working directory
  -> TestTree
testFailingProgram testExitCode testName program opts glob stdO stdF errCode expectedOutput workDir =
  singleTest testName (TestFailingProgram testExitCode program opts glob stdO stdF errCode expectedOutput workDir [])

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
  run opts (TestProgram program args glob stdO stdF workDir addEnv) _ = do
    execFound <- findExecutableAlt program

    args' <- globArgs glob workDir args

    -- Execute program
    case execFound of
      Nothing       -> return $ execNotFoundFailure program
      Just progPath -> runProgram False progPath args' stdO stdF workDir addEnv

  testOptions = return []

instance IsTest TestHeisenbugProgram where
  run opts (TestHeisenbugProgram program args glob stdO stdF workDir addEnv) _ = do
    execFound <- findExecutableAlt program

    args' <- globArgs glob workDir args

    -- Execute program
    case execFound of
      Nothing       -> return $ execNotFoundFailure program
      Just progPath -> runProgram True progPath args' stdO stdF workDir addEnv

  testOptions = return []

instance IsTest TestFailingProgram where
  run _opts (TestFailingProgram testExitCode program args glob stdO stdF errCode expectedOutput workDir addEnv) _ = do
    execFound <- findExecutableAlt program

    args' <- globArgs glob workDir args

    -- Execute program
    case execFound of
      Nothing       -> return $ execNotFoundFailure program
      Just progPath -> runFailingProgram False testExitCode progPath args stdO stdF errCode expectedOutput workDir addEnv

  testOptions = return []

instance IsTest TestFailingHeisenbugProgram where
  run _opts (TestFailingHeisenbugProgram testExitCode program args glob stdO stdF errCode expectedOutput workDir addEnv) _ = do
    execFound <- findExecutableAlt program

    args' <- globArgs glob workDir args

    -- Execute program
    case execFound of
      Nothing       -> return $ execNotFoundFailure program
      Just progPath -> runFailingProgram True testExitCode progPath args stdO stdF errCode expectedOutput workDir addEnv

  testOptions = return []

-- | Run a program with given options and optional working directory.
-- Return success if program exits with success code.
runProgram
  :: Bool
  -- ^ Should we retry when we see the heisenbug error msg?
  -> String
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
runProgram heisen program args stdO stdF workDir addEnv = do
  e <- getEnvironment
  let cp = (proc program args) { cwd = workDir, env = Just (addEnv ++ e) }
  (exitCode, stdout, stderr) <-
    filterHeisenbugProcess heisen $ readCreateProcessWithExitCode cp ""

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
  -- ^ Should we retry when we see the heisenbug error msg?
  -> Bool
  -- ^ Test exit code?
  -> String
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
  -> ExpectOutput T.Text
  -- ^ Expected string in stderr
  -> Maybe FilePath
  -- ^ Optional working directory
  -> [(String, String)]
  -- ^ Additional environment variables
  -> IO Result
runFailingProgram heisen testExitCode program args stdO errOnEmptyStderr expectedCode expectedStderr workDir addEnv = do
  e <- getEnvironment
  let cp = (proc program args) { cwd = workDir, env = Just (addEnv ++ e) }
  (exitCode0, stdout, stderr) <-
    filterHeisenbugProcess heisen $ readCreateProcessWithExitCode cp ""

  -- For debugging: Uncomment this to print executable and and its arguments
  --putStrLn $ show program ++ " " ++ concatMap (++ " ") args

  let stdoutT = T.pack stdout
      stderrT = T.pack stderr

      passed = testPassed (T.unpack $ testOutput stdO stderrT stdoutT)

  return (go (stdoutT, stderrT, stdout, stderr, passed) exitCode0)

 where
  -- TODO: Clean up this code..
  go e@(stdoutT, stderrT, _stdout, stderr, passed) exitCode1 =
    case exitCode1 of
      ExitSuccess ->
        if (testExitCode && isNothing expectedCode) then
          unexpectedSuccess program stderrT stdoutT
        else
          go e (ExitFailure 0)
      ExitFailure code ->
        if errOnEmptyStderr && null stderr
          then
            unexpectedEmptyStderr program code stdoutT
          else
            case expectedStderr of
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

filterHeisenbugProcess
  :: Bool
  -- ^ Should we retry when we see the heisenbug error msg?
  -> IO (ExitCode, String, String)
  -> IO (ExitCode, String, String)
filterHeisenbugProcess heisen process =
  if heisen then do
    res <- firstJustM skipHeisenbug [(1 :: Int) .. 20]
    case res of
      Just res0 -> pure res0
      Nothing -> pure (ExitFailure (-6), "", "Heisenbug limit exceeded")
  else do
    process
 where
  skipHeisenbug cnt = do
    unless (cnt == 1) $
      traceIO ("Retrying due to heisenbug (try: " <> show cnt <> ")")
    res@(_exitCode, _stdout, stderr) <- process
    if heisenMsg `T.isInfixOf` (T.pack stderr)
      then pure Nothing
      else pure $ Just res
  heisenMsg = T.pack "mmap 131072 bytes at (nil): Cannot allocate memory"

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

unexpectedNonEmptyStdout
  :: String
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
unexpectedNonEmptyStdout cmd args code stderr stdout =
  testFailed [I.i|
    Program #{cmd} (return code: #{code}) printed to stdout unexpectedly.

    Full invocation:

      #{cmd} #{List.intercalate " " args}

    Stderr was:

      #{stderr}

    Stdout was:

      #{stdout}
  |]

unexpectedNonEmptyStderr
  :: String
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
unexpectedNonEmptyStderr cmd args code stderr stdout =
  testFailed [I.i|
    Program #{cmd} (return code: #{code}) printed to stderr unexpectedly.

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
