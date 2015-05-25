{-# LANGUAGE DeriveDataTypeable #-}

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
 ) where

import Control.DeepSeq      ( deepseq                                  )
import Data.Typeable        ( Typeable                                 )
import Data.Proxy           ( Proxy (..)                               )
import System.Directory     ( findExecutable                           )
import System.Exit          ( ExitCode(..)                             )
import System.Process       ( runInteractiveProcess, waitForProcess    )
import System.IO            ( hGetContents                             )
import Test.Tasty.Providers ( IsTest (..), Result, TestName, TestTree,
                              singleTest, testPassed, testFailed       )
import Test.Tasty.Options   ( IsOption (..), OptionDescription(..),
                              safeRead, lookupOption, flagCLParser     )

data TestProgram = TestProgram String [String] (Maybe FilePath) Bool
     deriving (Typeable)

-- | Create test that runs a program with given options. Test succeeds
-- if program terminates successfully.
testProgram :: TestName        -- ^ Test name
            -> String          -- ^ Program name
            -> [String]        -- ^ Program options
            -> Maybe FilePath  -- ^ Optional working directory
            -> Bool            -- ^ Whether to print stdout or stderr on success
            -> TestTree
testProgram testName program opts workingDir stdO =
    singleTest testName (TestProgram program opts workingDir stdO)

instance IsTest TestProgram where
  run opts (TestProgram program args workingDir stdO) _ = do
    execFound <- findExecutable program

    case execFound of
      Nothing       -> return $ execNotFoundFailure program
      Just progPath -> runProgram progPath args workingDir stdO

  testOptions = return []

-- | Run a program with given options and optional working directory.
-- Return success if program exits with success code.
runProgram :: String          -- ^ Program name
           -> [String]        -- ^ Program options
           -> Maybe FilePath  -- ^ Optional working directory
           -> Bool            -- ^ Whether to print stdout or stderr on success
           -> IO Result
runProgram program args workingDir stdO = do
  (_, stdoutH, stderrH, pid) <- runInteractiveProcess program args workingDir Nothing

  stderr <- hGetContents stderrH
  stdout <- hGetContents stdoutH
  ecode  <- stdout `deepseq` stderr `deepseq` waitForProcess pid

  case ecode of
    ExitSuccess      -> return (testPassed (if stdO then stdout else stderr))
    ExitFailure code -> return $ exitFailure program code stderr stdout

-- | Indicates that program does not exist in the path
execNotFoundFailure :: String -> Result
execNotFoundFailure file =
  testFailed $ "Cannot locate program " ++ file ++ " in the PATH"

-- | Indicates that program failed with an error code
exitFailure :: String -> Int -> String -> String -> Result
exitFailure file code stderr stdout =
  testFailed $ "Program " ++ file ++ " failed with code " ++ show code
               ++ "\n Stderr was: \n" ++ stderr
               ++ "\n Stdout was: \n" ++ stdout
