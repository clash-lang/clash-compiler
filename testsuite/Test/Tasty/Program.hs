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

data TestProgram = TestProgram String [String] (Maybe FilePath) Bool Bool
     deriving (Typeable)

-- | Create test that runs a program with given options. Test succeeds
-- if program terminates successfully.
testProgram :: TestName        -- ^ Test name
            -> String          -- ^ Program name
            -> [String]        -- ^ Program options
            -> Maybe FilePath  -- ^ Optional working directory
            -> Bool            -- ^ Whether to print stdout or stderr on success
            -> Bool            -- ^ Whether a non-empty stdout means failure
            -> TestTree
testProgram testName program opts workingDir stdO stdF =
    singleTest testName (TestProgram program opts workingDir stdO stdF)

instance IsTest TestProgram where
  run opts (TestProgram program args workingDir stdO stdF) _ = do
    execFound <- findExecutable program

    case execFound of
      Nothing       -> return $ execNotFoundFailure program
      Just progPath -> runProgram progPath args workingDir stdO stdF

  testOptions = return []

-- | Run a program with given options and optional working directory.
-- Return success if program exits with success code.
runProgram :: String          -- ^ Program name
           -> [String]        -- ^ Program options
           -> Maybe FilePath  -- ^ Optional working directory
           -> Bool            -- ^ Whether to print stdout or stderr on success
           -> Bool            -- ^ Whether a non-empty stdout means failure
           -> IO Result
runProgram program args workingDir stdO stdF = do
  (_, stdoutH, stderrH, pid) <- runInteractiveProcess program args workingDir Nothing

  stderr <- hGetContents stderrH
  stdout <- hGetContents stdoutH
  ecode  <- stdout `deepseq` stderr `deepseq` waitForProcess pid

  case ecode of
    ExitSuccess      -> if stdF && not (null stdout)
                           then return (exitFailure program 1 stderr stdout)
                           else return (testPassed (if stdO then stdout else stderr))
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
