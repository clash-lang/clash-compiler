{-# LANGUAGE TypeApplications #-}

module Test.Tasty.Verilator where

import Control.Monad (filterM, forM_)
import Data.Coerce (coerce)
import Data.Text (Text)
import System.Directory (copyFile, doesDirectoryExist, listDirectory)
import System.FilePath ((</>))
import System.FilePath.Glob (glob)
import Test.Tasty.Common
import Test.Tasty.Program
import Test.Tasty.Providers

data VerilatorMakeTest = VerilatorMakeTest
  { vmDirectory :: IO FilePath
  , vmTop :: String
  }

instance IsTest VerilatorMakeTest where
  run optionSet (VerilatorMakeTest getDir top) progressCallback = do
    dir <- getDir
    libs <- listDirectory dir >>= filterM (doesDirectoryExist . (dir </>))

    -- Only pass the sources for the shim and the entity to simulate. The other
    -- modules will be found in the included directories. The path to the C++
    -- shim MUST include the subdirectories below the working directory
    -- explicitly.
    cSrc <- glob (dir </> "*" </> top <> "_shim.cpp")
    vSrc <- glob (dir </> "*" </> top <> ".v")

    -- Types modules have to be given first, or verilator will complain that
    -- they are not already declared when it sees them being imported.
    svSrc <- mappend
      <$> glob (dir </> "*" </> "*_types.sv")
      <*> glob (dir </> "*" </> top <> ".sv")

    -- Clash by default will not mix HDLs in it's output. If this ever changes,
    -- and it is possible to have `clash` output both Verilog and SystemVerilog
    -- then this will need to change.
    runVerilator dir (mkArgs libs (cSrc <> vSrc <> svSrc))
   where
    mkArgs libs srcs =
      ["-I" <> lib | lib <- libs]
        <> [ "-Wno-fatal"         -- Do not abort on warnings
           , "-Wall"
           , "+1364-2001ext+v"    -- Default to Verilog 2001
           , "+1800-2005ext+sv"   -- Default to SystemVerilog 2005
           , "--top"              -- This is used to set the C++ class names
           , top
           , "--cc"               -- Build for C++, not SystemC
           , "--build"            -- Build the verilated code immediately
           , "--exe"              -- Create an executable instead of a library
           ]
        <> srcs

    verilator workDir args =
      TestProgram "verilator" args NoGlob PrintNeither False (Just workDir)

    runVerilator workDir args =
      run optionSet (verilator workDir args) progressCallback

  testOptions = coerce (testOptions @TestProgram)

data VerilatorSimTest = VerilatorSimTest
  { vsExpectFailure :: Maybe (TestExitCode, Text)
  , vsStdoutNonEmptyFail :: Bool
  , vsDirectory :: IO FilePath
  , vsTop :: String
  }

instance IsTest VerilatorSimTest where
  run optionSet (VerilatorSimTest expectFail nonEmptyFail getDir top) progressCallback = do
    dir <- getDir

    -- Note [copy data files hack]
    lists <- glob (dir </> "*" </> "memory.list")
    forM_ lists $ \memFile ->
      copyFile memFile (dir </> "memory.list")

    let topExe = dir </> "obj_dir" </> ("V" <> top)

    case expectFail of
      Nothing -> run optionSet (verilated dir topExe) progressCallback
      Just exit -> run optionSet (failingVerilated dir topExe exit) progressCallback
   where
    verilated workDir exe =
      TestProgram exe [] NoGlob PrintNeither nonEmptyFail (Just workDir)

    failingVerilated workDir exe (exit, expectedErr) =
      TestFailingProgram (testExitCode exit) exe [] NoGlob PrintNeither False
        (specificExitCode exit) (ExpectEither expectedErr) (Just workDir)

  testOptions = coerce (testOptions @TestProgram)
