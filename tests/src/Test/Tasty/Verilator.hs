{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

module Test.Tasty.Verilator where

import Control.Monad (filterM, forM_)
import Data.Coerce (coerce)
import Data.Proxy
import Data.Tagged
import Data.Text (Text)
import System.Directory
  (copyFile, doesDirectoryExist, findExecutable, listDirectory)
import System.Environment (lookupEnv)
import System.FilePath ((</>), takeFileName)
import System.FilePath.Glob (glob)
import System.Info (os)
import System.IO.Temp (getCanonicalTemporaryDirectory)

import Test.Tasty.Common
import Test.Tasty.Options
import Test.Tasty.Program
import Test.Tasty.Providers

-- | @--verilator@ flag for enabling tests that use verilator.
newtype Verilator = Verilator Bool
  deriving (Eq, Ord)

instance IsOption Verilator where
  defaultValue = Verilator True
  parseValue = fmap Verilator . safeReadBool
  optionName = pure "no-verilator"
  optionHelp = pure "Skip verilator tests"
  optionCLParser = flagCLParser Nothing (Verilator False)

data VerilatorMakeTest = VerilatorMakeTest
  { vmParentDirectory :: IO FilePath
  , vmDirectory :: IO FilePath
  , vmTop :: String
  }

instance IsTest VerilatorMakeTest where
  run optionSet VerilatorMakeTest{..} progressCallback
    | Verilator True <- lookupOption optionSet = do
        buildTargetDir vmParentDirectory vmDirectory
        dir <- vmDirectory
        libs <- listDirectory dir >>= filterM doesDirectoryExist . fmap (dir </>)

        vSrc <- fmap takeFileName <$> glob (dir </> "*" </> vmTop <> ".v")

        -- Types modules have to be given first, or verilator will complain that
        -- they are not already declared when it sees them being imported.
        svSrc <- mappend
          <$> (fmap takeFileName <$> glob (dir </> "*" </> "*_types.sv"))
          <*> (fmap takeFileName <$> glob (dir </> "*" </> vmTop <> ".sv"))

        -- Clash by default will not mix HDLs in it's output. If this ever changes,
        -- and it is possible to have `clash` output both Verilog and SystemVerilog
        -- then this will need to change.
        objectCacheEnv <- verilatorObjectCacheEnv
        runVerilator dir objectCacheEnv (mkArgs libs (vSrc <> svSrc))

    | otherwise =
        pure (testPassed "Ignoring test due to --no-verilator")
   where
    mkArgs libs srcs =
      ["-I" <> lib | lib <- libs]
        <> [ "-Wno-fatal"         -- Do not abort on warnings
           , "-Wall"
           -- https://veripool.org/guide/latest/faq.html#why-do-i-get-undefined-reference-to-sc-time-stamp
           , "-CFLAGS", "-DVL_TIME_CONTEXT"
           -- Test benches simulate for microseconds at most, so optimizing the
           -- generated C++ buys nothing at run time but costs compile time.
           , "-CFLAGS", "-O0"
           , "+1364-2001ext+v"    -- Default to Verilog 2001
           , "+1800-2005ext+sv"   -- Default to SystemVerilog 2005
           , "--top"              -- This is used to set the C++ class names
           , vmTop
           , "--cc"               -- Build for C++, not SystemC
           , "--build"            -- Build the verilated code immediately
           , "--binary"           -- Create an binary to execute
           ]
        <> srcs

    verilator workDir env args =
      let program = case os of {"mingw32" -> "verilator_bin"; _ -> "verilator"}
       in TestProgram program args NoGlob PrintNeither False (Just workDir) env

    runVerilator workDir env args =
      run optionSet (verilator workDir env args) progressCallback

  testOptions =
    coerce (coerce (testOptions @TestProgram) <> [Option (Proxy @Verilator)])

{- Note [Verilator object cache]

Verilator compiles its runtime library (verilated.cpp, verilated_timing.cpp,
verilated_threads.cpp) from scratch for every model it builds. Those files are
identical for every test, yet compiling them is roughly 85% of the time spent
in a test's Verilator build; the model itself and the simulation are a small
remainder. Verilator's makefile prefixes every compiler invocation with the
@OBJCACHE@ make variable, which it also reads from the environment. Pointing
it at @ccache@ makes the first Verilator test in a run pay for the runtime and
every later test hit the cache.

ccache stores its cache under @$HOME@ by default. That directory is not
writable in some sandboxes (e.g. a nix build), so unless the user picked a
location we default to a stable directory in the system temporary directory,
which also lets consecutive test suite runs share the cache.
-}

-- | Environment variables that make Verilator use @ccache@ as its object cache,
-- if @ccache@ is available. See Note [Verilator object cache].
verilatorObjectCacheEnv :: IO [(String, String)]
verilatorObjectCacheEnv = do
  ccache <- findExecutable "ccache"
  case ccache of
    Nothing -> pure []
    Just _ -> do
      userCacheDir <- lookupEnv "CCACHE_DIR"
      cacheDirEnv <- case userCacheDir of
        Just _ -> pure []
        Nothing -> do
          tmpDir <- getCanonicalTemporaryDirectory
          pure [("CCACHE_DIR", tmpDir </> "clash-testsuite-ccache")]
      pure (("OBJCACHE", "ccache") : cacheDirEnv)

data VerilatorSimTest = VerilatorSimTest
  { vsExpectFailure :: Maybe (TestExitCode, Text)
  , vsStdoutNonEmptyFail :: Bool
  , vsDirectory :: IO FilePath
  , vsTop :: String
  }

instance IsTest VerilatorSimTest where
  run optionSet (VerilatorSimTest expectFail nonEmptyFail getDir top) progressCallback
    | Verilator True <- lookupOption optionSet = do
        dir <- getDir

        -- Note [copy data files hack]
        lists <- glob (dir </> "*" </> "memory.list")
        forM_ lists $ \memFile ->
          copyFile memFile (dir </> "memory.list")

        let topExe = dir </> "obj_dir" </> ("V" <> top)

        case expectFail of
          Nothing -> run optionSet (verilated dir topExe) progressCallback
          Just exit -> run optionSet (failingVerilated dir topExe exit) progressCallback

    | otherwise =
        pure (testPassed "Ignoring test due to --no-verilator")
   where
    verilated workDir exe =
      TestProgram exe [] NoGlob PrintNeither nonEmptyFail (Just workDir) []

    failingVerilated workDir exe (exit, expectedErr) =
      TestFailingProgram (testExitCode exit) exe [] NoGlob PrintNeither False
        (specificExitCode exit) (ExpectEither expectedErr) (Just workDir) []

  testOptions =
    coerce (coerce (testOptions @TestProgram) <> [Option (Proxy @Verilator)])
