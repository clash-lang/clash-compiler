{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

module Test.Tasty.Clash where

import           Clash.Annotations.Primitive (HDL(..))
import           Data.Coerce               (coerce)
import           Data.Default              (Default, def)
import qualified Data.List                 as List
import           Data.List                 (intercalate)
import           Data.Maybe                (isJust)
import qualified Data.Text                 as T
import qualified System.Directory          as Directory
import           System.FilePath           ((</>),(<.>))
import           System.IO.Unsafe          (unsafePerformIO)
import           System.IO.Temp            (createTempDirectory, getCanonicalTemporaryDirectory)

import           Test.Tasty
import           Test.Tasty.Program
import           Test.Tasty.Providers

import           Test.Tasty.Common
import           Test.Tasty.Ghdl
import           Test.Tasty.Iverilog
import           Test.Tasty.Modelsim
import           Test.Tasty.SymbiYosys

{- Note [copy data files hack]

Clash currently creates a directory for every top entity it synthesizes. For
example, two top entities 'foo' and 'bar' in module FooBar would be synthesized
in the folders:

  $hdldir/FooBar.foo
  $hdldir/FooBar.bar

Any data files used in 'foo' would be copied to its directory, while any data
files used by 'bar' would be copied to  _its_ directory. For example, if 'foo'
and 'bar' would both instantiate a 'blockRamFile' with 'memory_foo.list' and
'memory_bar.list' respectively, the following files would exist after synthesizing:

  $hdldir/FooBar.foo/foo.vhdl
  $hdldir/FooBar.foo/memory_foo.list
  $hdldir/FooBar.bar/bar.vhdl
  $hdldir/FooBar.bar/memory_bar.list

The HDL files (`foo.vhdl` and `bar.vhdl`) would refer to those files relative
to it, for example just "memory_foo.list". Some tools look for these
files relative to the HDL files (most synthesis tools), while others will try to
find them in whatever directory they're executed in (most simulators).

The "hack" in this case refers to copying the files from the HDL directory to
the directory the simulator is run from.

Note: the sensible thing for Clash would be to use relative paths in the HDL
      files as such: `../FooBar.foo/memory_foo.list`. This would satisfy both
      tooling looking relative to HDL files, and tooling run from a sibling
      directory.
-}

data SBuildTarget (target :: HDL) where
  SVHDL          :: SBuildTarget 'VHDL
  SVerilog       :: SBuildTarget 'Verilog
  SSystemVerilog :: SBuildTarget 'SystemVerilog

buildTargetToHdl :: SBuildTarget target -> HDL
buildTargetToHdl SVHDL = VHDL
buildTargetToHdl SVerilog = Verilog
buildTargetToHdl SSystemVerilog = SystemVerilog

-- | Targets to simulate. Defaults to @testBench@.
data BuildTargets
  = BuildAuto
  | BuildSpecific [String]

getBuildTargets :: TestOptions -> [String]
getBuildTargets (TestOptions{buildTargets=BuildSpecific targets}) = targets
getBuildTargets TestOptions{hdlSim, buildTargets=BuildAuto}
  -- We could be clever by adding test bench info to manifest files and
  -- using that in this testsuite
  | hdlSim = ["testBench"]
  | otherwise = ["topEntity"]

-- | Possible verification tools
data VerificationTool = SymbiYosys

data TestOptions =
  TestOptions
    { hdlSim :: Bool
    -- ^ Run hdl simulators (GHDL, ModelSim, etc.)
    , hdlLoad :: Bool
    -- ^ Load hdl into simulator (GHDL, ModelSim, etc.). Disabling this will
    -- disable 'hdlSim' too.
    , expectSimFail :: Maybe (TestExitCode, T.Text)
    -- ^ Expect simulation to fail: Nothing if simulation is expected to run
    -- without errors, or Just (part of) the error message the simulation is
    -- expected to throw.
    , verificationTool :: Maybe VerificationTool
    -- ^ A formal verification tool with which to attempt to verify
    -- the generated hdl
    , expectVerificationFail :: Maybe (TestExitCode, T.Text)
    -- ^ Expect the verifier to fail: Nothing if verification should succeed,
    -- or Just (part of) the error message Clash is expected to throw.
    , expectClashFail :: Maybe (TestExitCode, T.Text)
    -- ^ Expect Clash to fail: Nothing if Clash is expected to compile without
    -- errors, or Just (part of) the error message Clash is expected to throw.
    , hdlTargets :: [HDL]
    -- ^ Run tests for these targets
    , clashFlags :: [String]
    -- ^ Extra flags to pass to Clash
    , buildTargets :: BuildTargets
    -- ^ Indicates what should be built to an executable. Defaults to @["testBench"]@
    -- if 'hdlSim' is set, otherwise @["topEntity"]@.
    , vvpStderrEmptyFail :: Bool
    -- ^ Whether an empty stderr means test failure when running VVP
    }

allTargets :: [HDL]
allTargets = [VHDL, Verilog, SystemVerilog]

instance Default TestOptions where
  def =
    TestOptions
      { hdlSim=True
      , hdlLoad=True
      , expectClashFail=Nothing
      , expectSimFail=Nothing
      , expectVerificationFail=Nothing
      , verificationTool=Nothing
      , hdlTargets=allTargets
      , clashFlags=[]
      , buildTargets=BuildAuto
      , vvpStderrEmptyFail=True
      }

-- | Single directory for this test run. All tests are run relative to this
-- directory. This does require all test names to be unique, which is checked
-- in Main.hs.
temporaryDirectory :: String
temporaryDirectory = unsafePerformIO $ do
  cwd     <- Directory.getCurrentDirectory
  let tmpDir = cwd </> ".clash-test-tmp"
  Directory.createDirectoryIfMissing True tmpDir
  tmpDir' <- createTempDirectory tmpDir "clash-test-"
  return tmpDir'
{-# NOINLINE temporaryDirectory #-}

-- | Given the module name of test, provide the test directory it is running in
testDirectory
  :: [TestName]
  -- ^ Path of test
  -> FilePath
  -- ^ Test directory
testDirectory path =
  foldl (</>) temporaryDirectory (reverse path)
{-# NOINLINE testDirectory #-}

-- | Directory where testbenches live.
sourceDirectory :: String
sourceDirectory =
  -- TODO: Allow testsuite to be run from any directory
  unsafePerformIO Directory.getCurrentDirectory
{-# NOINLINE sourceDirectory #-}

-- | Gather all files with specific extension
hdlFiles
  :: String
  -- ^ Extension
  -> FilePath
  -- ^ Directory to search
  -> FilePath
  -- ^ Subdirectory to search
  -> IO [FilePath]
  -- ^ Files with subdirectory as prefix
hdlFiles ext dir subdir = do
  allFiles <- Directory.getDirectoryContents (dir </> subdir)
  return $ map (subdir </>) (filter (List.isSuffixOf ext) allFiles)
{-# NOINLINE hdlFiles #-}

-- | Called before running VHDL/Verilog/SystemVerilog test group. Creates
-- necessary subdirectories to run tests in.
tastyAcquire
  :: [TestName]
  -- ^ Path of test
  -> [FilePath]
  -- ^ Subdirectories to create
  -> IO FilePath
  -- ^ New test directory
tastyAcquire path subdirs = do
  let tdir     = testDirectory path
  let subdirs' = map (tdir </>) subdirs
  _ <- mapM (Directory.createDirectoryIfMissing True) (subdirs')
  return tdir

-- | Called after running VHDL/Verilog/System test group. Removes compiled files.
tastyRelease
  :: FilePath
  -> IO ()
tastyRelease path = do
  Directory.removeDirectoryRecursive path

-- | Given a number of test trees, make sure each one of them is executed
-- one after the other. To prevent naming collisions, parent group names can
-- be included. Parent group names should be ordered outer -> inner.
sequenceTests
  :: [TestName]
  -- ^ Parent group names
  -> [(TestName, TestTree)]
  -- ^ Tests to sequence
  -> [TestTree]
sequenceTests path (unzip -> (testNames, testTrees)) =
  zipWith applyAfter testPatterns testTrees
    where
      -- Make pattern for a single test
      pat :: TestName -> String
      pat nm = "$0 ~ /" ++ intercalate "." (reverse (nm:path)) ++ "/"

      -- Test patterns for all given tests such that each executes sequentially
      testPatterns = init (map (fmap pat) (Nothing : map Just testNames))

      -- | Generate pattenrs given parent patterns and item patterns
      applyAfter :: Maybe String -> TestTree -> TestTree
      applyAfter Nothing  tt = tt
      applyAfter (Just p) tt = after AllSucceed p tt

data ClashTest = ClashTest
  { ctExpectFailure :: Maybe (TestExitCode, T.Text)
    -- ^ Expected failure code and output (if any)
  , ctBuildTarget :: HDL
  , ctSourceDirectory :: FilePath
  , ctExtraArgs :: [String]
  , ctModName :: String
  , ctOutputDirectory :: IO FilePath
  }

instance IsTest ClashTest where
  run optionSet ClashTest{..} progressCallback = do
    oDir <- ctOutputDirectory
    case ctExpectFailure of
      Nothing -> run optionSet (program oDir) progressCallback
      Just exit -> run optionSet (failingProgram oDir exit) progressCallback
   where
    program oDir =
      TestProgram "clash" (args oDir) NoGlob PrintNeither False Nothing

    failingProgram oDir (testExit, expectedErr) =
      TestFailingProgram
        (testExitCode testExit) "clash" (args oDir) NoGlob PrintNeither False
        (specificExitCode testExit) (ExpectStdErr expectedErr) Nothing

    args oDir =
      [ target
      , "-i" <> ctSourceDirectory
      , ctModName
      , "-fclash-hdldir", oDir
      , "-odir", oDir
      , "-hidir", oDir
      , "-fclash-debug", "DebugSilent"
      ] <> ctExtraArgs

    target =
      case ctBuildTarget of
        VHDL          -> "--vhdl"
        Verilog       -> "--verilog"
        SystemVerilog -> "--systemverilog"

  testOptions = coerce (testOptions @TestProgram)

-- | Generate two test trees for testing VHDL: one for building designs and one
-- for running them. Depending on 'hdlSim' the latter will be executed or not.
vhdlTests
  :: TestOptions
  -> IO FilePath
  -> ( [(TestName, TestTree)] -- build tests
     , [(TestName, TestTree)] -- simulation tests
     )
vhdlTests opts@TestOptions{..} tmpDir = (buildTests, simTests)
 where
  importName = "GHDL (import)"
  makeName t = "GHDL (make " <> t <> ")"
  buildTests = concat
    [ [ (importName, singleTest importName (GhdlImportTest tmpDir)) ]
    , [ (makeName t, singleTest (makeName t) (GhdlMakeTest tmpDir t))
      | t <- getBuildTargets opts ]
    ]

  simName t = "GHDL (sim " <> t <> ")"
  simTests =
    [ (simName t, singleTest (simName t) (GhdlSimTest expectSimFail tmpDir t))
    | t <- getBuildTargets opts
    ]

-- | Generate two test trees for testing Verilog: one for building designs and one
-- for running them. Depending on 'hdlSim' the latter will be executed or not.
verilogTests
  :: TestOptions
  -> IO FilePath
  -> ( [(TestName, TestTree)] -- build tests
     , [(TestName, TestTree)] -- simulation tests
     )
verilogTests opts@TestOptions{..} tmpDir = (buildTests, simTests)
 where
  makeName t = "iverilog (make " <> t <> ")"
  buildTests =
    [ (makeName t, singleTest (makeName t) (IVerilogMakeTest tmpDir t))
    | t <- getBuildTargets opts ]

  simName t = "iverilog (sim " <> t <> ")"
  simTests =
    [ (simName t, singleTest (simName t) (IVerilogSimTest expectSimFail vvpStderrEmptyFail tmpDir t))
    | t <- getBuildTargets opts ]

-- | Generate two test trees for testing SystemVerilog: one for building designs and
-- one for running them. Depending on 'hdlSim' the latter will be executed or not.
systemVerilogTests
  :: TestOptions
  -> IO FilePath
  -> ( [(TestName, TestTree)] -- build tests
     , [(TestName, TestTree)] -- simulation tests
     )
systemVerilogTests opts@TestOptions{..} tmpDir = (buildTests, simTests)
 where
  vlibName = "modelsim (vlib)"
  vlogName = "modelsim (vlog)"
  buildTests =
    [ (vlibName, singleTest vlibName (ModelsimVlibTest tmpDir))
    , (vlogName, singleTest vlogName (ModelsimVlogTest tmpDir))
    ]

  simName t = "modelsim (sim " <> t <> ")"
  simTests =
    [ (simName t, singleTest (simName t) (ModelsimSimTest expectSimFail tmpDir t))
    | t <- getBuildTargets opts ]

-- | Generate a test tree for running SymbiYosys
sbyTests :: TestOptions -> IO FilePath -> ([(TestName, TestTree)])
sbyTests opts@TestOptions {..} tmpDir =
  [ ( "SymbiYosys"
    , singleTest "SymbiYosys"
                 (SbyVerificationTest expectVerificationFail tmpDir t)
    )
  | t <- getBuildTargets opts
  ]

runTest1
  :: String
  -> TestOptions
  -> [TestName]
  -> HDL
  -> TestTree
runTest1 modName opts@TestOptions{..} path target =
  withResource mkTmpDir Directory.removeDirectoryRecursive $ \tmpDir -> do
    testGroup (show target) $ sequenceTests (show target:path) $ clashTest tmpDir :
      (case target of
        VHDL -> buildAndSimTests (vhdlTests opts tmpDir)
        Verilog -> buildAndSimTests (verilogTests opts tmpDir)
        SystemVerilog-> buildAndSimTests (systemVerilogTests opts tmpDir)
      ) <>
      (case verificationTool of
         Nothing -> []
         Just SymbiYosys -> sbyTests opts tmpDir
      )

 where
  mkTmpDir = flip createTempDirectory "clash-test" =<< getCanonicalTemporaryDirectory
  sourceDir = foldl (</>) sourceDirectory (reverse (tail path))

  clashTest tmpDir =
    ("clash", singleTest "clash" (ClashTest {
      ctExpectFailure=expectClashFail
    , ctBuildTarget=target
    , ctSourceDirectory=sourceDir
    , ctExtraArgs=clashFlags
    , ctModName=modName
    , ctOutputDirectory=tmpDir
    }))

  buildAndSimTests (buildTests, simTests) =
    case (isJust expectClashFail, hdlLoad, hdlSim) of
      (True, _, _) -> []
      (_, False, _) -> []
      (_, _, False) -> buildTests
      _ -> buildTests <> simTests

runTest
  :: String
  -- ^ Name of test
  -> TestOptions
  -> [TestName]
  -- ^ Parent test names in order of distance to the test. That is, the last
  -- item in the list will be the root node, while the first one will be the
  -- one closest to the test. Should correspond to directories on filesystem.
  -> TestTree
runTest modName opts path =
  testGroup modName (map (runTest1 modName opts (modName:path)) (hdlTargets opts))

outputTest'
  :: String
  -- ^ Module name
  -> HDL
  -- ^ Build target
  -> [String]
  -- ^ Extra Clash arguments
  -> [String]
  -- ^ Extra GHC arguments
  -> [TestName]
  -- ^ Parent test names in order of distance to the test. That is, the last
  -- item in the list will be the root node, while the first one will be the
  -- one closest to the test.
  -> TestTree
outputTest' modName target extraClashArgs extraGhcArgs path =
  withResource acquire tastyRelease (const seqTests)
    where
      -- TODO: Run these tests in their own temporary directory
      path' = show target:path
      sourceDir = foldl (</>) sourceDirectory (reverse (tail path))
      acquire = tastyAcquire path' [modName]
      out = testDirectory path' </> modName </> "out"

      args =
        [ "-DCLASHLIBTEST"
        , "-package", "clash-testsuite"
        , "-main-is", modName ++ ".main" ++ show target
        , "-o", out
        , "-outputdir", testDirectory path' </> modName
        ] ++ extraGhcArgs ++ [sourceDir </> modName <.> "hs"]

      hdlTest = ("clash", singleTest "clash" (ClashTest {
          ctExpectFailure=Nothing
        , ctBuildTarget=target
        , ctSourceDirectory=sourceDir
        , ctExtraArgs="-DCLASHLIBTEST" : extraClashArgs
        , ctModName=modName
        , ctOutputDirectory=pure workDir
        }))

      workDir = testDirectory path'

      seqTests = testGroup (show target) $ sequenceTests path' $
        [ hdlTest
        , ( "[out] clash"
          , testProgram "[out] clash" "clash" args NoGlob PrintStdErr False Nothing )
        , ( "exec"
          , testProgram "exec" out [workDir] NoGlob PrintStdErr False Nothing ) ]

outputTest
  :: String
  -- ^ Module name
  -> [HDL]
  -- ^ Build targets
  -> [String]
  -- ^ Extra clash arguments
  -> [String]
  -- ^ Extra GHC arguments
  -> [TestName]
  -- ^ Parent test names in order of distance to the test. That is, the last
  -- item in the list will be the root node, while the first one will be the
  -- one closest to the test.
  -> TestTree
outputTest modName targets extraClashArgs extraGhcArgs path =
  let testName = modName ++ " [output test]"
   in testGroup testName
        [ outputTest' modName target extraClashArgs extraGhcArgs (testName:path)
        | target <- targets
        ]

clashLibTest'
  :: String
  -- ^ Module name
  -> HDL
  -- ^ Build target
  -> [String]
  -- ^ Extra GHC arguments
  -> [TestName]
  -- ^ Parent test names in order of distance to the test. That is, the last
  -- item in the list will be the root node, while the first one will be the
  -- one closest to the test.
  -> TestTree
clashLibTest' modName target extraGhcArgs path =
  withResource acquire tastyRelease (const seqTests)
    where
      -- TODO: Run these tests in their own temporary directory
      path' = show target:path
      sourceDir = foldl (</>) sourceDirectory (reverse (tail path))
      acquire = tastyAcquire path' [modName]
      out = testDirectory path' </> modName </> "out"

      args =
        [ "-DCLASHLIBTEST"
        , "-package", "clash-testsuite"
        , "-main-is", modName ++ ".main" ++ show target
        , "-o", out
        , "-outputdir", testDirectory path' </> modName
        ] ++ extraGhcArgs ++ [sourceDir </> modName <.> "hs"]

      seqTests = testGroup (show target) $ sequenceTests path' $
        [ ( "[lib] clash"
          , testProgram "[lib] clash" "clash" args NoGlob PrintStdErr False Nothing )
        , ("exec", testProgram "exec" out [] NoGlob PrintStdErr False Nothing) ]

clashLibTest
  :: String
  -- ^ Module name
  -> [HDL]
  -- ^ Build targets
  -> [String]
  -- ^ Extra GHC arguments
  -> [TestName]
  -- ^ Parent test names in order of distance to the test. That is, the last
  -- item in the list will be the root node, while the first one will be the
  -- one closest to the test.
  -> TestTree
clashLibTest modName targets extraGhcArgs path =
  -- HACK: clashLibTests are run sequentially to prevent race issues. See:
  -- HACK: https://github.com/clash-lang/clash-compiler/pull/1416
  let testName = modName ++ " [clash-lib test]"
   in testGroup testName $ sequenceTests (testName:path)
        [ (show target, clashLibTest' modName target extraGhcArgs (testName:path))
        | target <- targets
        ]
