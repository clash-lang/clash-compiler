{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}

module Test.Tasty.Clash where

import           Clash.Annotations.Primitive (HDL(..))
import           Clash.Driver.Manifest     (readManifest, Manifest(..), manifestFilename)
import           Control.Monad             (foldM, forM_)
import           Data.Char                 (toLower)
import           Data.Coerce               (coerce)
import           Data.Default              (Default, def)
import qualified Data.List                 as List
import           Data.List                 (intercalate)
import           Data.Maybe                (isJust, fromMaybe)
import qualified Data.Text                 as T
import qualified System.Directory          as Directory
import           System.Directory          (createDirectory, listDirectory, copyFile)
import           System.FilePath           ((</>),(<.>), replaceFileName)
import           System.FilePath.Glob      (glob)
import           System.IO.Unsafe          (unsafePerformIO)
import           System.IO.Temp            (createTempDirectory, getCanonicalTemporaryDirectory)

import           Test.Tasty
import           Test.Tasty.Program
import           Test.Tasty.Providers
import           Test.Tasty.Runners

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

data TestExitCode
  = TestExitCode
  | TestSpecificExitCode Int
  | NoTestExitCode

instance Default TestExitCode where
  def = TestExitCode

testExitCode :: TestExitCode -> Bool
testExitCode TestExitCode = True
testExitCode (TestSpecificExitCode _) = True
testExitCode NoTestExitCode = False

specificExitCode :: TestExitCode -> Maybe Int
specificExitCode (TestSpecificExitCode n) = Just n
specificExitCode _ = Nothing

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

getManifests :: String -> IO [(FilePath, Manifest)]
getManifests pattern = mapM go =<< glob pattern
 where
  go :: FilePath -> IO (FilePath, Manifest)
  go path = do
    let err = error ("Failed to read/decode: " <> show path)
    manifest <- fromMaybe err <$> readManifest path
    pure (path, manifest)

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

    failingProgram oDir (testExit, expectedErr) = let
        -- TODO: there's no easy way to test for the absence of something in stderr
        expected = case T.splitAt 4 expectedErr of
                     ("NOT:", rest) -> ExpectNotStdErr rest
                     _ -> ExpectStdErr expectedErr
      in
      TestFailingProgram
        (testExitCode testExit) "clash" (args oDir) NoGlob PrintNeither False
        (specificExitCode testExit) expected Nothing

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

-- | Search through a directory with VHDL files produced by Clash
-- and produces /work/ files using @ghdl -i@ for each library.
--
-- For example, for I2C it would execute:
--
-- @
-- ghdl -i --work=bitMaster --workdir=bitMaster --std=93 <files>
-- ghdl -i --work=byteMaster --workdir=byteMaster --std=93 <files>
-- ghdl -i --work=i2c --workdir=i2c --std=93 <files>
-- @
--
-- After executing this test, $tmpDir/work contains a directory for each
-- top entity: @bitMaster@, @byteMaster@, @i2c@. A more typical test case might
-- produce @topEntity@ and @testBench@ instead.
--
data GhdlImportTest = GhdlImportTest
  { gitSourceDirectory :: IO FilePath
    -- ^ Directory containing VHDL files produced by Clash
  }

instance IsTest GhdlImportTest where
  run optionSet GhdlImportTest{gitSourceDirectory} progressCallback = do
    src <- gitSourceDirectory
    let workDir = src </> "work"
    createDirectory workDir
    manifests <- getManifests (src </> "*" </> manifestFilename)
    foldM (goManifest workDir) (testPassed "") manifests
   where
    stdArgs  = ["-i", "--std=93"]
    runGhdlI workDir args =
      run optionSet (ghdlI workDir args) progressCallback
    ghdlI workDir args =
      TestProgram "ghdl" (stdArgs <> args) NoGlob PrintNeither False (Just workDir)

    -- Read a manifest file, error if its malformed / inaccessible. Run @ghdl -i@
    -- on files associated with the component.
    goManifest :: FilePath -> Result -> (FilePath, Manifest) -> IO Result
    goManifest workDir result (manifestPath, Manifest{topComponent,fileNames})
      | resultSuccessful result = do
        let
          top = T.unpack topComponent
          relVhdlFiles = filter (".vhdl" `List.isSuffixOf`) (map fst fileNames)
          absVhdlFiles = map (replaceFileName manifestPath) relVhdlFiles
        createDirectory (workDir </> top)
        runGhdlI workDir (["--work=" <> top, "--workdir=" <> top] <> absVhdlFiles)

      | otherwise = pure result

  testOptions = coerce (testOptions @TestProgram)

-- | Create an executable given directory 'GhdlImportTest' produced work files
-- in.
--
-- For example, for I2C it would execute:
--
-- @
-- ghdl -m -fpsl --work=i2c --workdir=i2c -PbitMaster -PbyteMaster -Pi2c -o i2c_exe i2c
-- @
--
data GhdlMakeTest = GhdlMakeTest
  { gmtSourceDirectory :: IO FilePath
    -- ^ Directory containing VHDL files produced by Clash
  , gmtTop :: String
    -- ^ Entry point to be converted to executables
  }

instance IsTest GhdlMakeTest where
  run optionSet GhdlMakeTest{gmtSourceDirectory,gmtTop} progressCallback = do
    src <- gmtSourceDirectory
    let
      workDir = src </> "work"
    libs <- listDirectory workDir
    runGhdl workDir $
        ["-m", "-fpsl", "--work=" <> gmtTop, "--workdir=" <> gmtTop]
      <> ["-P" <> lib | lib <- libs]
      <> ["-o", map toLower (gmtTop <> "_exe"), gmtTop]
   where
    ghdl workDir args = TestProgram "ghdl" args NoGlob PrintNeither False (Just workDir)
    runGhdl workDir args = run optionSet (ghdl workDir args) progressCallback

  testOptions = coerce (testOptions @TestProgram)

-- | Run executable generated by 'GhdlMakeTest'.
--
-- For examples, for I2C it would execute:
--
-- @
-- ghdl -r --workdir=i2c --work=i2c i2c_exe --assert-level=error
-- @
--
data GhdlSimTest = GhdlSimTest
  { gstExpectFailure :: Maybe (TestExitCode, T.Text)
    -- ^ Expected failure code and output (if any)
  , gstSourceDirectory :: IO FilePath
    -- ^ Directory containing executables produced by 'GhdlMakeTest'
  , gstTop :: String
    -- ^ Entry point to be executed
  }

instance IsTest GhdlSimTest where
  run optionSet GhdlSimTest{..} progressCallback = do
    src <- gstSourceDirectory
    let workDir = src </> "work"

    -- See Note [copy data files hack]
    lists <- glob (src </> "*/memory.list")
    forM_ lists $ \memFile ->
      copyFile memFile (workDir </> "memory.list")

    case gstExpectFailure of
      Nothing -> run optionSet (program workDir gstTop) progressCallback
      Just exit -> run optionSet (failingProgram workDir gstTop exit) progressCallback
   where
    program workDir top =
      TestProgram "ghdl" (args top) NoGlob PrintNeither False (Just workDir)

    failingProgram workDir top (testExit, expectedErr) =
      TestFailingProgram
        (testExitCode testExit) "ghdl" (args top) NoGlob PrintNeither False
        (specificExitCode testExit) (ExpectEither expectedErr) (Just workDir)

    args work =
      [ "-r"
      , "--workdir=" <> work
      , "--work=" <> work
      , map toLower (work <> "_exe")
      , "--assert-level=error"
      ]

  testOptions = coerce (testOptions @TestProgram)

-- | Make executable from Verilog produced by Clash using Icarus Verilog.
--
-- For example, for I2C it would execute:
--
-- @
-- iverilog \
--   -I test_i2c -I test_bitmaster -I test_bytemaster \
--   -g2 -s test_i2c -o test_i2c.exe \
--   <verilog_files>
-- @
--
data IVerilogMakeTest = IVerilogMakeTest
  { ivmSourceDirectory :: IO FilePath
    -- ^ Directory containing VHDL files produced by Clash
  , ivmTop :: String
    -- ^ Entry point to be compiled
  }

instance IsTest IVerilogMakeTest where
  run optionSet IVerilogMakeTest{ivmSourceDirectory,ivmTop} progressCallback = do
    src <- ivmSourceDirectory
    libs <- listDirectory src
    verilogFiles <- glob (src </> "*" </> "*.v")
    runIcarus src (mkArgs libs verilogFiles ivmTop)
   where
    mkArgs libs files top =
         concat [["-I", l] | l <- libs]
      <> ["-g2", "-s", top, "-o", top <> ".exe"]
      <> files

    icarus workDir args = TestProgram "iverilog" args NoGlob PrintNeither False (Just workDir)
    runIcarus workDir args = run optionSet (icarus workDir args) progressCallback

  testOptions = coerce (testOptions @TestProgram)

-- | Run executable produced by 'IverilogMakeTest'.
--
-- For example, for I2C it would execute:
--
-- @
-- vvp test_i2c.exe
-- @
--
data IVerilogSimTest = IVerilogSimTest
  { ivsExpectFailure :: Maybe (TestExitCode, T.Text)
    -- ^ Expected failure code and output (if any)
  , ivsStderrEmptyFail :: Bool
    -- ^ Whether empty stderr means failure
  , ivsSourceDirectory :: IO FilePath
    -- ^ Directory containing executables produced by 'IVerilogMakeTest'
  , ivsTop :: String
    -- ^ Entry point to simulate
  }

instance IsTest IVerilogSimTest where
  run optionSet IVerilogSimTest{..} progressCallback = do
    src <- ivsSourceDirectory

    -- See Note [copy data files hack]
    lists <- glob (src </> "*/memory.list")
    forM_ lists $ \memFile ->
      copyFile memFile (src </> "memory.list")

    let topExe = ivsTop <> ".exe"
    case ivsExpectFailure of
      Nothing -> run optionSet (vvp src [topExe]) progressCallback
      Just exit -> run optionSet (failingVvp src [topExe] exit) progressCallback
   where
    vvp workDir args =
      TestProgram "vvp" args NoGlob PrintNeither ivsStderrEmptyFail (Just workDir)

    failingVvp workDir args (testExit, expectedErr) =
      TestFailingProgram
        (testExitCode testExit) "vvp" args NoGlob PrintNeither False
        (specificExitCode testExit) (ExpectEither expectedErr) (Just workDir)

  testOptions = coerce (testOptions @TestProgram)

data ModelsimVlibTest = ModelsimVlibTest
  { mvtSourceDirectory :: IO FilePath
    -- ^ Directory containing VHDL files produced by Clash
  }

instance IsTest ModelsimVlibTest where
  run optionSet ModelsimVlibTest{mvtSourceDirectory} progressCallback = do
    src <- mvtSourceDirectory
    runVlib src ["work"]
   where
    vlib workDir args = TestProgram "vlib" args NoGlob PrintNeither False (Just workDir)
    runVlib workDir args = run optionSet (vlib workDir args) progressCallback

  testOptions = coerce (testOptions @TestProgram)

data ModelsimVlogTest = ModelsimVlogTest
  { vlogSourceDirectory :: IO FilePath
    -- ^ Directory containing VHDL files produced by Clash
  }

instance IsTest ModelsimVlogTest where
  run optionSet ModelsimVlogTest{vlogSourceDirectory} progressCallback = do
    src <- vlogSourceDirectory
    typeFiles <- glob (src </> "*" </> "*_types.sv")
    allFiles <- glob (src </> "*" </> "*.sv")
    runVlog src (["-sv", "-work", "work"] <> typeFiles <> allFiles)
   where
    vlog workDir args = TestProgram "vlog" args NoGlob PrintNeither False (Just workDir)
    runVlog workDir args = run optionSet (vlog workDir args) progressCallback

  testOptions = coerce (testOptions @TestProgram)

data ModelsimSimTest = ModelsimSimTest
  { msimExpectFailure :: Maybe (TestExitCode, T.Text)
    -- ^ Expected failure code and output (if any)
  , msimSourceDirectory :: IO FilePath
    -- ^ Directory containing VHDL files produced by Clash
  , msimTop :: String
    -- ^ Entry point to simulate
  }

instance IsTest ModelsimSimTest where
  run optionSet ModelsimSimTest{..} progressCallback = do
    src <- msimSourceDirectory

    -- See Note [copy data files hack]
    lists <- glob (src </> "*/memory.list")
    forM_ lists $ \memFile ->
      copyFile memFile (src </> "memory.list")

    let args = ["-batch", "-do", doScript, msimTop]
    case msimExpectFailure of
      Nothing -> run optionSet (vsim src args) progressCallback
      Just exit -> run optionSet (failingVsim src args exit) progressCallback
   where
    vsim workDir args =
      TestProgram "vsim" args NoGlob PrintNeither False (Just workDir)

    failingVsim workDir args (testExit, expectedErr) =
      TestFailingProgram
        (testExitCode testExit) "vsim" args NoGlob PrintNeither False
        (specificExitCode testExit) (ExpectEither expectedErr) (Just workDir)

    doScript = List.intercalate ";"
      [ "run -all"
      , unwords
         ["if {[string equal ready [runStatus]]}"
         ,"then {quit -f}"
         ,"else {quit -code 1 -f}"
         ]
      , "quit -code 2 -f"
      ]

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

runTest1
  :: String
  -> TestOptions
  -> [String]
  -> HDL
  -> TestTree
runTest1 modName opts@TestOptions{..} path target =
  withResource mkTmpDir Directory.removeDirectoryRecursive $ \tmpDir -> do
    testGroup (show target) $ sequenceTests (show target:path) $ clashTest tmpDir :
      case target of
        VHDL -> buildAndSimTests (vhdlTests opts tmpDir)
        Verilog -> buildAndSimTests (verilogTests opts tmpDir)
        SystemVerilog-> buildAndSimTests (systemVerilogTests opts tmpDir)

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
  :: FilePath
  -- ^ Work directory
  -> HDL
  -- ^ Build target
  -> [String]
  -- ^ Extra Clash arguments
  -> [String]
  -- ^ Extra GHC arguments
  -> String
  -- ^ Module name
  -> String
  -- ^ Base function name
  -> [TestName]
  -- ^ Parent test names in order of distance to the test. That is, the last
  -- item in the list will be the root node, while the first one will be the
  -- one closest to the test.
  -> TestTree
outputTest' env target extraClashArgs extraGhcArgs modName funcName path =
  withResource acquire tastyRelease (const seqTests)
    where
      -- TODO: Run these tests in their own temporary directory
      path' = show target:path
      acquire = tastyAcquire path' [modName]
      out = testDirectory path' </> modName </> "out"

      args =
        [ "-DCLASHLIBTEST"
        , "-package", "clash-testsuite"
        , "-main-is", modName ++ "." ++ funcName ++ show target
        , "-o", out
        , "-outputdir", testDirectory path' </> modName
        ] ++ extraGhcArgs ++ [env </> modName <.> "hs"]

      hdlTest = ("clash", singleTest "clash" (ClashTest {
          ctExpectFailure=Nothing
        , ctBuildTarget=target
        , ctSourceDirectory=sourceDirectory </> env
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
  :: FilePath
  -- ^ Work directory
  -> [HDL]
  -- ^ Build targets
  -> [String]
  -- ^ Extra clash arguments
  -> [String]
  -- ^ Extra GHC arguments
  -> String
  -- ^ Module name
  -> String
  -- ^ Base function name
  -> [TestName]
  -- ^ Parent test names in order of distance to the test. That is, the last
  -- item in the list will be the root node, while the first one will be the
  -- one closest to the test.
  -> TestTree
outputTest env targets extraClashArgs extraGhcArgs modName funcName path =
  let testName = modName ++ " [output test]" in
  let path' = testName : path in
  testGroup testName
    [outputTest' env target extraClashArgs extraGhcArgs modName funcName path' | target <- targets]

clashLibTest'
  :: FilePath
  -- ^ Work directory
  -> HDL
  -- ^ Build target
  -> [String]
  -- ^ Extra GHC arguments
  -> String
  -- ^ Module name
  -> String
  -- ^ Base function name
  -> [TestName]
  -- ^ Parent test names in order of distance to the test. That is, the last
  -- item in the list will be the root node, while the first one will be the
  -- one closest to the test.
  -> TestTree
clashLibTest' env target extraGhcArgs modName funcName path =
  withResource acquire tastyRelease (const seqTests)
    where
      -- TODO: Run these tests in their own temporary directory
      path' = show target:path
      acquire = tastyAcquire path' [modName]
      out = testDirectory path' </> modName </> "out"

      args =
        [ "-DCLASHLIBTEST"
        , "-package", "clash-testsuite"
        , "-main-is", modName ++ "." ++ funcName ++ show target
        , "-o", out
        , "-outputdir", testDirectory path' </> modName
        ] ++ extraGhcArgs ++ [env </> modName <.> "hs"]

      seqTests = testGroup (show target) $ sequenceTests path' $
        [ ( "[lib] clash"
          , testProgram "[lib] clash" "clash" args NoGlob PrintStdErr False Nothing )
        , ("exec", testProgram "exec" out [] NoGlob PrintStdErr False Nothing) ]

clashLibTest
  :: FilePath
  -- ^ Work directory
  -> [HDL]
  -- ^ Build targets
  -> [String]
  -- ^ Extra GHC arguments
  -> String
  -- ^ Module name
  -> String
  -- ^ Base function name
  -> [TestName]
  -- ^ Parent test names in order of distance to the test. That is, the last
  -- item in the list will be the root node, while the first one will be the
  -- one closest to the test.
  -> TestTree
clashLibTest env targets extraGhcArgs modName funcName path =
  -- HACK: clashLibTests are run sequentially to prevent race issues. See:
  -- HACK: https://github.com/clash-lang/clash-compiler/pull/1416
  testGroup testName $ sequenceTests path'
    [(show target, runLibTest target) | target <- targets]
 where
  runLibTest target = clashLibTest' env target extraGhcArgs modName funcName path'
  testName = modName ++ " [clash-lib test]"
  path' = testName : path
