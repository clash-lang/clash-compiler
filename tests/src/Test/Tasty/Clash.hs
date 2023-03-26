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
import           Test.Tasty.Runners

import           Test.Tasty.Common
import           Test.Tasty.Ghdl
import           Test.Tasty.Iverilog
import           Test.Tasty.Modelsim hiding (ModelSim)
import           Test.Tasty.SymbiYosys
import           Test.Tasty.Verilator hiding (Verilator)
import           Test.Tasty.Vivado hiding (Vivado)

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
  | not (null hdlSim) = ["testBench"]
  | otherwise = ["topEntity"]

-- | Possible verification tools
data VerificationTool = SymbiYosys

data Sim = Vivado | GHDL | IVerilog | ModelSim | Verilator
  deriving (Show, Eq, Bounded, Enum)

data TestOptions =
  TestOptions
    { hdlSim :: [Sim]
    -- ^ Run hdl simulators (GHDL, ModelSim, etc.)
    , hdlLoad :: [Sim]
    -- ^ Load hdl into simulators (GHDL, ModelSim, etc.). All simulators except
    -- Vivado need to be listed in `hdlLoad` for `hdlSim` to be able to run. For
    -- Vivado, listing it here is a no-op.
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
    , ghcFlags :: [String]
    -- ^ Extra flags to pass to GHC
    , clashFlags :: [String]
    -- ^ Extra flags to pass to Clash
    , buildTargets :: BuildTargets
    -- ^ Indicates what should be built to an executable. Defaults to @["testBench"]@
    -- if 'hdlSim' is set, otherwise @["topEntity"]@.
    , vvpStdoutNonEmptyFail :: Bool
    -- ^ Whether a non-empty stdout means test failure when running VVP
    }

allTargets :: [HDL]
allTargets = [VHDL, Verilog, SystemVerilog]

instance Default TestOptions where
  def =
    TestOptions
      { hdlSim=[minBound ..]
      , hdlLoad=[minBound ..]
      , expectClashFail=Nothing
      , expectSimFail=Nothing
      , expectVerificationFail=Nothing
      , verificationTool=Nothing
      , hdlTargets=allTargets
      , ghcFlags=[]
      , clashFlags=[]
      , buildTargets=BuildAuto
      , vvpStdoutNonEmptyFail=True
      }

-- | Directory where testbenches live.
sourceDirectory :: String
sourceDirectory =
  -- TODO: Allow testsuite to be run from any directory. This is easy from the
  -- clash-testsuite side, as we can add
  --
  --     data-files:
  --       shouldfail/**/*.hs
  --       shouldwork/**/*.hs
  --
  -- to the cabal file, then use Paths_clash_testsuite to get access to the
  -- data directory / data files. However, invoking clash for building in the
  -- testsuite requires the location of primitives to be known and accessible
  -- by clash (clash will automatically search in clash-lib/prims, which is
  -- why running from the project root works). Getting these would require
  -- exporting Paths_clash_lib from clash-lib, which might be grimy or not
  -- possible when using stack instead of cabal. Someone should investigate
  unsafePerformIO Directory.getCurrentDirectory
{-# NOINLINE sourceDirectory #-}

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

      -- | Generate patterns given parent patterns and item patterns
      applyAfter :: Maybe String -> TestTree -> TestTree
      applyAfter Nothing  tt = tt
      applyAfter (Just p) tt = after AllSucceed p tt

targetTempPath
  :: IO FilePath
  -- ^ Parent temporary directory
  -> String
  -- ^ HDL tool
  -> String
  -- ^ Build target
  -> IO FilePath
targetTempPath parent tool target =
  fmap (</> (tool <> "-" <> target)) parent

stepName
  :: String
  -- ^ Tool name
  -> String
  -- ^ Step name
  -> String
  -- ^ Build target
  -> String
stepName tool step target = tool <> " (" <> step <> " " <> target <> ")"

-- | Simulation test for a specific build target
--
-- Simulation tests usually consist of two test trees: one for building designs
-- and one for running them. It depends on 'hdlLoad' and 'hdlSim' what will be
-- executed.
data TestTarget = TestTarget
  { buildTests :: [(TestName, TestTree)]
  , simTests :: [(TestName, TestTree)]
  }

data ClashGenTest = ClashGenTest
  { cgExpectFailure :: Maybe (TestExitCode, T.Text)
    -- ^ Expected failure code and output (if any)
  , cgBuildTarget :: HDL
  , cgSourceDirectory :: FilePath
  , cgExtraArgs :: [String]
  , cgModName :: String
  , cgOutputDirectory :: IO FilePath
  , cgHdlDirectory :: IO FilePath
  }

instance IsTest ClashGenTest where
  run optionSet ClashGenTest{..} progressCallback = do
    oDir <- cgOutputDirectory
    hdlDir <- cgHdlDirectory
    case cgExpectFailure of
      Nothing ->
        run optionSet (program oDir hdlDir) progressCallback
      Just exit ->
        run optionSet (failingProgram oDir hdlDir exit) progressCallback
   where
    program oDir hdlDir =
      TestProgram
        "clash" (args oDir hdlDir) NoGlob PrintNeither False Nothing []

    failingProgram oDir hdlDir (testExit, expectedErr) = let
        -- TODO: there's no easy way to test for the absence of something in stderr
        expected = case T.splitAt 4 expectedErr of
                     ("NOT:", rest) -> ExpectNotStdErr rest
                     _ -> ExpectStdErr expectedErr
      in
      TestFailingProgram
        (testExitCode testExit) "clash" (args oDir hdlDir) NoGlob PrintNeither
        False (specificExitCode testExit) expected Nothing []

    args oDir hdlDir =
      [ target
      , "-i" <> cgSourceDirectory
      , cgModName
      , "-fclash-hdldir", hdlDir
      , "-odir", oDir
      , "-hidir", oDir
      , "-fclash-debug", "DebugSilent"
      ] <> cgExtraArgs

    target =
      case cgBuildTarget of
        VHDL          -> "--vhdl"
        Verilog       -> "--verilog"
        SystemVerilog -> "--systemverilog"

  testOptions = coerce (testOptions @TestProgram)

data ClashBinaryTest = ClashBinaryTest
  { cbBuildTarget :: HDL
  , cbSourceDirectory :: FilePath
  , cbExtraBuildArgs :: [String]
  , cbExtraExecArgs :: [String]
  , cbModName :: String
  , cbOutputDirectory :: IO FilePath
  }

instance IsTest ClashBinaryTest where
  run optionSet ClashBinaryTest{..} progressCallback = do
    oDir <- cbOutputDirectory
    buildRes <- run optionSet (buildProgram oDir) progressCallback

    if resultSuccessful buildRes
      then run optionSet (execProgram oDir) progressCallback
      else pure buildRes
   where
    buildProgram oDir =
      TestProgram "clash" (buildArgs oDir) NoGlob PrintStdErr False Nothing []

    buildArgs oDir =
      [ "-package", "clash-testsuite"
      , "-rtsopts"
      , "-main-is", cbModName <> ".main" <> show cbBuildTarget
      , "-o", oDir </> "out"
      , "-i" <> cbSourceDirectory
      , "-outputdir", oDir
      ] <> cbExtraBuildArgs <>
      [ cbSourceDirectory </> cbModName <.> "hs"
      ]

    execProgram oDir =
      TestHeisenbugProgram (oDir </> "out")
        (   [ "+RTS", "-xm20000000", "-RTS" ]
         <> (oDir:cbExtraExecArgs))
        NoGlob PrintStdErr False Nothing []

  testOptions = coerce (testOptions @TestProgram)

-- | Generate test trees for running GHDL
ghdlTests
  :: TestOptions
  -> IO FilePath
  -> [TestTarget]
ghdlTests opts@TestOptions{..} parentTmp =
  flip map (getBuildTargets opts) (\t ->
    TestTarget { buildTests = build t
               , simTests = sim t
               })
 where
  dir = targetTempPath parentTmp "ghdl"
  toolName = stepName "ghdl"
  importName = toolName "import"
  makeName = toolName "make"
  simName = toolName "sim"
  build t =
    [ ( importName t
      , singleTest (importName t) $ GhdlImportTest parentTmp (dir t))
    , ( makeName t
      , singleTest (makeName t) $ GhdlMakeTest (dir t) t)
    ]
  sim t =
    [ (simName t, singleTest (simName t) $ GhdlSimTest expectSimFail (dir t) t)
    ]

-- | Generate test trees for running Icarus Verilog
iverilogTests
  :: TestOptions
  -> IO FilePath
  -> [TestTarget]
iverilogTests opts@TestOptions{..} parentTmp =
  flip map (getBuildTargets opts) (\t ->
    TestTarget { buildTests = build t
               , simTests = sim t
               })
 where
  dir = targetTempPath parentTmp "iverilog"
  toolName = stepName "iverilog"
  makeName = toolName "make"
  simName = toolName "sim"
  build t =
    [ ( makeName t
      , singleTest (makeName t) $ IVerilogMakeTest parentTmp (dir t) t)
    ]
  sim t =
    [ ( simName t
      , singleTest (simName t) $
          IVerilogSimTest expectSimFail vvpStdoutNonEmptyFail (dir t) t)
    ]

-- | Generate test trees for running ModelSim
modelsimTests
  :: TestOptions
  -> IO FilePath
  -> [TestTarget]
modelsimTests opts@TestOptions{..} parentTmp =
  flip map (getBuildTargets opts) (\t ->
    TestTarget { buildTests = build t
               , simTests = sim t
               })
 where
  dir = targetTempPath parentTmp "modelsim"
  toolName = stepName "modelsim"
  vlibName = toolName "vlib"
  vlogName = toolName "vlog"
  simName = toolName "sim"
  build t =
    [ ( vlibName t
      , singleTest (vlibName t) $ ModelsimVlibTest parentTmp (dir t))
    , ( vlogName t
      , singleTest (vlogName t) $ ModelsimVlogTest (dir t))
    ]
  sim t =
    [ ( simName t
      , singleTest (simName t) $ ModelsimSimTest expectSimFail (dir t) t)
    ]

-- | Generate test trees for running Verilator
verilatorTests
  :: TestOptions
  -> IO FilePath
  -> [TestTarget]
verilatorTests opts@TestOptions{..} parentTmp =
  flip map (getBuildTargets opts) (\t ->
    TestTarget { buildTests = build t
               , simTests = sim t
               })
 where
  dir = targetTempPath parentTmp "verilator"
  toolName = stepName "verilator"
  makeName = toolName "make"
  simName = toolName "sim"
  build t =
    [ ( makeName t
      , singleTest (makeName t) $ VerilatorMakeTest parentTmp (dir t) t)
    ]
  sim t =
    [ ( simName t
      , singleTest (simName t) $
          VerilatorSimTest expectSimFail vvpStdoutNonEmptyFail (dir t) t)
    ]

-- | Generate a test tree for running Vivado. Depending on 'hdlSim' it will be
-- executed or not.
vivadoTests
  :: TestOptions
  -> IO FilePath
  -> [TestTarget]
vivadoTests opts parentTmp =
  flip map (getBuildTargets opts) (\t ->
    TestTarget { buildTests = []
               , simTests = sim t
               })
 where
  dir = targetTempPath parentTmp "vivado"
  simName = stepName "vivado" "sim"
  sim t =
    [ ( simName t
      , singleTest (simName t) $ VivadoTest parentTmp (dir t) (T.pack t))
    ]

-- | Generate a test tree for running SymbiYosys
sbyTests
  :: TestOptions
  -> IO FilePath
  -> [(TestName, TestTree)]
sbyTests opts@TestOptions {..} parentTmp =
  [ ( sbyName t
    , singleTest (sbyName t) $
        SbyVerificationTest expectVerificationFail parentTmp (dir t) t
    )
  | t <- getBuildTargets opts
  ]
 where
  dir = targetTempPath parentTmp "symbiyosys"
  sbyName t = "symbiyosys (" <> t <> ")"

runTest1
  :: String
  -> TestOptions
  -> [TestName]
  -> HDL
  -> TestTree
runTest1 modName opts@TestOptions{..} path target =
  withResource mkTmpDir Directory.removeDirectoryRecursive $ \tmpDir ->
    testGroup (show target) $
      sequenceIndependent (clashTest tmpDir) $
           (case verificationTool of
             Nothing -> []
             Just SymbiYosys -> sbyTests opts tmpDir)
         : hdlTests tmpDir
 where
  mkTmpDir = flip createTempDirectory "clash-test" =<< getCanonicalTemporaryDirectory
  sourceDir = List.foldl' (</>) sourceDirectory (reverse (tail path))

  clashTest tmpDir =
    ("clash (gen)", singleTest "clash (gen)" (ClashGenTest {
      cgExpectFailure=expectClashFail
    , cgBuildTarget=target
    , cgSourceDirectory=sourceDir
    , cgExtraArgs=clashFlags
    , cgModName=modName
    , cgOutputDirectory=tmpDir
    , cgHdlDirectory=fmap (</> "hdl") tmpDir
    }))

  buildAndSimTests :: Sim -> [TestTarget] -> [[(TestName, TestTree)]]
  buildAndSimTests sim tests
    | isJust expectClashFail = []
    | otherwise              = flip map tests $ \TestTarget{..} ->
      (if sim `elem` hdlLoad then buildTests else []) <>
      (if sim `elem` hdlSim then simTests else [])

  -- HACK: We want to run simulators independently if multiple are going to be
  -- run, otherwise failures from whichever comes first will mean the second is
  -- skipped. In lieu of a better way to sequence tests we can sequence tests
  -- for multiple simulators, then drop the first test tree for all but the
  -- first simulator (as it will be copies of clash gen).
  --
  -- TODO: Since tasty doesn't provide one, we should really provide a better
  -- set of combinators for describing test dependencies. That way we can have
  -- some more principled way of having a test structure like
  --
  --   Group A
  --     - Task A1
  --     - Group B
  --         - Task B1
  --         - Task B2
  --     - Group C
  --         - Task C1
  --
  -- where groups B and C are independent of each other, but both dependent on
  -- the success of Task A1.
  sequenceIndependent
    :: (TestName, TestTree)
    -- ^ The tree everything else depends on ("clash (gen)")
    -> [[(TestName, TestTree)]]
    -- ^ All the independent trees to run after "clash (gen)"
    -> [TestTree]
  sequenceIndependent gen = go
   where
    go [] = sequenceOne []
    go (l:ls) = sequenceOne l <> concatMap (tail . sequenceOne) ls
    sequenceOne ts = sequenceTests (show target : path) (gen : ts)

  -- | The tests that are switched by `hdlLoad` and `hdlSim`
  hdlTests tmpDir = case target of
    VHDL -> concat
      [ buildAndSimTests GHDL (ghdlTests opts tmpDir)
      , buildAndSimTests Vivado (vivadoTests opts tmpDir)
      ]
    Verilog -> concat
      [ buildAndSimTests IVerilog (iverilogTests opts tmpDir)
      , buildAndSimTests Verilator (verilatorTests opts tmpDir)
      , buildAndSimTests Vivado (vivadoTests opts tmpDir)
      ]
    SystemVerilog -> concat
      [ -- TODO: ModelSim can do VHDL and Verilog too. Add that?
        buildAndSimTests ModelSim (modelsimTests opts tmpDir)
      , buildAndSimTests Verilator (verilatorTests opts tmpDir)
      ]

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
  withResource mkTmpDir Directory.removeDirectoryRecursive $ \tmpDir ->
    testGroup (show target) $ sequenceTests (show target : path) $
      [ clashGenHdl tmpDir
      , clashBuild tmpDir
      ]
 where
  mkTmpDir = flip createTempDirectory "clash-test" =<< getCanonicalTemporaryDirectory
  sourceDir = List.foldl' (</>) sourceDirectory (reverse (tail path))

  clashGenHdl workDir = ("clash (gen)", singleTest "clash (gen)" (ClashGenTest {
      cgExpectFailure=Nothing
    , cgBuildTarget=target
    , cgSourceDirectory=sourceDir
    , cgExtraArgs=extraClashArgs
    , cgModName=modName
    , cgOutputDirectory=workDir
    , cgHdlDirectory=workDir
    }))

  clashBuild workDir = ("clash (exec)", singleTest "clash (exec)" (ClashBinaryTest {
      cbBuildTarget=target
    , cbSourceDirectory=sourceDir
    , cbExtraBuildArgs="-DOUTPUTTEST" : extraGhcArgs
    , cbExtraExecArgs=[]
    , cbModName=modName
    , cbOutputDirectory=workDir
    }))

outputTest
  :: String
  -- ^ Module name
  -> TestOptions
  -> [TestName]
  -- ^ Parent test names in order of distance to the test. That is, the last
  -- item in the list will be the root node, while the first one will be the
  -- one closest to the test.
  -> TestTree
outputTest modName opts path =
  let testName = modName ++ " [output test]"
   in testGroup testName
        [ outputTest' modName target (clashFlags opts) (ghcFlags opts) (testName:path)
        | target <- hdlTargets opts
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
  withResource mkTmpDir Directory.removeDirectoryRecursive $ \tmpDir ->
    testGroup (show target) $ sequenceTests (show target : path) $
      [ clashBuild tmpDir
      ]
 where
  mkTmpDir = flip createTempDirectory "clash-test" =<< getCanonicalTemporaryDirectory
  sourceDir = List.foldl' (</>) sourceDirectory (reverse (tail path))

  clashBuild workDir = ("clash (exec)", singleTest "clash (exec)" (ClashBinaryTest {
      cbBuildTarget=target
    , cbSourceDirectory=sourceDir
    , cbExtraBuildArgs="-DCLASHLIBTEST" : extraGhcArgs
    , cbExtraExecArgs=[]
    , cbModName=modName
    , cbOutputDirectory=workDir
    }))

clashLibTest
  :: String
  -- ^ Module name
  -> TestOptions
  -> [TestName]
  -- ^ Parent test names in order of distance to the test. That is, the last
  -- item in the list will be the root node, while the first one will be the
  -- one closest to the test.
  -> TestTree
clashLibTest modName opts path =
  -- HACK: clashLibTests are run sequentially to prevent race issues. See:
  -- HACK: https://github.com/clash-lang/clash-compiler/pull/1416
  let testName = modName ++ " [clash-lib test]"
   in testGroup testName $ sequenceTests (testName:path)
        [ (show target, clashLibTest' modName target (ghcFlags opts) (testName:path))
        | target <- hdlTargets opts
        ]
