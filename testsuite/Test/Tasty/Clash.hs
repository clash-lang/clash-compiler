{-# LANGUAGE CPP #-}
module Test.Tasty.Clash where

import           Data.Char                 (toLower)
import qualified Data.List                 as List
import           Data.List                 (intercalate)
import qualified Data.Text                 as T
import qualified System.Directory          as Directory
import           System.Environment        (getEnv)
import           System.FilePath           ((</>),(<.>))
import           System.IO.Unsafe          (unsafePerformIO)
import           System.IO.Temp            (createTempDirectory)

import Test.Tasty
  (TestTree, TestName, DependencyType(AllSucceed), testGroup, withResource, after)

import Test.Tasty.Program
  ( testProgram, testFailingProgram
  , PrintOutput (PrintStdErr, PrintNeither), GlobArgs(..))

data BuildTarget
  = VHDL
  | SystemVerilog
  | Verilog
  deriving (Show, Eq, Ord)


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

-- | Directory to install Clash binary in
clashBin :: String
clashBin = unsafePerformIO (getEnv "clash_bin")
{-# NOINLINE clashBin #-}

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

-- | Set the stage for compilation
createDirs
  :: [TestName]
  -> [FilePath]
  -> (TestName, TestTree)
createDirs path subdirs =
  (tnm, testProgram tnm "mkdir" ("-p":allDirs) NoGlob PrintStdErr False Nothing)
  where
    tdir     = testDirectory path
    subdirs' = map (tdir </>) subdirs
    allDirs  = tdir:subdirs'
    tnm      = "create temporary directories"

-- | Generate command to run clash to compile a file and place the resulting
-- hdl files in a specific directory
clashCmd
  :: BuildTarget
  -- ^ Build target
  -> FilePath
  -- ^ Source directory
  -> [String]
  -- ^ Extra arguments
  -> String
  -- ^ Module name
  -> FilePath
  -- ^ Output directory
  -> (String, [String])
  -- ^ (command, arguments)
clashCmd target sourceDir extraArgs modName oDir =
  (clashBin, args)
    where
      args = concat [
          [target']
        , extraArgs
        , [sourceDir </> modName <.> "hs"]
        , ["-i" ++ sourceDir]
        , ["-fclash-hdldir", oDir]
        , ["-odir", oDir]
        , ["-hidir", oDir]
        , ["-fclash-debug", "DebugSilent"]
        ]

      target' =
        case target of
          VHDL          -> "--vhdl"
          Verilog       -> "--verilog"
          SystemVerilog -> "--systemverilog"

clashHDL
  :: BuildTarget
  -- ^ Build target
  -> FilePath
  -- ^ Source directory
  -> [String]
  -- ^ Extra arguments
  -> String
  -- ^ Module name
  -> FilePath
  -- ^ Output directory
  -> (TestName, TestTree)
clashHDL t sourceDir extraArgs modName oDir =
  let (cmd, args) = clashCmd t sourceDir extraArgs modName oDir in
--   let testName = List.intercalate " " $ "clash" : extraArgs in
  let testName = "clash" in
  (testName, testProgram testName cmd args NoGlob PrintStdErr False Nothing)

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
      pattern :: TestName -> String
      pattern nm = "$0 == \"" ++ intercalate "." (reverse (nm:path)) ++ "\""

      -- Test patterns for all given tests such that each executes sequentially
      testPatterns = init (map (fmap pattern) (Nothing : map Just testNames))

      -- | Generate pattenrs given parent patterns and item patterns
      applyAfter :: Maybe String -> TestTree -> TestTree
      applyAfter Nothing  tt = tt
      applyAfter (Just p) tt = after AllSucceed p tt


ghdlLibrary
  :: [TestName]
  -- ^ Path to test
  -> String
  -- ^ Module name
  -> FilePath
  -- ^ Directory with the VHDL files
  -> (TestName, TestTree)
ghdlLibrary path modName lib =
  (testName, testProgram testName "ghdl" args GlobStar PrintStdErr False (Just workDir))
      where
        testName =
          case lib of
            "" -> "GHDL (library)"
            _  -> "GHDL (library) [" ++ lib ++ "]"

        workDir = testDirectory path </> "vhdl" </> modName

        args :: [String]
        args = [ "-i"
               , ("--work=" ++ workName)
               , ("--workdir=" ++ relWorkdir)
               , ("--std=93")
               , (workDir </> lib' </> "*.vhdl")
               ]

        lib' = map toLower lib

        -- Special case for FIR?
        workName =
          case lib' of
            [] ->
              case modName of
                "FIR" -> "test_fir_topentity"
                _     -> map toLower modName ++ "_topentity"
            k ->
              k

        relWorkdir =
          case lib' of
            [] -> "."
            k -> k

ghdlImport
  :: [TestName]
  -- ^ Path to test
  -> String
  -- ^ Module name
  -> [FilePath]
  -- ^ Directories with the VHDL files
  -> (TestName, TestTree)
ghdlImport path modName subdirs =
  (testName, test)
    where
      subdirs' = (map.map) toLower subdirs
      testName = "GHDL (import)"
      test = testProgram testName "ghdl" args GlobStar PrintStdErr False (Just workDir)
      workDir = testDirectory path </> "vhdl" </> modName
      args = "-i":"--workdir=work":"--std=93":[workDir </> subdir </> "*.vhdl" | subdir <- subdirs']

ghdlMake
  :: [TestName]
  -- ^ Path to test
  -> String
  -- ^ Module name
  -> [FilePath]
  -- ^ Directories with the VHDL files
  -> [FilePath]
  -- ^ Library directories
  -> String
  -- ^ Name of the components we want to build
  -> (TestName, TestTree)
ghdlMake path modName subdirs libs entName =
  (testName, test)
  where
    args = concat [ ["-m"]
               -- TODO: Automatically detect GCC/linker version
               -- Enable flags when running newer versions of the (GCC) linker.
               -- , ["-Wl,-no-pie"]
                  , ["--workdir=work"]
                  , map (\l -> "-P" ++ emptyToDot (map toLower l)) libs
                  , ["-o", map toLower (noConflict entName subdirs) ]
                  , [entName] ]
    testName = "GHDL (make)"
    test = testProgram testName "ghdl" args NoGlob PrintStdErr False (Just workDir)
    workDir = testDirectory path </> "vhdl" </> modName
    emptyToDot [] = "."
    emptyToDot k  = k

ghdlSim
  :: [TestName]
  -- ^ Path to test
  -> String
  -- ^ Module name
  -> String
  -- ^ Name of the testbench executable
  -> (TestName, TestTree)
ghdlSim path modName tbName =
  (testName, test)
  where
    workDir = testDirectory path </> "vhdl" </> modName
    testName = "GHDL (sim)"
    args = ["-r","--workdir=work",tbName,"--assert-level=error"]
    test = testProgram testName "ghdl" args NoGlob PrintStdErr False (Just workDir)

iverilog
  :: [TestName]
  -- ^ Path to test
  -> String
  -- ^ Module name
  -> [FilePath]
  -- ^ Directories with the Verilog files
  -> String
  -- ^ Name of the component we want to build
  -> (TestName, TestTree)
iverilog path modName subdirs entName =
  (testName, test)
    where
      workDir = testDirectory path </> "verilog" </> modName
      test = testProgram testName "iverilog" args GlobStar PrintStdErr False (Just workDir)
      testName = "iverilog"
      args = ("-g2":"-s":entName:"-o":noConflict entName subdirs:[workDir </> subdir </> "*.v" | subdir <- subdirs])

noConflict :: String -> [String] -> String
noConflict nm seen
  | nm `elem` seen = go (0 :: Int)
  | otherwise      = nm
  where
    go n
      | (nm ++ show n) `elem` seen = go (n+1)
      | otherwise                  = (nm ++ show n)

vvp
  :: [TestName]
  -- ^ Path to test
  -> String
  -- ^ Module name
  -> String
  -- ^ Name of the testbench object
  -> (TestName, TestTree)
vvp path modName entName =
  ("vvp", testProgram "vvp" "vvp" [entName] NoGlob PrintStdErr True (Just workDir))
    where
      workDir = testDirectory path </> "verilog" </> modName

vlog
  :: [TestName]
  -- ^ Path to test
  -> String
  -- ^ Module name
  -> [FilePath]
  -- ^ Directory with the SystemVerilog files
  -> [(TestName, TestTree)]
vlog path modName subdirs =
  [ ("vlib", testProgram "vlib" "vlib" ["work"] NoGlob PrintStdErr False (Just workDir))
  , ("vlog", testProgram "vlog" "vlog" ("-sv":"-work":"work":typFiles++allFiles) GlobStar PrintStdErr False (Just workDir))
  ]
  where
    workDir = testDirectory path </> "systemverilog" </> modName
    typFiles = map (</> "*_types.sv") subdirs
    allFiles = map (</> "*.sv") subdirs

vsim
  :: [TestName]
  -- ^ Path to test
  -> String
  -- ^ Module name
  -> String
  -- ^ Name of testbench
  -> (TestName, TestTree)
vsim path modName entName =
  ("vsim", testProgram "vsim" "vsim" args NoGlob PrintStdErr False (Just workDir))
  where
    workDir = testDirectory path </> "systemverilog" </> modName

    args = ["-batch", "-do", doScript, entName]

    doScript = List.intercalate ";"
      [ "run -all"
      , unwords
         ["if {[string equal ready [runStatus]]}"
         ,"then {quit -f}"
         ,"else {quit -code 1 -f}"
         ]
      , "quit -code 2 -f"
      ]


runTest'
  :: FilePath
  -- ^ Directory test files are located
  -> BuildTarget
  -- ^ Targets which should be tested
  -> [String]
  -- ^ Commandline options passed to Clash
  -> String
  -- ^ Name of test
  -> [String]
  -- ^ Directories to import in simulator
  -> String
  -- ^ Function to test/run
  -> Bool
  -- ^ Run HDL simulations. If False, functions will only be compiled with
  -- the simulators, but never executed.
  -> [TestName]
  -- ^ Parent test names in order of distance to the test. That is, the last
  -- item in the list will be the root node, while the first one will be the
  -- one closest to the test.
  -> TestTree
runTest' env VHDL extraArgs modName subdirs entName doSim path =
  withResource acquire tastyRelease (const seqTests)
    where
      vhdlDir = "vhdl"
      modDir  = vhdlDir </> modName
      workDir = modDir </> "work"
      acquire = tastyAcquire path' [vhdlDir, modDir, workDir]
      path'   = "VHDL":path

      libs
        | length subdirs == 1 = []
        | otherwise           = subdirs List.\\ [entName]

      seqTests :: TestTree
      seqTests = testGroup "VHDL" (sequenceTests path' tests)

      tests = concat $ [
          [ clashHDL VHDL (sourceDirectory </> env) extraArgs modName (testDirectory path') ]
        , map (ghdlLibrary path' modName) libs
        , [ghdlImport path' modName (subdirs List.\\ libs)]
        , [ghdlMake path' modName subdirs libs entName]
        ] ++ [if doSim then [ghdlSim path' modName (noConflict entName subdirs)] else []]

runTest' env Verilog extraArgs modName subdirs entName doSim path =
  withResource acquire tastyRelease (const seqTests)
    where
      verilogDir = "verilog"
      modDir     = verilogDir </> modName
      acquire    = tastyAcquire path' [verilogDir, modDir]
      path'      = "Verilog":path

      seqTests = testGroup "Verilog" $ sequenceTests path' $
        [ clashHDL Verilog (sourceDirectory </> env) extraArgs modName (testDirectory path')
        , iverilog path' modName subdirs entName
        ] ++ if doSim then [vvp path' modName (noConflict entName subdirs)] else []
--           ++ map (\f -> f (cwDir </> env) Verilog verilogDir modDir modName entName)

runTest' env SystemVerilog extraArgs modName subdirs entName doSim path =
  withResource acquire tastyRelease (const seqTests)
    where
      svDir   = "systemverilog"
      modDir  = svDir </> modName
      acquire = tastyAcquire path' [svDir, modDir]
      path'   = "SystemVerilog":path

      seqTests = testGroup "SystemVerilog" $ sequenceTests path' $ concat $
        [ [ clashHDL SystemVerilog (sourceDirectory </> env) extraArgs modName (testDirectory path') ]
          , vlog path' modName subdirs
          ] ++ [if doSim then [vsim path' modName entName] else []]
            -- ++ [map (\f -> f (cwDir </> env) SystemVerilog svDir modDir modName entName) ]

runTest
  :: FilePath
  -- ^ Directory test files are in
  -> [BuildTarget]
  -- ^ Targets which should be tested
  -> [String]
  -- ^ Commandline options passed to Clash
  -> String
  -- ^ Name of test
  -> ([String], String, Bool)
  -- ^ ( Directories to import in simulator
  --   , Function to test/run/
  --   , Run HDL simulations. If False, functions will only be compiled with
  --     the simulators, but never executed. )
  -> [TestName]
  -- ^ Parent test names in order of distance to the test. That is, the last
  -- item in the list will be the root node, while the first one will be the
  -- one closest to the test.
  -> TestTree
runTest env targets extraArgs modName (subdirs, entName, doSim) path =
  testGroup modName (map runTest'' targets)
    where
      runTest'' target =
        runTest' env target extraArgs modName subdirs entName doSim (modName:path)

runFailingTest'
  :: Bool
  -- ^ Test exit code?
  -> FilePath
  -- ^ Work directory
  -> BuildTarget
  -- ^ Build target
  -> [String]
  -- ^ Extra arguments
  -> String
  -- ^ Module name
  -> Maybe T.Text
  -- ^ Expected stderr
  -> [TestName]
  -- ^ Parent test names in order of distance to the test. That is, the last
  -- item in the list will be the root node, while the first one will be the
  -- one closest to the test.
  -> TestTree
runFailingTest' testExitCode env target extraArgs modName expectedStderr path =
  let args0 = "-fclash-no-cache" : extraArgs in
  let (cmd, args1) = clashCmd target (sourceDirectory </> env) args0 modName (testDirectory path) in
  let testName    = "clash" in
  testFailingProgram
    testExitCode
    testName
    cmd
    args1
    NoGlob
    PrintNeither
    False
    Nothing
    expectedStderr
    Nothing

runFailingTest
  :: FilePath
  -- ^ Work directory
  -> [BuildTarget]
  -- ^ Build targets
  -> [String]
  -- ^ Extra arguments
  -> String
  -- ^ Module name
  -> Maybe T.Text
  -- ^ Expected stderr
  -> [TestName]
  -- ^ Parent test names in order of distance to the test. That is, the last
  -- item in the list will be the root node, while the first one will be the
  -- one closest to the test.
  -> TestTree
runFailingTest env targets extraArgs modName expectedStderr path =
  testGroup modName [ rft target (modName : path) | target <- targets ]
  where
    rft target path' =
      testGroup (show target) $
        return $
          runFailingTest' True env target extraArgs modName expectedStderr (show target : path')

runWarningTest
  :: FilePath
  -- ^ Work directory
  -> [BuildTarget]
  -- ^ Build targets
  -> [String]
  -- ^ Extra arguments
  -> String
  -- ^ Module name
  -> Maybe T.Text
  -- ^ Expected stderr
  -> [TestName]
  -- ^ Parent test names in order of distance to the test. That is, the last
  -- item in the list will be the root node, while the first one will be the
  -- one closest to the test.
  -> TestTree
runWarningTest env targets extraArgs modName expectedStderr path =
  testGroup modName [ rft target (modName : path) | target <- targets ]
  where
    rft target path' =
      testGroup (show target) $
        return $
          runFailingTest' False env target extraArgs modName expectedStderr (show target : path')

outputTest'
  :: FilePath
  -- ^ Work directory
  -> BuildTarget
  -- ^ Build target
  -> [String]
  -- ^ Extra clash arguments
  -> String
  -- ^ Module name
  -> String
  -- ^ Base function name
  -> [TestName]
  -- ^ Parent test names in order of distance to the test. That is, the last
  -- item in the list will be the root node, while the first one will be the
  -- one closest to the test.
  -> TestTree
outputTest' env target extraArgs modName funcName path =
  withResource acquire tastyRelease (const seqTests)
    where
      path' = show target:path
      acquire = tastyAcquire path' [modDir]

      modDir = (map (toLower) (show target)) </> modName

    -- Also update @Clash.GHC.LoadModules.wantedLanguagesExtensions@ when
    -- updating this list!
      args = [ "new-exec"
             , "--write-ghc-environment-files=never"
             , "--"
             , "runghc"
             , "-XBinaryLiterals"
             , "-XConstraintKinds"
             , "-XDataKinds"
             , "-XDeriveAnyClass"
             , "-XDeriveGeneric"
             , "-XDeriveLift"
             , "-XExplicitForAll"
             , "-XExplicitNamespaces"
             , "-XFlexibleContexts"
             , "-XKindSignatures"
             , "-XMagicHash"
             , "-XMonoLocalBinds"
             , "-XNoImplicitPrelude"
             , "-XNoMonomorphismRestriction"
#if __GLASGOW_HASKELL__ >= 806
             , "-XNoStarIsType"
#endif
             , "-XNoStrict"
             , "-XNoStrictData"
             , "-XQuasiQuotes"
             , "-XScopedTypeVariables"
             , "-XTemplateHaskell"
             , "-XTemplateHaskellQuotes"
             , "-XTypeApplications"
             , "-XTypeFamilies"
             , "-XTypeOperators"
             , "--ghc-arg=-main-is"
             , "--ghc-arg=" ++ modName ++ "." ++ funcName ++ show target
             , env </> modName <.> "hs"
             , workDir </> topFile
             ]

      topFile =
        case target of
          VHDL ->
            "vhdl" </> modName </> map toLower modName ++ "_topentity.vhdl"
          Verilog ->
            "verilog" </> modName </> modName ++ "_topEntity.v"
          SystemVerilog ->
            "systemverilog" </> modName </> modName ++ "_topEntity.sv"

      workDir = testDirectory path'

      seqTests = testGroup (show target) $ sequenceTests path' $
        [ clashHDL target (sourceDirectory </> env) extraArgs modName workDir
        , ("runghc", testProgram "runghc" "cabal" args NoGlob PrintStdErr False Nothing)
        ]

outputTest
  :: FilePath
  -- ^ Work directory
  -> [BuildTarget]
  -- ^ Build targets
  -> [String]
  -- ^ Extra clash arguments
  -> String
  -- ^ Module name
  -> String
  -- ^ Base function name
  -> [TestName]
  -- ^ Parent test names in order of distance to the test. That is, the last
  -- item in the list will be the root node, while the first one will be the
  -- one closest to the test.
  -> TestTree
outputTest env targets extraArgs modName funcName path =
  let testName = modName ++ " [output test]" in
  let path' = testName : path in
  testGroup testName
    [outputTest' env target extraArgs modName funcName path' | target <- targets]
