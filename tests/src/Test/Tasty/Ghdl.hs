{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

module Test.Tasty.Ghdl where

import           Clash.Driver.Manifest     (Manifest(..), manifestFilename)
import           Control.Exception         (IOException, try)
import           Control.Monad             (foldM, forM_)
import           Control.Monad.Except      (ExceptT, throwError, runExceptT)
import           Control.Monad.IO.Class    (liftIO)
import           Data.Char                 (toLower)
import           Data.Coerce               (coerce)
import qualified Data.List                 as List
import           Data.Proxy
import           Data.Tagged
import qualified Data.Text                 as T
import           Data.String.Interpolate   (i)
import           System.Directory          (createDirectory, listDirectory, copyFile)
import           System.FilePath           ((</>), replaceFileName)
import           System.FilePath.Glob      (glob)
import           System.Process            (readProcess)

import           Test.Tasty.Common
import           Test.Tasty.Options
import           Test.Tasty.Program
import           Test.Tasty.Providers
import           Test.Tasty.Runners

-- | @--no-ghdl@ flag for disabling tests that use GHDL.
newtype Ghdl = Ghdl Bool
  deriving (Eq, Ord)

instance IsOption Ghdl where
  defaultValue = Ghdl True
  parseValue = fmap Ghdl . safeReadBool
  optionName = pure "no-ghdl"
  optionHelp = pure "Skip GHDL tests"
  optionCLParser = flagCLParser Nothing (Ghdl False)

-- | @--ghdl-gcc@ flag for forcing the GCC-backed GHDL binary.
newtype GhdlGcc = GhdlGcc Bool
  deriving (Eq, Ord)

instance IsOption GhdlGcc where
  defaultValue = GhdlGcc False
  parseValue = fmap GhdlGcc . safeReadBool
  optionName = pure "ghdl-gcc"
  optionHelp = pure "Force GHDL tests to use ghdl-gcc"
  optionCLParser = flagCLParser Nothing (GhdlGcc True)

-- | @--ghdl-llvm@ flag for forcing the LLVM-backed GHDL binary.
newtype GhdlLlvm = GhdlLlvm Bool
  deriving (Eq, Ord)

instance IsOption GhdlLlvm where
  defaultValue = GhdlLlvm False
  parseValue = fmap GhdlLlvm . safeReadBool
  optionName = pure "ghdl-llvm"
  optionHelp = pure "Force GHDL tests to use ghdl-llvm"
  optionCLParser = flagCLParser Nothing (GhdlLlvm True)

-- | @--ghdl-mcode@ flag for forcing the mcode-backed GHDL binary.
newtype GhdlMcode = GhdlMcode Bool
  deriving (Eq, Ord)

instance IsOption GhdlMcode where
  defaultValue = GhdlMcode False
  parseValue = fmap GhdlMcode . safeReadBool
  optionName = pure "ghdl-mcode"
  optionHelp = pure "Force GHDL tests to use ghdl-mcode"
  optionCLParser = flagCLParser Nothing (GhdlMcode True)

-- | The Tasty options that every GHDL @IsTest@ instance below needs to look at.
ghdlOptions :: [OptionDescription]
ghdlOptions =
  [ Option (Proxy @Ghdl)
  , Option (Proxy @GhdlGcc)
  , Option (Proxy @GhdlLlvm)
  , Option (Proxy @GhdlMcode)
  ]

-- | Which native code generator backs a GHDL binary. Some invocations need
-- to differ per flavor — e.g. @-o@ on @-m@ is gcc/llvm-only.
data GhdlFlavor = GhdlGccBackend | GhdlLlvmBackend | GhdlMcodeBackend
  deriving (Eq, Show)

-- | Resolve the GHDL binary to invoke based on the flavor flags. If not flags
-- are set, auto-detect.
getGhdl :: OptionSet -> ExceptT String IO (Maybe (FilePath, GhdlFlavor))
getGhdl optionSet | Ghdl False <- lookupOption optionSet = pure Nothing
getGhdl optionSet =
  case forced of
    [] -> do
      flavor <- detectGhdlFlavor
      pure $ Just ("ghdl", flavor)
    [(_, bin, flavor)] ->
      pure $ Just (bin, flavor)
    xs -> do
      let flags = List.intercalate ", " $ map (\(f, _, _) -> show f) xs
      throwError [i|Conflicting GHDL flags: #{flags}. Pass at most one.|]
 where
  GhdlGcc gcc = lookupOption optionSet
  GhdlLlvm llvm = lookupOption optionSet
  GhdlMcode mcode = lookupOption optionSet

  forced :: [(String, String, GhdlFlavor)]
  forced =
       [("--ghdl-gcc",   "ghdl-gcc",   GhdlGccBackend)   | gcc]
    <> [("--ghdl-llvm",  "ghdl-llvm",  GhdlLlvmBackend)  | llvm]
    <> [("--ghdl-mcode", "ghdl-mcode", GhdlMcodeBackend) | mcode]

-- | Detect a GHDL binary's backend by parsing the output of @ghdl --version@.
detectGhdlFlavor :: ExceptT String IO GhdlFlavor
detectGhdlFlavor = do
  eOut <- liftIO (try (readProcess "ghdl" ["--version"] "") :: IO (Either IOException String))
  case eOut of
    Left err -> throwError (show err)
    Right out
      | "mcode" `List.isInfixOf` out -> pure GhdlMcodeBackend
      | "GCC"   `List.isInfixOf` out -> pure GhdlGccBackend
      | "llvm"  `List.isInfixOf` out -> pure GhdlLlvmBackend
      | otherwise -> throwError [i|Could not parse backend:\n\n#{out}|]

-- | Search through a directory with VHDL files produced by Clash
-- and produces /work/ files using @ghdl -i@ for each library.
--
-- For example, for I2C it would execute:
--
-- > ghdl -i --work=bitMaster --workdir=bitMaster --std=93 <files>
-- > ghdl -i --work=byteMaster --workdir=byteMaster --std=93 <files>
-- > ghdl -i --work=i2c --workdir=i2c --std=93 <files>
--
-- After executing this test, $tmpDir/work contains a directory for each
-- top entity: @bitMaster@, @byteMaster@, @i2c@. A more typical test case might
-- produce @topEntity@ and @testBench@ instead.
--
data GhdlImportTest = GhdlImportTest
  { gitParentDirectory :: IO FilePath
    -- ^ Shared temporary directory
  , gitSourceDirectory :: IO FilePath
    -- ^ Directory to work from
  }

instance IsTest GhdlImportTest where
  run optionSet GhdlImportTest{..} progressCallback = do
    ghdlOrException <- runExceptT (getGhdl optionSet)
    case ghdlOrException of
      Left err -> pure (testFailed err)
      Right Nothing -> pure (testPassed "Ignoring test: due to --no-ghdl")
      Right (Just (bin, _flavor)) -> do
        buildTargetDir gitParentDirectory gitSourceDirectory
        src <- gitSourceDirectory
        let workDir = src </> "work"
        createDirectory workDir
        manifests <- getManifests (src </> "*" </> manifestFilename)
        foldM (goManifest bin workDir) (testPassed "") manifests
   where
    stdArgs  = ["-i", "--std=93"]
    runGhdlI bin workDir args =
      run optionSet (ghdlI bin workDir args) progressCallback
    ghdlI bin workDir args =
      TestProgram bin (stdArgs <> args) NoGlob PrintNeither False (Just workDir) []

    -- Read a manifest file, error if its malformed / inaccessible. Run @ghdl -i@
    -- on files associated with the component.
    goManifest :: FilePath -> FilePath -> Result -> (FilePath, Manifest) -> IO Result
    goManifest bin workDir result (manifestPath, Manifest{topComponent,fileNames})
      | resultSuccessful result = do
        let
          top = T.unpack topComponent
          relVhdlFiles = filter (".vhdl" `List.isSuffixOf`) (map fst fileNames)
          absVhdlFiles = map (replaceFileName manifestPath) relVhdlFiles
        createDirectory (workDir </> top)
        runGhdlI bin workDir (["--work=" <> top, "--workdir=" <> top] <> absVhdlFiles)

      | otherwise = pure result

  testOptions =
    coerce (coerce (testOptions @TestProgram) <> ghdlOptions)

-- | Elaborate the design given directory 'GhdlImportTest' produced work files
-- in. With ghdl-gcc/-llvm this also produces a native executable; with
-- ghdl-mcode it just runs the elaborator (no executable is produced).
--
-- For example, for I2C it would execute:
--
-- > ghdl -m -fpsl --work=i2c --workdir=i2c -PbitMaster -PbyteMaster -Pi2c i2c
--
data GhdlMakeTest = GhdlMakeTest
  { gmtSourceDirectory :: IO FilePath
    -- ^ Directory containing VHDL files produced by Clash
  , gmtTop :: String
    -- ^ Entry point to be converted to executables
  }

instance IsTest GhdlMakeTest where
  run optionSet GhdlMakeTest{gmtSourceDirectory,gmtTop} progressCallback = do
    ghdlOrException <- runExceptT (getGhdl optionSet)
    case ghdlOrException of
      Left err -> pure (testFailed err)
      Right Nothing -> pure (testPassed "Ignoring test due to --no-ghdl")
      Right (Just (bin, flavor)) -> do
        src <- gmtSourceDirectory
        let workDir = src </> "work"
        libs <- listDirectory workDir
        -- GCC/LLM need an explicit `-o` because their default output filename is
        -- the lowercased unit name, which collides with the per-library subdir
        -- of the same name in cwd. MCODE rejects `-o` entirely (it produces no
        -- executable; it's a JIT simulator).
        let outFlag = case flavor of
              GhdlMcodeBackend -> []
              _ -> ["-o", map toLower (gmtTop <> "_exe")]
        runGhdl bin workDir $
          ["-m", "-fpsl", "--work=" <> gmtTop, "--workdir=" <> gmtTop]
            <> ["-P" <> lib | lib <- libs]
            <> outFlag
            <> [gmtTop]
   where
    ghdl bin workDir args = TestProgram bin args NoGlob PrintNeither False (Just workDir) []
    runGhdl bin workDir args = run optionSet (ghdl bin workDir args) progressCallback

  testOptions =
    coerce (coerce (testOptions @TestProgram) <> ghdlOptions)

-- | Run the design unit elaborated by 'GhdlMakeTest'.
--
-- For example, for I2C it would execute:
--
-- > ghdl -r --workdir=i2c --work=i2c i2c --assert-level=error
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
    ghdlOrException <- runExceptT (getGhdl optionSet)
    case ghdlOrException of
      Left err -> pure (testFailed err)
      Right Nothing -> pure (testPassed "Ignoring test due to --no-ghdl")
      Right (Just (bin, flavor)) -> do
        src <- gstSourceDirectory
        let workDir = src </> "work"
        libs <- listDirectory workDir

        -- See Note [copy data files hack]
        lists <- glob (src </> "*/memory.list")
        forM_ lists $ \memFile ->
          copyFile memFile (workDir </> "memory.list")

        case gstExpectFailure of
          Nothing -> run optionSet (program flavor bin workDir libs gstTop) progressCallback
          Just exit -> run optionSet (failingProgram flavor bin workDir libs gstTop exit) progressCallback
   where
    program flavor bin workDir libs top =
      TestProgram bin (args flavor libs top) NoGlob PrintNeither False (Just workDir) []

    failingProgram flavor bin workDir libs top (testExit, expectedErr) =
      TestFailingProgram
        (testExitCode testExit) bin (args flavor libs top) NoGlob PrintNeither False
        (specificExitCode testExit) (ExpectEither expectedErr) (Just workDir) []

    -- GCC/LLVM produce a linked executable on `-m` named `<lower top>_exe`
    -- (matching the `-o` we passed there). Pass that filename to `-r`. MCODE has
    -- no executable pass the unit name to `-r` and it will re-elaborate. The
    -- `-P` flags are required for MCODE's re-elaboration find sibling libraries
    -- by directory name; they are harmless for the others.
    args flavor libs work =
      [ "-r"
      , "--workdir=" <> work
      , "--work=" <> work
      ]
      <> ["-P" <> lib | lib <- libs]
      <> psl
      <> [target, "--assert-level=error"]
     where
      target = case flavor of
        GhdlMcodeBackend -> work
        _ -> map toLower (work <> "_exe")
      -- MCODE re-elaborates at -r time; without -fpsl here, PSL assertions
      -- elaborated by `ghdl -m -fpsl` are dropped and never fire. GCC/LLVM
      -- run a linked binary so the flag is irrelevant at -r.
      psl = case flavor of
        GhdlMcodeBackend -> ["-fpsl"]
        _ -> []

  testOptions =
    coerce (coerce (testOptions @TestProgram) <> ghdlOptions)
