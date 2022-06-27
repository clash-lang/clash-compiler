{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

module Test.Tasty.Modelsim where

import           Control.Monad             (forM_)
import           Data.Coerce               (coerce)
import qualified Data.List                 as List
import           Data.Proxy
import           Data.Tagged
import qualified Data.Text                 as T
import           System.Directory          (copyFile)
import           System.FilePath           ((</>))
import           System.FilePath.Glob      (glob)

import           Test.Tasty.Common
import           Test.Tasty.Options
import           Test.Tasty.Program
import           Test.Tasty.Providers

-- | @--modelsim@ flag for enabling tests that use modelsim.
newtype ModelSim = ModelSim Bool
  deriving (Eq, Ord)

instance IsOption ModelSim where
  defaultValue = ModelSim True
  parseValue = fmap ModelSim . safeReadBool
  optionName = pure "no-modelsim"
  optionHelp = pure "Skip modelsim tests"
  optionCLParser = flagCLParser Nothing (ModelSim False)

data ModelsimVlibTest = ModelsimVlibTest
  { mvtSourceDirectory :: IO FilePath
    -- ^ Directory containing VHDL files produced by Clash
  }

instance IsTest ModelsimVlibTest where
  run optionSet ModelsimVlibTest{mvtSourceDirectory} progressCallback
    | ModelSim True <- lookupOption optionSet = do
        src <- mvtSourceDirectory
        runVlib src ["work"]

    | otherwise =
        pure (testPassed "Ignoring test due to --no-modelsim")
   where
    vlib workDir args = TestProgram "vlib" args NoGlob PrintNeither False (Just workDir) []
    runVlib workDir args = run optionSet (vlib workDir args) progressCallback

  testOptions =
    coerce (coerce (testOptions @TestProgram) <> [Option (Proxy @ModelSim)])

data ModelsimVlogTest = ModelsimVlogTest
  { vlogSourceDirectory :: IO FilePath
    -- ^ Directory containing VHDL files produced by Clash
  }

instance IsTest ModelsimVlogTest where
  run optionSet ModelsimVlogTest{vlogSourceDirectory} progressCallback
    | ModelSim True <- lookupOption optionSet = do
        src <- vlogSourceDirectory
        typeFiles <- glob (src </> "*" </> "*_types.sv")
        allFiles <- glob (src </> "*" </> "*.sv")
        runVlog src (["-sv", "-work", "work"] <> typeFiles <> allFiles)

    | otherwise =
        pure (testPassed "Ignoring test due to --no-modelsim")
   where
    vlog workDir args = TestProgram "vlog" args NoGlob PrintNeither False (Just workDir) []
    runVlog workDir args = run optionSet (vlog workDir args) progressCallback

  testOptions =
    coerce (coerce (testOptions @TestProgram) <> [Option (Proxy @ModelSim)])

data ModelsimSimTest = ModelsimSimTest
  { msimExpectFailure :: Maybe (TestExitCode, T.Text)
    -- ^ Expected failure code and output (if any)
  , msimSourceDirectory :: IO FilePath
    -- ^ Directory containing VHDL files produced by Clash
  , msimTop :: String
    -- ^ Entry point to simulate
  }

instance IsTest ModelsimSimTest where
  run optionSet ModelsimSimTest{..} progressCallback
    | ModelSim True <- lookupOption optionSet = do
        src <- msimSourceDirectory

        -- See Note [copy data files hack]
        lists <- glob (src </> "*/memory.list")
        forM_ lists $ \memFile ->
          copyFile memFile (src </> "memory.list")

        -- TODO: remove -voptargs=+acc=p for a next release of questa intel edition
        let args = ["-voptargs=+acc=p","-batch", "-do", doScript, msimTop]
        case msimExpectFailure of
          Nothing -> run optionSet (vsim src args) progressCallback
          Just exit -> run optionSet (failingVsim src args exit) progressCallback

    | otherwise =
        pure (testPassed "Ignoring test due to --no-modelsim")
   where
    vsim workDir args =
      TestProgram "vsim" args NoGlob PrintNeither False (Just workDir) []

    failingVsim workDir args (testExit, expectedErr) =
      TestFailingProgram
        (testExitCode testExit) "vsim" args NoGlob PrintNeither False
        (specificExitCode testExit) (ExpectEither expectedErr) (Just workDir) []

    doScript = List.intercalate ";"
      [ "run -all"
      , unwords
         ["if {[string equal ready [runStatus]]}"
         ,"then {quit -f}"
         ,"else {quit -code 1 -f}"
         ]
      , "quit -code 2 -f"
      ]

  testOptions =
    coerce (coerce (testOptions @TestProgram) <> [Option (Proxy @ModelSim)])
