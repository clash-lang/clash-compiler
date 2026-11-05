{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

module Test.Tasty.Quartus where

import Data.Coerce (coerce)
import qualified Data.List as List
import Data.Maybe (listToMaybe, mapMaybe)
import Data.Proxy (Proxy (..))
import Data.String.Interpolate (__i)
import Data.Tagged (Tagged (..))
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import System.FilePath ((</>), (<.>), takeExtension)
import System.Process (readProcess)
import Test.Tasty.Options
  (IsOption (..), OptionDescription (..), flagCLParser, lookupOption, safeReadBool)
import Test.Tasty.Providers (IsTest (..), testFailed, testPassed)
import Test.Tasty.Runners (resultSuccessful)
import Text.Read (readMaybe)

import Test.Tasty.Common
import Test.Tasty.Program

-- | @--no-quartus@ flag for disabling tests that use Quartus
newtype Quartus = Quartus Bool

instance IsOption Quartus where
  defaultValue = Quartus True
  parseValue = fmap Quartus . safeReadBool
  optionName = pure "no-quartus"
  optionHelp = pure "Skip Quartus tests"
  optionCLParser = flagCLParser Nothing (Quartus False)

-- | Synthesize a top entity for an Intel Cyclone 10 GX part with Quartus Prime
-- Pro, and check rows of the resource usage summary of the synthesis report.
-- Passes without doing anything for other editions of Quartus: they do not
-- support this part, nor the SystemVerilog Clash generates.
data QuartusSynthTest = QuartusSynthTest
  { qstParentDir :: IO FilePath
  , qstHdlDir :: IO FilePath
  , qstTopEntity :: Text
  , qstResources :: [(Text, Int)]
    -- ^ Expected values of rows in the resource usage summary, such as
    -- @("Dedicated logic registers", 0)@
  }

instance IsTest QuartusSynthTest where
  run optionSet QuartusSynthTest{..} progressCallback
    | Quartus True <- lookupOption optionSet = do
        version <- readProcess "quartus_sh" ["--version"] ""
        if not ("Pro Edition" `List.isInfixOf` version) then
          pure (testPassed "Ignoring test: it needs Quartus Prime Pro")
        else do
          buildTargetDir qstParentDir qstHdlDir
          dir <- qstHdlDir
          files <- getHdlFiles dir qstTopEntity
          let
            top = T.unpack qstTopEntity
            setupFp = dir </> "setup.tcl"
            addFiles = unlines (mapMaybe addFile files)
          writeFile setupFp [__i|
            project_new #{top} -overwrite
            set_global_assignment -name FAMILY "Cyclone 10 GX"
            set_global_assignment -name DEVICE 10CX085YU484E6G
            set_global_assignment -name TOP_LEVEL_ENTITY #{top}
            set_global_assignment -name NUM_PARALLEL_PROCESSORS 1
            #{addFiles}
            project_close
            |]
          setup <- run optionSet (quartus dir "quartus_sh" ["-t", setupFp]) progressCallback
          if not (resultSuccessful setup) then pure setup else do
            synth <- run optionSet (quartus dir "quartus_syn" [top]) progressCallback
            if not (resultSuccessful synth) then pure synth else do
              report <- T.readFile (dir </> top <.> "syn.rpt")
              pure (checkResources report)
    | otherwise = pure (testPassed "Ignoring test due to --no-quartus")
   where
    quartus workDir program args =
      TestProgram program args NoGlob PrintNeither False (Just workDir) []

    addFile f = case takeExtension f of
      ".vhdl" -> Just (assignment "VHDL_FILE")
      ".v" -> Just (assignment "VERILOG_FILE")
      ".sv" -> Just (assignment "SYSTEMVERILOG_FILE")
      _ -> Nothing
     where
      assignment name = "set_global_assignment -name " <> name <> " {" <> f <> "}"

    checkResources report =
      case mapMaybe (checkResource report) qstResources of
        [] -> testPassed ""
        errs -> testFailed (unlines errs)

    checkResource report (name, expected) =
      case reportRow report name of
        Just actual | actual == expected -> Nothing
        actual -> Just $
          "Expected " <> T.unpack name <> " to be " <> show expected
            <> ", but the synthesis report says " <> maybe "nothing" show actual

  testOptions =
    coerce (coerce (testOptions @TestProgram) <> [Option (Proxy @Quartus)])

-- | Value of the first row in a Quartus report table with a given name. Rows
-- look like @; Dedicated logic registers ; 0 ;@.
reportRow :: Text -> Text -> Maybe Int
reportRow report name = listToMaybe
  [ n
  | line <- T.lines report
  , _ : key : value : _ <- [map T.strip (T.splitOn ";" line)]
  , key == name
  , Just n <- [readMaybe (T.unpack (T.filter (/= ',') value))]
  ]
