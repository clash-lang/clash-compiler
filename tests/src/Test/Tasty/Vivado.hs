{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

module Test.Tasty.Vivado where

import Data.Coerce (coerce)
import Data.String.Interpolate (__i)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Tagged (Tagged (..))
import Data.Proxy (Proxy (..))

import System.Directory (createDirectory)
import System.FilePath ((</>), dropFileName)
import Test.Tasty.Providers (IsTest (..), Progress, Result, testPassed)
import Test.Tasty.Options (IsOption (..), OptionSet, safeReadBool, flagCLParser, lookupOption, OptionDescription (..))
import Text.Regex.TDFA (ExecOption (..), defaultCompOpt)
import Text.Regex.TDFA.Text (compile)

import Clash.Driver.Manifest (topComponent, manifestFilename)
import Clash.DataFiles (tclConnector)

import Test.Tasty.Common
import Test.Tasty.Program

-- | @--vivado@ flag for enabling tests that use vivado.
newtype Vivado = Vivado Bool

instance IsOption Vivado where
  defaultValue = Vivado True
  parseValue = fmap Vivado . safeReadBool
  optionName = pure "no-vivado"
  optionHelp = pure "Skip Vivado tests"
  optionCLParser = flagCLParser Nothing (Vivado False)

data VivadoTest = VivadoTest
  { parentDir :: IO FilePath
  , hdlDir :: IO FilePath
  , topEntity :: Text
  }

instance IsTest VivadoTest where
  run optionSet VivadoTest{..} progressCallback
    | Vivado True <- lookupOption optionSet = do
        buildTargetDir parentDir hdlDir
        dir <- hdlDir
        createDirectory $ dir </> "ip"
        createDirectory $ dir </> "project"
        let tclFp = dir </> "sim.tcl"
        tcl <- genSimTcl dir topEntity
        writeFile tclFp tcl
        runVivado optionSet dir ["-mode", "batch", "-source", tclFp] progressCallback
    | otherwise = pure (testPassed "Ignoring test due to --no-vivado")

  testOptions =
    coerce (coerce (testOptions @TestProgram) <> [Option (Proxy @Vivado)])

-- | Synthesize a top entity for a Xilinx 7-series part and check how many
-- primitive cells of each type synthesis produced. Designs that instantiate
-- Xilinx IP are not supported.
data VivadoSynthTest = VivadoSynthTest
  { vstParentDir :: IO FilePath
  , vstHdlDir :: IO FilePath
  , vstTopEntity :: Text
  , vstCells :: [(Text, Int)]
    -- ^ Expected number of cells per cell type. Cell types are glob patterns,
    -- such as @"FD*"@.
  }

instance IsTest VivadoSynthTest where
  run optionSet VivadoSynthTest{..} progressCallback
    | Vivado True <- lookupOption optionSet = do
        buildTargetDir vstParentDir vstHdlDir
        dir <- vstHdlDir
        let tclFp = dir </> "synth.tcl"
        tcl <- genSynthTcl dir vstTopEntity vstCells
        writeFile tclFp tcl
        runVivado optionSet dir ["-mode", "batch", "-source", tclFp] progressCallback
    | otherwise = pure (testPassed "Ignoring test due to --no-vivado")

  testOptions =
    coerce (coerce (testOptions @TestProgram) <> [Option (Proxy @Vivado)])

runVivado ::
  OptionSet ->
  -- | Work directory
  FilePath ->
  -- | Arguments
  [String] ->
  (Progress -> IO ()) ->
  IO Result
runVivado optionSet workDir args =
  run optionSet $
    TestFailingProgram True "vivado" args NoGlob PrintNeither False (Just 0)
      (ExpectNotMatchStdOut re) (Just workDir)
      -- Without XILINX_LOCAL_USER_DATA=no, concurrently running instances of
      -- Vivado might error out while accessing the Xilinx Tcl App Store at
      -- ~/.Xilinx. https://support.xilinx.com/s/article/63253
      [("XILINX_LOCAL_USER_DATA", "no")]
 where
  re = either error id
    (compile defaultCompOpt (ExecOption False) "^\\s*(@|(Error)|(FATAL_ERROR)|(The simulator has terminated in an unexpected manner))")

-- | Directory containing the HDL of a top entity
getTopEntityDir ::
  -- | HDL directory
  FilePath ->
  -- | Top entity
  Text ->
  IO FilePath
getTopEntityDir dir top = do
  manifests <- getManifests (dir </> "*" </> manifestFilename)
  pure $ case filter ((== top) . topComponent . snd) manifests of
    (x,_):_ -> dropFileName x
    _ -> error "topEntity not found in manifest"

genSimTcl ::
  -- | HDL directory
  FilePath ->
  -- | Top entity
  Text ->
  -- | TCL script
  IO String
genSimTcl dir top = do
  connector <- tclConnector
  topEntityDir <- getTopEntityDir dir top
  pure [__i|
    set_msg_config -severity {CRITICAL WARNING} -new_severity ERROR
    source -notrace {#{connector}}
    clash::readMetadata {#{topEntityDir}}
    clash::createAndReadIp -dir ip
    clash::readHdl
    set_property TOP $clash::topEntity [current_fileset -sim]
    set_property -name xsim.elaborate.xelab.more_options -value {-L xpm} -objects [current_fileset -sim]
    save_project_as sim project -force
    set_property RUNTIME all [current_fileset -sim]
    launch_simulation

    \# Uncomment if you want to dump all signals to /tmp/log.vcd
    \# restart
    \# open_vcd /tmp/log.vcd
    \# log_vcd /*
    \# run -all
    \# flush_vcd
    \# close_vcd
    |]

genSynthTcl ::
  -- | HDL directory
  FilePath ->
  -- | Top entity
  Text ->
  -- | Expected number of cells per cell type
  [(Text, Int)] ->
  -- | TCL script
  IO String
genSynthTcl dir top cells = do
  connector <- tclConnector
  topEntityDir <- getTopEntityDir dir top
  let expected = unwords [T.unpack ty <> " " <> show n | (ty, n) <- cells]
  pure [__i|
    set_msg_config -severity {CRITICAL WARNING} -new_severity ERROR
    source -notrace {#{connector}}
    clash::readMetadata {#{topEntityDir}}
    clash::readHdl
    synth_design -top $clash::topEntity -part xc7a35tcsg324-1 -mode out_of_context
    foreach {cellType expected} {#{expected}} {
      set actual [llength [get_cells -quiet -hierarchical -filter "IS_PRIMITIVE && REF_NAME =~ $cellType"]]
      if {$actual != $expected} {
        error "Expected $expected cells of type $cellType, but synthesis produced $actual"
      }
    }
    |]
