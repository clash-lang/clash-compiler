{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

module Test.Tasty.Vivado where

import Data.Coerce (coerce)
import Data.String.Interpolate (__i)
import Data.Text (Text)
import Data.Tagged (Tagged (..))
import Data.Proxy (Proxy (..))

import System.Directory (createDirectory)
import System.FilePath ((</>), dropFileName)
import Test.Tasty.Providers (IsTest (..), testPassed)
import Test.Tasty.Options (IsOption (..), safeReadBool, flagCLParser, lookupOption, OptionDescription (..))
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
        runVivado dir ["-mode", "batch", "-source", tclFp]
    | otherwise = pure (testPassed "Ignoring test due to --no-vivado")

   where
    vivado workDir args =
      TestFailingProgram True "vivado" args NoGlob PrintNeither False (Just 0)
        (ExpectNotMatchStdOut re) (Just workDir)
        -- Without XILINX_LOCAL_USER_DATA=no, concurrently running instances of
        -- Vivado might error out while accessing the Xilinx Tcl App Store at
        -- ~/.Xilinx. https://support.xilinx.com/s/article/63253
        [("XILINX_LOCAL_USER_DATA", "no")]
      where re = either error id
              (compile defaultCompOpt (ExecOption False) "^\\s*(@|(Error))")
    runVivado workDir args =
      run optionSet (vivado workDir args) progressCallback

  testOptions =
    coerce (coerce (testOptions @TestProgram) <> [Option (Proxy @Vivado)])

genSimTcl ::
  -- | HDL directory
  FilePath ->
  -- | Top entity
  Text ->
  -- | TCL script
  IO String
genSimTcl dir top = do
  connector <- tclConnector
  manifests <- getManifests (dir </> "*" </> manifestFilename)
  let [(dropFileName -> topEntityDir, _)] =
        filter ((== top) . topComponent . snd) manifests
  pure [__i|
    set_msg_config -severity {CRITICAL WARNING} -new_severity ERROR
    source -notrace {#{connector}}
    clash::readMetadata {#{topEntityDir}}
    clash::createAndReadIp -dir ip
    clash::readHdl
    \# Compiler doesn't topologically sort source files (bug)
    update_compile_order -fileset [current_fileset]
    set_property TOP $clash::topEntity [current_fileset -sim]
    save_project_as sim project -force
    set_property RUNTIME all [current_fileset -sim]
    launch_simulation
    |]
