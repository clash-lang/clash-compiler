{-# LANGUAGE TypeApplications #-}

module Test.Tasty.Vivado where

import Control.Monad (unless)
import Data.Coerce (coerce)
import Data.Tagged (Tagged (..))
import Data.Proxy (Proxy (..))

import System.Directory (createDirectory, doesDirectoryExist)
import System.FilePath ((</>))
import Test.Tasty.Providers (IsTest (..), testPassed)
import Test.Tasty.Options (IsOption (..), safeReadBool, flagCLParser, lookupOption, OptionDescription (..))
import Text.Regex.TDFA (ExecOption (..), defaultCompOpt)
import Text.Regex.TDFA.Text (compile)

import Test.Tasty.Program
import Test.Tasty.Vivado.GenTcl (tclFromManifest)

-- | @--vivado@ flag for enabling tests that use vivado.
newtype Vivado = Vivado Bool

instance IsOption Vivado where
  defaultValue = Vivado True
  parseValue = fmap Vivado . safeReadBool
  optionName = pure "no-vivado"
  optionHelp = pure "Skip Vivado tests"
  optionCLParser = flagCLParser Nothing (Vivado False)

data VivadoTest = VivadoTest
  { hdlSource :: IO FilePath
  , moduleTest :: String
  , entity :: String
  }

createDirIfNotExists :: FilePath -> IO ()
createDirIfNotExists fp = do
  b <- doesDirectoryExist fp
  unless b (createDirectory fp)

instance IsTest VivadoTest where
  run optionSet (VivadoTest dir m top) progressCallback
    | Vivado True <- lookupOption optionSet = do
        hdlDir <- dir
        createDirIfNotExists $ hdlDir </> "ip"
        createDirIfNotExists $ hdlDir </> "project"
        let tclFp = hdlDir </> "sim.tcl"
        tcl <- tclFromManifest hdlDir m top
        writeFile tclFp tcl
        runVivado hdlDir ["-mode", "batch", "-source", tclFp]
    | otherwise = pure (testPassed "Ignoring test due to --no-vivado")

   where
    vivado workDir args =
      TestFailingProgram True "vivado" args NoGlob PrintNeither False (Just 0)
        (ExpectNotMatchStdOut re) (Just workDir) [("XILINX_LOCAL_USER_DATA", "no")]
      where re = either error id
              (compile defaultCompOpt (ExecOption False) "^\\s*(@|(Error))")
    runVivado workDir args =
      run optionSet (vivado workDir args) progressCallback

  testOptions =
    coerce (coerce (testOptions @TestProgram) <> [Option (Proxy @Vivado)])
