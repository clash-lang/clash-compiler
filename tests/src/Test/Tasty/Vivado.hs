{-# LANGUAGE TypeApplications #-}

module Test.Tasty.Vivado where

import Data.Coerce (coerce)
import Data.Tagged (Tagged (..))
import Data.Proxy (Proxy (..))
import System.IO (hPutStr, hFlush)

import System.IO.Temp (withSystemTempFile)
import Test.Tasty.Providers (IsTest (..), testPassed)
import Test.Tasty.Options (IsOption (..), safeReadBool, flagCLParser, lookupOption, OptionDescription (..))

import Clash.Vivado (fromModuleName, HdlSource (..))
import Clash.Annotations.Primitive (HDL (..))
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
  { hdlTarget :: HDL
  , hdlSource :: IO FilePath
  , moduleTest :: String
  , entity :: String
  }

instance IsTest VivadoTest where
  run optionSet (VivadoTest target dir m top) progressCallback
    | Vivado True <- lookupOption optionSet =
        withSystemTempFile "sim.tcl" $ \fp h -> do
          hdlSrc <- dir
          tcl <- fromModuleName (HdlSource hdlSrc target) m top
          hPutStr h tcl *>
            hFlush h
          runVivado hdlSrc ["-mode", "batch", "-source", fp]
    | otherwise = pure (testPassed "Ignoring test due to --no-vivado")

   where
    vivado workDir args =
      TestFailingProgram True "vivado" args NoGlob PrintNeither False (Just 0) (ExpectNotStdOut "expected: ") (Just workDir)
    runVivado workDir args =
      run optionSet (vivado workDir args) progressCallback

  testOptions =
    coerce (coerce (testOptions @TestProgram) <> [Option (Proxy @Vivado)])
