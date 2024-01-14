{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

module Test.Tasty.Iverilog where

import           Control.Monad             (forM_)
import           Data.Coerce               (coerce)
import           Data.Proxy
import           Data.Tagged
import qualified Data.Text                 as T
import           System.Directory          (listDirectory, copyFile)
import           System.FilePath           ((</>))
import           System.FilePath.Glob      (glob)

import           Test.Tasty.Common
import           Test.Tasty.Options
import           Test.Tasty.Program
import           Test.Tasty.Providers

-- | @--iverilog@ flag for enabling tests that use iverilog.
newtype Iverilog = Iverilog Bool
  deriving (Eq, Ord)

instance IsOption Iverilog where
  defaultValue = Iverilog True
  parseValue = fmap Iverilog . safeReadBool
  optionName = pure "no-iverilog"
  optionHelp = pure "Skip iverilog tests"
  optionCLParser = flagCLParser Nothing (Iverilog False)

-- | Make executable from Verilog produced by Clash using Icarus Verilog.
--
-- For example, for I2C it would execute:
--
-- > iverilog \
-- >   -I test_i2c -I test_bitmaster -I test_bytemaster \
-- >   -g2 -s test_i2c -o test_i2c.exe \
-- >   <verilog_files>
--
data IVerilogMakeTest = IVerilogMakeTest
  { ivmSourceDirectory :: IO FilePath
    -- ^ Directory containing VHDL files produced by Clash
  , ivmTop :: String
    -- ^ Entry point to be compiled
  }

instance IsTest IVerilogMakeTest where
  run optionSet IVerilogMakeTest{ivmSourceDirectory,ivmTop} progressCallback
    | Iverilog True <- lookupOption optionSet = do
        src <- ivmSourceDirectory
        libs <- listDirectory src
        verilogFiles <- glob (src </> "*" </> "*.v")
        runIcarus src (mkArgs libs verilogFiles ivmTop)

    | otherwise =
        pure (testPassed "Ignoring test due to --no-verilog")
   where
    mkArgs libs files top =
         concat [["-I", l] | l <- libs]
      <> ["-g2", "-s", top, "-o", top <> ".exe"]
      <> files

    icarus workDir args = TestProgram "iverilog" args NoGlob PrintNeither False (Just workDir)
    runIcarus workDir args = run optionSet (icarus workDir args) progressCallback

  testOptions =
    coerce (coerce (testOptions @TestProgram) <> [Option (Proxy @Iverilog)])

-- | Run executable produced by 'IverilogMakeTest'.
--
-- For example, for I2C it would execute:
--
-- > vvp test_i2c.exe
--
data IVerilogSimTest = IVerilogSimTest
  { ivsExpectFailure :: Maybe (TestExitCode, T.Text)
    -- ^ Expected failure code and output (if any)
  , ivsStdoutNonEmptyFail :: Bool
    -- ^ Whether a non-empty stdout means failure
  , ivsSourceDirectory :: IO FilePath
    -- ^ Directory containing executables produced by 'IVerilogMakeTest'
  , ivsTop :: String
    -- ^ Entry point to simulate
  }

instance IsTest IVerilogSimTest where
  run optionSet IVerilogSimTest{..} progressCallback
    | Iverilog True <- lookupOption optionSet = do
        src <- ivsSourceDirectory

        -- See Note [copy data files hack]
        lists <- glob (src </> "*/memory.list")
        forM_ lists $ \memFile ->
          copyFile memFile (src </> "memory.list")

        let topExe = ivsTop <> ".exe"
        case ivsExpectFailure of
          Nothing -> run optionSet (vvp src [topExe]) progressCallback
          Just exit -> run optionSet (failingVvp src [topExe] exit) progressCallback

    | otherwise =
        pure (testPassed "Ignoring test due to --no-iverilog")
   where
    vvp workDir args =
      TestProgram "vvp" args NoGlob PrintNeither ivsStdoutNonEmptyFail (Just workDir)

    failingVvp workDir args (testExit, expectedErr) =
      TestFailingProgram
        (testExitCode testExit) "vvp" args NoGlob PrintNeither False
        (specificExitCode testExit) (ExpectEither expectedErr) (Just workDir)

  testOptions =
    coerce (coerce (testOptions @TestProgram) <> [Option (Proxy @Iverilog)])
