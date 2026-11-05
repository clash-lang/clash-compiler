{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

module Test.Tasty.Yosys where

import Data.Coerce (coerce)
import Data.Proxy (Proxy (..))
import Data.String.Interpolate (__i)
import Data.Tagged (Tagged (..))
import Data.Text (Text)
import qualified Data.Text as T
import System.FilePath ((</>), takeExtension)
import Test.Tasty.Options
  (IsOption (..), OptionDescription (..), flagCLParser, lookupOption, safeReadBool)
import Test.Tasty.Providers (IsTest (..), testPassed)

import Test.Tasty.Common
import Test.Tasty.Program

-- | @--no-yosys@ flag for disabling tests that use Yosys
newtype Yosys = Yosys Bool

instance IsOption Yosys where
  defaultValue = Yosys True
  parseValue = fmap Yosys . safeReadBool
  optionName = pure "no-yosys"
  optionHelp = pure "Skip Yosys tests"
  optionCLParser = flagCLParser Nothing (Yosys False)

-- | Synthesize a top entity for a Xilinx 7-series part with Yosys'
-- @synth_xilinx@ and check how many cells of each type synthesis produced.
-- Only supports Verilog: Yosys cannot read VHDL, nor the SystemVerilog Clash
-- generates for most types.
data YosysSynthTest = YosysSynthTest
  { ystParentDir :: IO FilePath
  , ystHdlDir :: IO FilePath
  , ystTopEntity :: Text
  , ystCells :: [(Text, Int)]
    -- ^ Expected number of cells per cell type. Cell types are glob patterns,
    -- such as @"FD*"@.
  }

instance IsTest YosysSynthTest where
  run optionSet YosysSynthTest{..} progressCallback
    | Yosys True <- lookupOption optionSet = do
        buildTargetDir ystParentDir ystHdlDir
        dir <- ystHdlDir
        files <- filter ((== ".v") . takeExtension) <$> getHdlFiles dir ystTopEntity
        let
          scriptFp = dir </> "synth.ys"
          readFiles = unlines ["read_verilog -defer " <> f | f <- files]
          checks = unlines
            [ "select -assert-count " <> show n <> " t:" <> T.unpack ty
            | (ty, n) <- ystCells ]
        writeFile scriptFp [__i|
          #{readFiles}
          synth_xilinx -family xc7 -flatten -top #{ystTopEntity}
          #{checks}
          |]
        run optionSet (yosys dir ["-q", "-s", scriptFp]) progressCallback
    | otherwise = pure (testPassed "Ignoring test due to --no-yosys")
   where
    yosys workDir args =
      TestProgram "yosys" args NoGlob PrintNeither False (Just workDir) []

  testOptions =
    coerce (coerce (testOptions @TestProgram) <> [Option (Proxy @Yosys)])
