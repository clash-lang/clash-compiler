{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

module Test.Tasty.SymbiYosys(SbyVerificationTest(..)) where

import           Clash.Driver.Manifest     (Manifest(..), manifestFilename)
import           Data.Coerce               (coerce)
import           Data.Proxy
import           Data.Tagged
import qualified Data.Text                 as T
import           System.FilePath

import           Test.Tasty.Common
import           Test.Tasty.Options
import           Test.Tasty.Program
import           Test.Tasty.Providers

-- | @--symbiyosys@ flag for enabling tests that use symbiyosys.
newtype Symbiyosys = Symbiyosys Bool
  deriving (Eq, Ord)

instance IsOption Symbiyosys where
  defaultValue = Symbiyosys True
  parseValue = fmap Symbiyosys . safeReadBool
  optionName = pure "no-symbiyosys"
  optionHelp = pure "Skip symbiyosys tests"
  optionCLParser = flagCLParser Nothing (Symbiyosys False)

-- | Run SymbiYosys on some Clash generated HDL
data SbyVerificationTest = SbyVerificationTest
  { svtExpectFail :: Maybe (TestExitCode, T.Text)
    -- ^ Expected failure code and output (if any)
  , svtSourceDirectory :: IO FilePath
    -- ^ Directory containing files produced by Clash
  , svtTop :: String
    -- ^ Entry point to be verified
  }

instance IsTest SbyVerificationTest where
  run optionSet SbyVerificationTest {..} progressCallback
    | Symbiyosys True <- lookupOption optionSet = do
        src <- svtSourceDirectory
        [(manifestFile, Manifest {..})] <- getManifests (src </> "*" </> manifestFilename)

        let path    = takeDirectory manifestFile
            sbyFile = path </> T.unpack topComponent <.> "sby"
            sourceFiles =
                  fmap (path </>)
                    . filter ((`elem` [".v", ".sv"]) . takeExtension)
                    . fmap fst
                    $ fileNames

        writeFile sbyFile $ makeSbyFile (T.unpack topComponent) sourceFiles

        let args    = ["-f", sbyFile]
        case svtExpectFail of
          Nothing   -> run optionSet (sby src args) progressCallback
          Just exit -> run optionSet (failingSby src args exit) progressCallback

    | otherwise =
        pure (testPassed "Ignoring test due to --no-symbiyosys")
   where
    sby workDir args = TestProgram "sby" args NoGlob PrintNeither False (Just workDir) []

    failingSby workDir args (testExit, expectedErr) =
      TestFailingProgram
        (testExitCode testExit) "sby" args NoGlob PrintNeither False
        (specificExitCode testExit) (ExpectEither expectedErr) (Just workDir) []

  testOptions =
    coerce (coerce (testOptions @TestProgram) <> [Option (Proxy @Symbiyosys)])

makeSbyFile :: String -> [FilePath] -> String
makeSbyFile top files = unlines
  [ "[options]"
  , "mode prove"
  , "mode cover"
  , ""
  , "[engines]"
  , "smtbmc z3"
  , ""
  , "[script]"
  , "read -formal " <> unwords (takeFileName <$> files)
  , "prep -top " <> top
  , ""
  , "[files]"
  , unlines files
  ]
