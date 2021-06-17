{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

module Test.Tasty.SymbiYosys(SbyVerificationTest(..)) where

import           Clash.Driver.Manifest     (Manifest(..), manifestFilename)
import           Data.Coerce               (coerce)
import qualified Data.Text                 as T
import           System.FilePath

import           Test.Tasty.Common
import           Test.Tasty.Program
import           Test.Tasty.Providers

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
  run optionSet SbyVerificationTest {..} progressCallback = do
    src                             <- svtSourceDirectory
    [(manifestFile, Manifest {..})] <- getManifests
      (src </> "*" </> manifestFilename)
    let path    = takeDirectory manifestFile
        sbyFile = path </> T.unpack topComponent <.> "sby"
        sourceFiles =
          fmap (path </>)
            . filter ((`elem` [".v", ".sv"]) . takeExtension)
            . fmap fst
            $ fileNames
    writeFile sbyFile $ makeSbyFile (T.unpack topComponent) sourceFiles
    let args    = ["-f", sbyFile]
    let sbyTest = TestProgram "sby" args NoGlob PrintNeither False (Just src)
    let sbyTestFail (testExit, expectedErr) = TestFailingProgram
          (testExitCode testExit)
          "sby"
          args
          NoGlob
          PrintNeither
          False
          (specificExitCode testExit)
          (ExpectEither expectedErr)
          (Just src)
    case svtExpectFail of
      Nothing   -> run optionSet sbyTest progressCallback
      Just exit -> run optionSet (sbyTestFail exit) progressCallback
   where
  testOptions = coerce (testOptions @TestProgram)

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
