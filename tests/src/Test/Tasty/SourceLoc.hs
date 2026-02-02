{-# LANGUAGE OverloadedStrings #-}
module Test.Tasty.SourceLoc (sourceLocTests) where

import qualified Data.List as List
import           Data.List (isInfixOf)
import qualified System.Directory as Directory
import           System.FilePath ((</>), takeFileName)
import           System.FilePath.Glob (glob)
import           System.IO.Temp (createTempDirectory, getCanonicalTemporaryDirectory)
import           Test.Tasty (TestName, TestTree, testGroup, withResource)
import           Test.Tasty.HUnit (assertBool, assertFailure, testCase)
import           Test.Tasty.Providers (singleTest)

import           Clash.Annotations.Primitive (HDL(Verilog))
import           Test.Tasty.Clash (ClashGenTest(..), sourceDirectory)

sourceLocTests :: [TestName] -> TestTree
sourceLocTests _parentNames =
  testGroup "SourceLocations"
    [ sourceLocTest "No -g strips SourceNote ticks" []
        (\v -> not ("SrcLocCounter.hs:" `isInfixOf` v))
    , sourceLocTest "-g keeps SourceNote ticks" ["-g"]
        (\v -> "SrcLocCounter.hs:" `isInfixOf` v)
    ]
 where
  srcDir = sourceDirectory </> "tests" </> "shouldwork" </> "SourceLocations"
  modName = "SrcLocTest"

  mkTmpDir =
    flip createTempDirectory ("clash-test_" <> modName) =<< getCanonicalTemporaryDirectory

  rmTmpDir = Directory.removeDirectoryRecursive

  sourceLocTest name extraGhcArgs predicate =
    withResource mkTmpDir rmTmpDir $ \tmpDir ->
      testGroup name
        [ singleTest "clash (gen)" (ClashGenTest
            { cgExpectFailure = Nothing
            , cgBuildTarget = Verilog
            , cgSourceDirectory = srcDir
            , cgExtraArgs = ["-fclash-no-cache"] <> extraGhcArgs
            , cgModName = modName
            , cgOutputDirectory = tmpDir
            , cgHdlDirectory = tmpDir
            })
        , testCase "check generated HDL" $ do
            hdlRoot <- tmpDir
            v <- readTopEntityV hdlRoot
            assertBool ("Unexpected HDL contents for: " <> name) (predicate v)
        ]

  readTopEntityV hdlRoot = do
    files <- glob (hdlRoot </> "**" </> "topEntity.v")
    case List.nub (filter ((== "topEntity.v") . takeFileName) files) of
      [vFile] -> readFile vFile
      [] -> assertFailure ("No `topEntity.v` found under " <> show hdlRoot)
      vFiles -> assertFailure ("Multiple `topEntity.v` found under " <> show hdlRoot <> ": " <> show vFiles)
