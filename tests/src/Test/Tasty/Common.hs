module Test.Tasty.Common where

import           Clash.Driver.Manifest     (readManifest, Manifest(..))
import           Data.Default              (Default, def)
import           Data.Maybe                (fromMaybe)
import           System.FilePath.Glob      (glob)

data TestExitCode
  = TestExitCode
  | TestSpecificExitCode Int
  | NoTestExitCode

instance Default TestExitCode where
  def = TestExitCode


getManifests :: String -> IO [(FilePath, Manifest)]
getManifests pattern = mapM go =<< glob pattern
 where
  go :: FilePath -> IO (FilePath, Manifest)
  go path = do
    let err = error ("Failed to read/decode: " <> show path)
    manifest <- fromMaybe err <$> readManifest path
    pure (path, manifest)

testExitCode :: TestExitCode -> Bool
testExitCode TestExitCode = True
testExitCode (TestSpecificExitCode _) = True
testExitCode NoTestExitCode = False

specificExitCode :: TestExitCode -> Maybe Int
specificExitCode (TestSpecificExitCode n) = Just n
specificExitCode _ = Nothing
