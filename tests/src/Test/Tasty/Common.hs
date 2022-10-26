module Test.Tasty.Common where

import           Control.Monad.Extra       (forM_, ifM)
import           Clash.Driver.Manifest     (readManifest, Manifest(..))
import           Data.Default              (Default, def)
import           Data.Maybe                (fromMaybe)
import           System.Directory
  (copyFile, createDirectory, doesDirectoryExist, listDirectory)
import           System.FilePath           ((</>))
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

buildTargetDir
  :: IO FilePath
  -> IO FilePath
  -> IO ()
buildTargetDir parentDir targetDir = do
  hdlDir <- fmap (</> "hdl") parentDir
  targetDir1 <- targetDir
  copyDir hdlDir targetDir1
 where
  -- Note it doesn't handle symlinks as we don't have any anyway
  copyDir src dst = do
    createDirectory dst
    es <- listDirectory src
    forM_ es $ \e -> do
      let srcE = src </> e
          dstE = dst </> e
      ifM (doesDirectoryExist srcE)
        (copyDir srcE dstE)
        (copyFile srcE dstE)
