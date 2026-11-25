module Test.Tasty.Common where

import           Control.Monad.Extra       (forM, forM_, ifM)
import           Clash.Driver.Manifest     (readManifest, Manifest(..), manifestFilename)
import           Data.Default              (Default, def)
import           Data.Maybe                (fromMaybe)
import qualified Data.Text                 as T
import           System.Directory
  (copyFile, createDirectory, doesDirectoryExist, listDirectory)
import           System.FilePath           ((</>), takeDirectory)
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

-- | All HDL files needed to synthesize a top entity, in compilation order: the
-- files of its dependencies come first.
getHdlFiles ::
  -- | HDL directory, containing a directory per top entity
  FilePath ->
  -- | Name of the top entity, as in 'topComponent'
  T.Text ->
  IO [FilePath]
getHdlFiles hdlDir top = do
  manifests <- getManifests (hdlDir </> "*" </> manifestFilename)
  case filter ((== top) . topComponent . snd) manifests of
    [(manifestFile, manifest)] -> do
      deps <- forM (transitiveDependencies manifest) $ \dep ->
        getManifests (hdlDir </> T.unpack dep </> manifestFilename)
      pure (concatMap files (concat deps <> [(manifestFile, manifest)]))
    _ -> error ("Expected exactly one manifest for top entity " <> show top)
 where
  files (manifestFile, manifest) =
    map ((takeDirectory manifestFile </>) . fst) (fileNames manifest)

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
