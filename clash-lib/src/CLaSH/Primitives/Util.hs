-- | Utility functions to generate Primitives
module CLaSH.Primitives.Util where

import           Data.Aeson.Extra       (decodeAndReport)
import qualified Data.ByteString.Lazy   as LZ
import qualified Data.HashMap.Lazy      as HashMap
import           Data.List              (isSuffixOf)
import           Data.Maybe             (fromMaybe)
import qualified System.Directory       as Directory
import qualified System.FilePath        as FilePath

import           CLaSH.Primitives.Types
import           CLaSH.Util

-- | Generate a set of primitives that are found in the primitive definition
-- files in the given directories.
generatePrimMap :: [FilePath] -- ^ Directories to search for primitive definitions
                -> IO PrimMap
generatePrimMap filePaths = do
  primitiveFiles <- fmap concat $ mapM
                      (\filePath ->
                          fmap ( map (FilePath.combine filePath)
                               . filter (isSuffixOf ".json")
                               ) (Directory.getDirectoryContents filePath)
                      ) filePaths

  primitives <- fmap concat $ mapM
                  ( return
                  . fromMaybe []
                  . decodeAndReport
                  <=< LZ.readFile
                  ) primitiveFiles

  let primMap = HashMap.fromList $ zip (map name primitives) primitives

  return primMap
