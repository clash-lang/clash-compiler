-- | Utility functions to generate Primitives
module CLaSH.Primitives.Util where

import           Data.Aeson             (FromJSON, Result (..), fromJSON, json)
import qualified Data.Attoparsec.Lazy   as L
import           Data.ByteString.Lazy   (ByteString)
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

-- | Parse a ByteString according to the given JSON template. Prints failures
-- on @stdout@, and returns 'Nothing' if parsing fails.
decodeAndReport :: (FromJSON a)
                => ByteString -- ^ Bytestring to parse
                -> Maybe a
decodeAndReport s =
  case L.parse json s of
    L.Done _ v -> case fromJSON v of
                    Success a -> Just a
                    Error msg -> traceIf True msg Nothing
    L.Fail _ _ msg -> traceIf True msg Nothing
