module CLaSH.Primitives.Util where

import           Data.Aeson           (FromJSON,fromJSON,json,Result(..))
import qualified Data.Attoparsec.Lazy as L
import           Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as LZ
import qualified Data.HashMap.Lazy    as HashMap
import           Data.List            (isSuffixOf)
import           Data.Maybe           (fromMaybe)
import qualified System.Directory     as Directory
import qualified System.FilePath      as FilePath

import CLaSH.Primitives.Types
import CLaSH.Util

generatePrimMap :: [FilePath] -> IO PrimMap
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

decodeAndReport ::
  (FromJSON a)
  => ByteString
  -> Maybe a
decodeAndReport s =
  case L.parse json s of
    L.Done _ v -> case fromJSON v of
                    Success a -> Just a
                    Error msg -> traceIf True msg Nothing
    L.Fail _ _ msg -> traceIf True msg Nothing
