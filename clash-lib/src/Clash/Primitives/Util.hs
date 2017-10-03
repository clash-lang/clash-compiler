{-|
  Copyright  :  (C) 2012-2016, University of Twente,
                    2017     , Myrtle Software Ltd
  License    :  BSD2 (see the file LICENSE)
  Maintainer :  Christiaan Baaij <christiaan.baaij@gmail.com>

  Utility functions to generate Primitives
-}

module Clash.Primitives.Util where

import           Data.Aeson.Extra       (decodeAndReport)
import qualified Data.ByteString.Lazy   as LZ
import qualified Data.HashMap.Lazy      as HashMap
import           Data.List              (isSuffixOf)
import           Data.Maybe             (fromMaybe)
import           Data.Text.Lazy         (Text)
import qualified System.Directory       as Directory
import qualified System.FilePath        as FilePath

import           Clash.Primitives.Types
import           Clash.Util

-- | Generate a set of primitives that are found in the primitive definition
-- files in the given directories.
generatePrimMap :: [FilePath] -- ^ Directories to search for primitive definitions
                -> IO (PrimMap Text)
generatePrimMap filePaths = do
  primitiveFiles <- fmap concat $ mapM
     (\filePath -> do
         fpExists <- Directory.doesDirectoryExist filePath
         if fpExists
           then
             fmap ( map (FilePath.combine filePath)
                  . filter (isSuffixOf ".json")
                  ) (Directory.getDirectoryContents filePath)
           else
             return []
     ) filePaths

  primitives <- fmap concat $ mapM
                  ( return
                  . fromMaybe []
                  . decodeAndReport
                  <=< LZ.readFile
                  ) primitiveFiles

  let primMap = HashMap.fromList $ zip (map name primitives) primitives

  return primMap
