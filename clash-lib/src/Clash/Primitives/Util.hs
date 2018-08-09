{-|
  Copyright  :  (C) 2012-2016, University of Twente,
                    2017     , Myrtle Software Ltd
                    2018     , Google Inc.
  License    :  BSD2 (see the file LICENSE)
  Maintainer :  Christiaan Baaij <christiaan.baaij@gmail.com>

  Utility functions to generate Primitives
-}

{-# LANGUAGE RecordWildCards #-}

module Clash.Primitives.Util (generatePrimMap) where

import           Data.Aeson.Extra       (decodeOrErr)
import qualified Data.ByteString.Lazy   as LZ
import qualified Data.HashMap.Lazy      as HashMap
import           Data.Maybe             (fromMaybe)
import           Data.List              (isSuffixOf)
import           Data.Text.Lazy         (Text)
import qualified Data.Text.Lazy.IO      as T
import           Data.Traversable       (mapM)
import           GHC.Stack              (HasCallStack)
import qualified System.Directory       as Directory
import qualified System.FilePath        as FilePath
import           System.IO.Error        (tryIOError)

import           Clash.Primitives.Types

resolveTemplateSource
  :: HasCallStack
  => FilePath
  -> TemplateSource
  -> IO Text
resolveTemplateSource _metaPath (TInline text) =
  return text
resolveTemplateSource metaPath (TFile path) =
  let path' = FilePath.replaceFileName metaPath path in
  either (error . show) id <$> (tryIOError $ T.readFile path')

-- | Replace file pointers with file contents
resolvePrimitive'
  :: HasCallStack
  => FilePath
  -> UnresolvedPrimitive
  -> IO ResolvedPrimitive
resolvePrimitive' _metaPath (Primitive name primType) =
  return $ Primitive name primType
resolvePrimitive' metaPath BlackBox{template=t, includes=i, ..} = do
  let resolvedIncludes = mapM (traverse (traverse (traverse (resolveTemplateSource metaPath)))) i
      resolved         = traverse (traverse (resolveTemplateSource metaPath)) t
  BlackBox name kind outputReg libraries imports <$> resolvedIncludes <*> resolved
resolvePrimitive' metaPath (BlackBoxHaskell bbName funcName t) =
  BlackBoxHaskell bbName funcName <$> (mapM (resolveTemplateSource metaPath) t)

-- | Interprets contents of json file as list of @Primitive@s.
resolvePrimitive
  :: HasCallStack
  => FilePath
  -> IO [ResolvedPrimitive]
resolvePrimitive fileName = do
  let decode = fromMaybe [] . decodeOrErr fileName
  prims <- decode <$> LZ.readFile fileName
  mapM (resolvePrimitive' fileName) prims

-- | Generate a set of primitives that are found in the primitive definition
-- files in the given directories.
generatePrimMap
  :: HasCallStack
  => [FilePath]
  -- ^ Directories to search for primitive definitions
  -> IO ResolvedPrimMap
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

  primitives <- fmap concat $ mapM resolvePrimitive primitiveFiles
  return $ HashMap.fromList $ zip (map name primitives) primitives
