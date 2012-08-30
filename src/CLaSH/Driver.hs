{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE CPP #-}
module CLaSH.Driver where

import qualified Data.ByteString.Lazy         as LZ
import           Data.Maybe                   (fromMaybe)
import qualified Control.Concurrent.Supply    as Supply
import qualified Data.HashMap.Lazy            as HashMap
import           Data.List                    (isSuffixOf)
import qualified System.Directory             as Directory
import qualified System.FilePath              as FilePath
import qualified System.IO                    as IO
import           Text.PrettyPrint.Leijen.Text (Doc,hPutDoc)
import           Unbound.LocallyNameless      (name2String)

import           CLaSH.Core.Term              (TmName)
import           CLaSH.Driver.PrepareBinding
import           CLaSH.Netlist                (genNetlist)
import           CLaSH.Netlist.VHDL           (genVHDL)
import           CLaSH.Normalize              (runNormalization, normalize)
import           CLaSH.Primitives.Types
import           CLaSH.Primitives.Util
import           CLaSH.Rewrite.Types          (DebugLevel(..))
import           CLaSH.Util

#ifdef CABAL
import           Paths_clash
#else
getDataFileName :: FilePath -> IO FilePath
getDataFileName = return . ("../" ++)
#endif

generateVHDL ::
  String
  -> IO ()
generateVHDL modName = do
  primitiveDir   <- getDataFileName "primitives"
  primitiveFiles <- fmap (filter (isSuffixOf ".json")) $
                      Directory.getDirectoryContents primitiveDir

  let primitiveFiles' = map (FilePath.combine primitiveDir) primitiveFiles

  primitives <- fmap concat $ mapM
                  ( return
                  . fromMaybe []
                  . decodeAndReport
                  <=< LZ.readFile
                  ) primitiveFiles'

  let primMap = HashMap.fromList $ zip (map name primitives) primitives

  (bindingsMap,dfunMap,clsOpMap) <- prepareBinding primMap modName

  let topEntities = HashMap.toList
                  $ HashMap.filterWithKey isTopEntity bindingsMap

  case topEntities of
    [topEntity] -> do
      let bindingsMap' = HashMap.map snd bindingsMap
      supply <- Supply.newSupply
      let transformedBindings
            = runNormalization DebugFinal supply bindingsMap' dfunMap clsOpMap
            $ normalize [fst topEntity]

      netlist <- genNetlist (HashMap.fromList $ transformedBindings)
                            primMap
                            (fst topEntity)

      let dir = "./vhdl/" ++ (fst $ snd topEntity) ++ "/"
      prepareDir dir
      mapM_ (writeVHDL dir . genVHDL) netlist

    [] -> error $ $(curLoc) ++ "No 'topEntity' found"
    _  -> error $ $(curLoc) ++ "Multiple 'topEntity's found"

isTopEntity ::
  TmName
  -> a
  -> Bool
isTopEntity var _ = name2String var == "topEntity"

-- | Prepares the directory for writing VHDL files. This means creating the
--   dir if it does not exist and removing all existing .vhdl files from it.
prepareDir :: String -> IO ()
prepareDir dir = do
  -- Create the dir if needed
  Directory.createDirectoryIfMissing True dir
  -- Find all .vhdl files in the directory
  files <- Directory.getDirectoryContents dir
  let to_remove = filter ((==".vhdl") . FilePath.takeExtension) files
  -- Prepend the dirname to the filenames
  let abs_to_remove = map (FilePath.combine dir) to_remove
  -- Remove the files
  mapM_ Directory.removeFile abs_to_remove

writeVHDL :: FilePath -> (String, Doc) -> IO ()
writeVHDL dir (cname, vhdl) = do
  -- Find the filename
  let fname = dir ++ cname ++ ".vhdl"
  -- Write the file
  handle <- IO.openFile fname IO.WriteMode
  IO.hPutStrLn handle "-- Automatically generated VHDL"
  hPutDoc handle vhdl
  IO.hClose handle
