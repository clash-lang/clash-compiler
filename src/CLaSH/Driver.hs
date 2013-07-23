{-# LANGUAGE CPP                 #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
module CLaSH.Driver where

import           Control.Monad                (unless)
import           Control.Monad.State          (evalState)
import qualified Data.ByteString.Lazy         as LZ
import           Data.Maybe                   (catMaybes,fromMaybe,listToMaybe)
import qualified Control.Concurrent.Supply    as Supply
import qualified Data.HashMap.Lazy            as HashMap
import           Data.List                    (isSuffixOf)
import           Data.Text.Lazy               (pack)
import qualified System.Directory             as Directory
import qualified System.FilePath              as FilePath
import qualified System.IO                    as IO
import           Text.PrettyPrint.Leijen.Text (Doc,hPutDoc,linebreak,punctuate,vcat)
import           Unbound.LocallyNameless      (name2String)

import           CLaSH.Core.Term              (TmName)
import           CLaSH.Driver.PrepareBinding
import           CLaSH.Driver.TestbenchGen
import           CLaSH.Netlist                (genNetlist)
import           CLaSH.Netlist.VHDL           (genVHDL)
import           CLaSH.Netlist.Types          (Component(..))
import           CLaSH.Normalize              (runNormalization, normalize, cleanupGraph)
import           CLaSH.Primitives.Types
import           CLaSH.Primitives.Util
import           CLaSH.Rewrite.Types          (DebugLevel(..))
import           CLaSH.Util

import qualified Data.Time.Clock as Clock

#ifdef CABAL

import           Paths_clash
getDefPrimDir :: IO FilePath
getDefPrimDir = getDataFileName "primitives"

#elif STANDALONE

import qualified Control.Exception as Exception
import System.Environment (getEnv)

getDefPrimDir :: IO FilePath
getDefPrimDir = catchIO (getEnv "clash_primdir") (error "Environment variable \"clash_primdir\" undefined")

catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
catchIO = Exception.catch

#else

getDefPrimDir :: IO FilePath
getDefPrimDir = return "../primitives"

#endif

generateVHDL ::
  String
  -> IO ()
generateVHDL modName = do
  start <- Clock.getCurrentTime

  primitiveDir   <- getDefPrimDir
  primitiveFiles <- fmap (filter (isSuffixOf ".json")) $
                      Directory.getDirectoryContents primitiveDir

  localPrimitives <- fmap (filter (isSuffixOf ".json")) $
                      Directory.getDirectoryContents "."

  let primitiveFiles' = map (FilePath.combine primitiveDir) primitiveFiles ++
                        localPrimitives

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

      testInputs  = HashMap.toList
                  $ HashMap.filterWithKey isTestInput bindingsMap

      expectedOutputs = HashMap.toList
                      $ HashMap.filterWithKey isExpectedOutput bindingsMap

  case topEntities of
    [topEntity] -> do
      let bindingsMap' = HashMap.map snd bindingsMap
      (supplyN,supplyTB) <- fmap Supply.splitSupply Supply.newSupply

      prepTime <- dfunMap `seq` Clock.getCurrentTime
      traceIf True ("Loading dependencies took " ++ show (Clock.diffUTCTime prepTime start)) $ return ()

      let transformedBindings
            = runNormalization DebugNone supplyN bindingsMap' dfunMap clsOpMap
            $ (normalize [fst topEntity]) >>= cleanupGraph [fst topEntity]

      normTime <- transformedBindings `seq` Clock.getCurrentTime
      traceIf True ("Normalisation took " ++ show (Clock.diffUTCTime normTime prepTime)) $ return ()

      (netlist,vhdlState) <- genNetlist Nothing (HashMap.fromList $ transformedBindings)
                              primMap
                              Nothing
                              (fst topEntity)

      netlistTime <- netlist `seq` Clock.getCurrentTime
      traceIf True ("Netlist generation took " ++ show (Clock.diffUTCTime netlistTime normTime)) $ return ()

      (testBench,vhdlState') <- genTestBench DebugNone supplyTB dfunMap clsOpMap primMap vhdlState
                                  bindingsMap'
                                  (listToMaybe $ map fst testInputs)
                                  (listToMaybe $ map fst expectedOutputs)
                                  (head $ filter (\(Component cName _ _ _ _) -> cName == (pack "topEntity_0")) netlist)

      testBenchTime <- testBench `seq` Clock.getCurrentTime
      traceIf True ("Testbench generation took " ++ show (Clock.diffUTCTime testBenchTime netlistTime)) $ return ()

      let dir = "./vhdl/" ++ (fst $ snd topEntity) ++ "/"
      prepareDir dir
      let (vhdlNms,typeDocsM,vhdlDocs) = unzip3 $ evalState (mapM genVHDL (netlist ++ testBench)) vhdlState'
      let typeDocs = catMaybes typeDocsM

      unless (null typeDocs) $ writeVHDL dir ("types", vcat $ punctuate linebreak typeDocs)
      mapM_ (writeVHDL dir) (zip vhdlNms vhdlDocs)

      end <- Clock.getCurrentTime
      traceIf True ("Total compilation took " ++ show (Clock.diffUTCTime end start)) $ return ()

    [] -> error $ $(curLoc) ++ "No 'topEntity' found"
    _  -> error $ $(curLoc) ++ "Multiple 'topEntity's found"

isTopEntity ::
  TmName
  -> a
  -> Bool
isTopEntity var _ = name2String var == "topEntity"

isTestInput ::
  TmName
  -> a
  -> Bool
isTestInput var _ = name2String var == "testInput"

isExpectedOutput ::
  TmName
  -> a
  -> Bool
isExpectedOutput var _ = name2String var == "expectedOutput"

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
  handle <- IO.openFile (dir ++ cname ++ ".vhdl") IO.WriteMode
  IO.hPutStrLn handle "-- Automatically generated VHDL"
  hPutDoc handle vhdl
  IO.hPutStr handle "\n"
  IO.hClose handle
