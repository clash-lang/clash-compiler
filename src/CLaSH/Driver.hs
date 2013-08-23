{-# LANGUAGE CPP                 #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
module CLaSH.Driver where

import           Control.Monad.State          (evalState)
import           Data.Maybe                   (catMaybes,listToMaybe)
import qualified Control.Concurrent.Supply    as Supply
import qualified Data.HashMap.Lazy            as HashMap
import           Data.List                    (isSuffixOf)
import qualified Data.Text.Lazy               as Text
import qualified System.Directory             as Directory
import qualified System.FilePath              as FilePath
import qualified System.IO                    as IO
import           Text.PrettyPrint.Leijen.Text (Doc,hPutDoc)
import           Unbound.LocallyNameless      (name2String)

import           CLaSH.Core.Term              (TmName)
import           CLaSH.Core.Type              (Type)
import           CLaSH.Driver.TestbenchGen
import           CLaSH.Driver.Types
import           CLaSH.Netlist                (genNetlist)
import           CLaSH.Netlist.VHDL           (genVHDL,mkTyPackage)
import           CLaSH.Netlist.Types          (Component(..),HWType)
import           CLaSH.Normalize              (runNormalization, normalize, checkNonRecursive, cleanupGraph)
import           CLaSH.Primitives.Types
import           CLaSH.Rewrite.Types          (DebugLevel(..))
import           CLaSH.Util

import qualified Data.Time.Clock as Clock

generateVHDL :: BindingMap
             -> ClassOpMap
             -> DFunMap
             -> PrimMap
             -> (Type -> Maybe (Either String HWType))
             -> DebugLevel
             -> IO ()
generateVHDL bindingsMap clsOpMap dfunMap primMap typeTrans dbgLevel = do
  start <- Clock.getCurrentTime

  let topEntities = HashMap.toList
                  $ HashMap.filterWithKey isTopEntity bindingsMap

      testInputs  = HashMap.toList
                  $ HashMap.filterWithKey isTestInput bindingsMap

      expectedOutputs = HashMap.toList
                      $ HashMap.filterWithKey isExpectedOutput bindingsMap

  case topEntities of
    [topEntity] -> do
      (supplyN,supplyTB) <- fmap (Supply.splitSupply . snd .  Supply.freshId) Supply.newSupply

      prepTime <- dfunMap `seq` Clock.getCurrentTime
      traceIf True ("Loading dependencies took " ++ show (Clock.diffUTCTime prepTime start)) $ return ()

      let transformedBindings
            = runNormalization dbgLevel supplyN bindingsMap dfunMap clsOpMap typeTrans
            $ (normalize [fst topEntity]) >>= checkNonRecursive (fst topEntity) >>= cleanupGraph [fst topEntity]

      normTime <- transformedBindings `seq` Clock.getCurrentTime
      traceIf True ("Normalisation took " ++ show (Clock.diffUTCTime normTime prepTime)) $ return ()

      (netlist,vhdlState) <- genNetlist Nothing (HashMap.fromList $ transformedBindings)
                              primMap
                              typeTrans
                              Nothing
                              (fst topEntity)

      netlistTime <- netlist `seq` Clock.getCurrentTime
      traceIf True ("Netlist generation took " ++ show (Clock.diffUTCTime netlistTime normTime)) $ return ()

      (testBench,vhdlState') <- genTestBench DebugNone supplyTB dfunMap clsOpMap primMap typeTrans vhdlState
                                  bindingsMap
                                  (listToMaybe $ map fst testInputs)
                                  (listToMaybe $ map fst expectedOutputs)
                                  (head $ filter (\(Component cName _ _ _ _) -> Text.isSuffixOf (Text.pack "topEntity_0") cName) netlist)

      testBenchTime <- testBench `seq` Clock.getCurrentTime
      traceIf True ("Testbench generation took " ++ show (Clock.diffUTCTime testBenchTime netlistTime)) $ return ()

      let (vhdlNms,vhdlDocs,typesPkgM) = flip evalState vhdlState' $ do
            { (vhdlNms',typeDocsM,vhdlDocs') <- fmap unzip3 $ mapM genVHDL (netlist ++ testBench)
            ; let (typeDecDocs,typeBodyDocs) = ((catMaybes >< catMaybes) . unzip) typeDocsM
            ; typesPkgM'  <- case typeDecDocs of
                              [] -> return Nothing
                              _  -> Just <$> mkTyPackage (typeDecDocs,typeBodyDocs)
            ; return (vhdlNms',vhdlDocs',typesPkgM')
            }

      let dir = "./vhdl/" ++ (takeWhile (/= '.') $ name2String $ fst $ topEntity) ++ "/"
      prepareDir dir
      maybe (return ()) (\typesPkg -> writeVHDL dir ("types", typesPkg)) typesPkgM
      mapM_ (writeVHDL dir) (zip vhdlNms vhdlDocs)

      end <- Clock.getCurrentTime
      traceIf True ("Total compilation took " ++ show (Clock.diffUTCTime end start)) $ return ()

    [] -> error $ $(curLoc) ++ "No 'topEntity' found"
    _  -> error $ $(curLoc) ++ "Multiple 'topEntity's found"

isTopEntity ::
  TmName
  -> a
  -> Bool
isTopEntity var _ = isSuffixOf "topEntity" $ name2String var

isTestInput ::
  TmName
  -> a
  -> Bool
isTestInput var _ = isSuffixOf "testInput" $ name2String var

isExpectedOutput ::
  TmName
  -> a
  -> Bool
isExpectedOutput var _ = isSuffixOf "expectedOutput" $ name2String var

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
