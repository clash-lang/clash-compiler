{-# LANGUAGE CPP                 #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
module CLaSH.Driver where

import qualified Control.Concurrent.Supply    as Supply
import           Control.Monad.State          (evalState)
import qualified Data.HashMap.Lazy            as HashMap
import           Data.List                    (isSuffixOf)
import           Data.Maybe                   (listToMaybe)
import qualified Data.Text.Lazy               as Text
import qualified System.Directory             as Directory
import qualified System.FilePath              as FilePath
import qualified System.IO                    as IO
import           Text.PrettyPrint.Leijen.Text (Doc, hPutDoc)
import           Unbound.LocallyNameless      (name2String)

import           CLaSH.Core.Term              (TmName)
import           CLaSH.Core.Type              (Type)
import           CLaSH.Driver.TestbenchGen
import           CLaSH.Driver.Types
import           CLaSH.Netlist                (genNetlist)
import           CLaSH.Netlist.Types          (Component (..), HWType,
                                               VHDLState)
import           CLaSH.Netlist.VHDL           (genVHDL, mkTyPackage)
import           CLaSH.Normalize              (checkNonRecursive, cleanupGraph,
                                               normalize, runNormalization)
import           CLaSH.Primitives.Types
import           CLaSH.Rewrite.Types          (DebugLevel (..))
import           CLaSH.Util

import qualified Data.Time.Clock              as Clock

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
      -- Create unique supplies for normalisation and TB generation
      (supplyN,supplyTB) <- ( Supply.splitSupply
                            . snd
                            . Supply.freshId)
                           <$> Supply.newSupply

      prepTime <- bindingsMap `seq` dfunMap `seq` Clock.getCurrentTime
      let prepStartDiff = Clock.diffUTCTime prepTime start
      putStrLn $ "Loading dependencies took " ++ show prepStartDiff

      let doNorm = do norm <- normalize [fst topEntity]
                      normChecked <- checkNonRecursive (fst topEntity) norm
                      cleanupGraph [fst topEntity] normChecked

          transformedBindings =
            runNormalization dbgLevel supplyN bindingsMap dfunMap clsOpMap
                             typeTrans doNorm

      normTime <- transformedBindings `seq` Clock.getCurrentTime
      let prepNormDiff = Clock.diffUTCTime normTime prepTime
      putStrLn $ "Normalisation took " ++ show prepNormDiff

      (netlist,vhdlState) <- genNetlist Nothing
                               (HashMap.fromList transformedBindings)
                               primMap typeTrans Nothing (fst topEntity)

      netlistTime <- netlist `seq` Clock.getCurrentTime
      let normNetDiff = Clock.diffUTCTime netlistTime normTime
      putStrLn $ "Netlist generation took " ++ show normNetDiff

      let topComponent = head
                       $ filter (\(Component cName _ _ _ _) ->
                                    Text.isSuffixOf (Text.pack "topEntity_0")
                                      cName)
                                netlist

      (testBench,vhdlState') <- genTestBench dbgLevel supplyTB dfunMap
                                  clsOpMap primMap typeTrans vhdlState
                                  bindingsMap
                                  (listToMaybe $ map fst testInputs)
                                  (listToMaybe $ map fst expectedOutputs)
                                  topComponent


      testBenchTime <- testBench `seq` Clock.getCurrentTime
      let netTBDiff = Clock.diffUTCTime testBenchTime netlistTime
      putStrLn $ "Testbench generation took " ++ show netTBDiff

      let vhdlDocs = createVHDL vhdlState' (netlist ++ testBench)
          dir = concat [ "./vhdl/"
                       , takeWhile (/= '.') (name2String $ fst topEntity)
                       , "/"
                       ]
      prepareDir dir
      mapM_ (writeVHDL dir) vhdlDocs

      end <- vhdlDocs `seq` Clock.getCurrentTime
      let startEndDiff = Clock.diffUTCTime end start
      putStrLn $ "Total compilation took " ++ show startEndDiff

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

createVHDL :: VHDLState
           -> [Component]
           -> [(String,Doc)]
createVHDL vhdlState components = flip evalState vhdlState $ do
  (vhdlNms,hwtyss,vhdlDocs) <- unzip3 <$> mapM genVHDL components
  let hwtys      = concat hwtyss
      vhdlNmDocs = zip vhdlNms vhdlDocs
  typesPkgM <- case hwtys of
                 [] -> return Nothing
                 _  -> Just <$> mkTyPackage hwtys

  return $ maybe vhdlNmDocs (\t -> ("types",t):vhdlNmDocs) typesPkgM

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
