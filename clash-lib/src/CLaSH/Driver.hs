{-|
  Copyright   :  (C) 2012-2016, University of Twente,
                          2017, QBayLogic, Google Inc.
  License     :  BSD2 (see the file LICENSE)
  Maintainer  :  Christiaan Baaij <christiaan.baaij@gmail.com>

  Module that connects all the parts of the CLaSH compiler library
-}

{-# LANGUAGE NondecreasingIndentation #-}
{-# LANGUAGE ScopedTypeVariables      #-}
{-# LANGUAGE TemplateHaskell          #-}
{-# LANGUAGE TupleSections            #-}

module CLaSH.Driver where

import qualified Control.Concurrent.Supply        as Supply
import           Control.DeepSeq
import           Control.Exception                (tryJust)
import           Control.Lens                     ((^.), _5)
import           Control.Monad                    (guard, when, unless)
import           Control.Monad.State              (evalState, get)
import           Data.Hashable                    (hash)
import qualified Data.HashMap.Lazy                as HML
import           Data.HashMap.Strict              (HashMap)
import qualified Data.HashMap.Strict              as HM
import qualified Data.HashSet                     as HashSet
import           Data.IntMap                      (IntMap)
import           Data.Maybe                       (fromMaybe)
import           Data.Text.Lazy                   (Text)
import qualified Data.Text.Lazy                   as Text
import qualified Data.Time.Clock                  as Clock
import qualified System.Directory                 as Directory
import           System.FilePath                  ((</>), (<.>))
import qualified System.FilePath                  as FilePath
import qualified System.IO                        as IO
import           System.IO.Error                  (isDoesNotExistError)
import           Text.PrettyPrint.Leijen.Text     (Doc, hPutDoc, text)
import           Text.PrettyPrint.Leijen.Text.Monadic (displayT, renderOneLine)

import           GHC.BasicTypes.Extra             ()

import           CLaSH.Annotations.TopEntity      (TopEntity (..))
import           CLaSH.Annotations.TopEntity.Extra ()
import           CLaSH.Backend
import           CLaSH.Core.Name                  (Name (..), name2String)
import           CLaSH.Core.Term                  (Term, TmName, TmOccName)
import           CLaSH.Core.Type                  (Type)
import           CLaSH.Core.TyCon                 (TyCon, TyConName, TyConOccName)
import           CLaSH.Driver.Types
import           CLaSH.Netlist                    (genComponentName, genNetlist)
import           CLaSH.Netlist.BlackBox.Parser    (runParse)
import           CLaSH.Netlist.BlackBox.Types     (BlackBoxTemplate)
import           CLaSH.Netlist.Id                 (IdType (..))
import           CLaSH.Netlist.Types              (Component (..), HWType)
import           CLaSH.Normalize                  (checkNonRecursive, cleanupGraph,
                                                   normalize, runNormalization)
import           CLaSH.Normalize.Util             (callGraph)
import           CLaSH.Primitives.Types
import           CLaSH.Util                       (first, second)

-- | Create a set of target HDL files for a set of functions
generateHDL
  :: forall backend . Backend backend
  => BindingMap
  -- ^ Set of functions
  -> Maybe backend
  -> PrimMap (Text.Text)
  -- ^ Primitive / BlackBox Definitions
  -> HashMap TyConOccName TyCon
  -- ^ TyCon cache
  -> IntMap TyConName
  -- ^ Tuple TyCon cache
  -> (HashMap TyConOccName TyCon -> Type -> Maybe (Either String HWType))
  -- ^ Hardcoded 'Type' -> 'HWType' translator
  -> (HashMap TyConOccName TyCon -> Bool -> Term -> Term)
  -- ^ Hardcoded evaluator (delta-reduction)
  -> [( TmName
      , Type
      , Maybe TopEntity
      , Maybe TmName
      )]
  -- ^ topEntity bndr
  -- + (maybe) TopEntity annotation
  -- + (maybe) testBench bndr
  -> CLaSHOpts
  -- ^ Debug information level for the normalization process
  -> (Clock.UTCTime,Clock.UTCTime)
  -> IO ()
generateHDL bindingsMap hdlState primMap tcm tupTcm typeTrans eval topEntities
  opts (startTime,prepTime) = go prepTime [] topEntities where

  primMap' = HM.map parsePrimitive primMap

  -- No more TopEntities to process
  go prevTime _ [] = putStrLn $ "Total compilation took " ++
                              show (Clock.diffUTCTime prevTime startTime)

  -- Process the next TopEntity
  go prevTime seen ((topEntity,_,annM,benchM):topEntities') = do
  putStrLn $ "Compiling: " ++ name2String topEntity

  -- Some initial setup
  let modName   = maybe (takeWhile (/= '.') (name2String topEntity)) t_name annM
      iw        = opt_intWidth opts
      hdlsyn    = opt_hdlSyn opts
      hdlState' = setModName modName
                $ fromMaybe (initBackend iw hdlsyn :: backend) hdlState
      hdlDir    = fromMaybe "." (opt_hdlDir opts) </>
                        CLaSH.Backend.name hdlState' </>
                        takeWhile (/= '.') (name2String topEntity)
      mkId      = evalState mkIdentifier hdlState'
      extId     = evalState extendIdentifier hdlState'
      topNm     = maybe (mkId Basic (Text.pack $ modName ++ "_topEntity"))
                        (Text.pack . t_name)
                        annM

  -- Calculate the hash over the callgraph and the topEntity annotation
  (sameTopHash,sameBenchHash,manifest) <- do
    let topHash    = hash (annM,callGraphBindings bindingsMap (nameOcc topEntity))
        benchHashM = fmap (hash . (annM,) . callGraphBindings bindingsMap . nameOcc) benchM
        manifestI  = Manifest (topHash,benchHashM) [] [] [] [] []

        manFile = maybe (hdlDir </> Text.unpack topNm <.> "manifest")
                        (\ann -> hdlDir </> t_name ann </> t_name ann <.> "manifest")
                        annM
    manM <- fmap read . either (const Nothing) Just <$>
            tryJust (guard . isDoesNotExistError) (readFile manFile)
    -- manM <- fmap (Just . read) (readFile manFile)
    return (maybe (False,False,manifestI)
                  (\man -> (fst (manifestHash man) == topHash
                           ,snd (manifestHash man) == benchHashM
                           ,man {manifestHash = (topHash,benchHashM)}
                           ))
                  manM)

  (supplyN,supplyTB) <- Supply.splitSupply
                    . snd
                    . Supply.freshId
                   <$> Supply.newSupply
  let topEntityNames = map (\(x,_,_,_) -> nameOcc x) topEntities

  (topTime,manifest',seen') <- if sameTopHash
    then do
      putStrLn ("Using cached result for: " ++ name2String topEntity)
      topTime <- Clock.getCurrentTime
      return (topTime,manifest,componentNames manifest ++ seen)
    else do
      -- 1. Normalise topEntity
      let transformedBindings = normalizeEntity bindingsMap primMap' tcm tupTcm
                                  typeTrans eval topEntityNames opts supplyN
                                  (nameOcc topEntity)

      normTime <- transformedBindings `deepseq` Clock.getCurrentTime
      let prepNormDiff = Clock.diffUTCTime normTime prevTime
      putStrLn $ "Normalisation took " ++ show prepNormDiff

      -- 2. Generate netlist for topEntity
      (netlist,dfiles,seen') <- genNetlist transformedBindings topEntities primMap'
                                tcm typeTrans [] iw mkId extId seen
                                hdlDir (nameOcc topEntity)

      netlistTime <- netlist `deepseq` Clock.getCurrentTime
      let normNetDiff = Clock.diffUTCTime netlistTime normTime
      putStrLn $ "Netlist generation took " ++ show normNetDiff

      -- 3. Generate topEntity wrapper
      let topComponent = snd . head $
            filter (\(_,Component cName _ _ _) -> maybe
              (Text.isSuffixOf (genComponentName [] mkId topEntity))
              (\te n -> n == Text.pack (t_name te)) annM
                cName)
              netlist
          (hdlDocs,manifest')  = createHDL hdlState' modName netlist topComponent
                                   (Text.unpack topNm, Right manifest)
          dir = hdlDir </> maybe "" t_name annM
      prepareDir (opt_cleanhdl opts) (extension hdlState') dir
      mapM_ (writeHDL dir) hdlDocs
      copyDataFiles (opt_importPaths opts) dir dfiles

      topTime <- hdlDocs `seq` Clock.getCurrentTime
      return (topTime,manifest',seen')

  benchTime <- case benchM of
    Just tb | not sameBenchHash -> do
      putStrLn $ "Compiling: " ++ name2String tb

      let modName'  = Text.unpack (genComponentName [] mkId tb)
          hdlState2 = setModName modName' hdlState'

      -- 1. Normalise testBench
      let transformedBindings = normalizeEntity bindingsMap primMap' tcm tupTcm
                                  typeTrans eval topEntityNames opts supplyTB (nameOcc tb)
      normTime <- transformedBindings `deepseq` Clock.getCurrentTime
      let prepNormDiff = Clock.diffUTCTime normTime topTime
      putStrLn $ "Testbench normalisation took " ++ show prepNormDiff

      -- 2. Generate netlist for topEntity
      (netlist,dfiles,_) <- genNetlist transformedBindings topEntities primMap'
                              tcm typeTrans [] iw mkId extId seen'
                              hdlDir (nameOcc tb)

      netlistTime <- netlist `deepseq` Clock.getCurrentTime
      let normNetDiff = Clock.diffUTCTime netlistTime normTime
      putStrLn $ "Testbench netlist generation took " ++ show normNetDiff

      -- 3. Write HDL
      let (hdlDocs,_) = createHDL hdlState2 modName' netlist undefined
                           (Text.unpack topNm, Left manifest')
          dir = hdlDir </> maybe "" t_name annM </> modName'
      prepareDir (opt_cleanhdl opts) (extension hdlState2) dir
      writeHDL (hdlDir </> maybe "" t_name annM) (head hdlDocs)
      mapM_ (writeHDL dir) (tail hdlDocs)
      copyDataFiles (opt_importPaths opts) dir dfiles

      hdlDocs `seq` Clock.getCurrentTime

    Just tb -> do
      let tb' = name2String tb
      putStrLn ("Compiling: " ++ tb')
      putStrLn ("Using cached result for: " ++ tb')
      return topTime

    Nothing -> return topTime

  go benchTime seen' topEntities'

parsePrimitive :: Primitive Text -> Primitive BlackBoxTemplate
parsePrimitive (BlackBox pNm oReg libM imps inc templT) =
  let (templ,err) = either (first Left . runParse) (first Right . runParse) templT
      inc'        = case fmap (second runParse) inc of
                      Just (x,(t,[])) -> Just (x,t)
                      _ -> Nothing
  in  case err of
        [] -> BlackBox pNm oReg libM imps inc' templ
        _  -> error $ "Errors in template for: " ++ show pNm ++ ":\n" ++ show err
parsePrimitive (Primitive pNm typ) = Primitive pNm typ

-- | Pretty print Components to HDL Documents
createHDL
  :: Backend backend
  => backend
  -- ^ Backend
  -> String
  -- ^ Module hierarchy root
  -> [(SrcSpan,Component)]
  -- ^ List of components
  -> Component
  -- ^ Top component
  -> (String, Either Manifest Manifest)
  -- ^ Name of the manifest file
  -- + Either:
  --   * Left manifest:  Only write/update the hashes of the @manifest@
  --   * Right manifest: Update all fields of the @manifest@
  -> ([(String,Doc)],Manifest)
  -- ^ The pretty-printed HDL documents
  -- + The update manifest file
createHDL backend modName components top (topName,manifestE) = flip evalState backend $ do
  (hdlNmDocs,incs) <- unzip <$> mapM (uncurry (genHDL modName)) components
  hwtys <- HashSet.toList <$> extractTypes <$> get
  typesPkg <- mkTyPackage modName hwtys
  let hdl   = map (first (<.> CLaSH.Backend.extension backend)) (typesPkg ++ hdlNmDocs)
      qincs = map (first (<.> "qsys")) (concat incs)
      topFiles = hdl ++ qincs
  manifest <- either return (\m -> do
      let topInNames  = map fst (inputs top)
      topInTypes  <- mapM (fmap (displayT . renderOneLine) . hdlType . snd) (inputs top)
      let topOutNames = map (fst . snd) (outputs top)
      topOutTypes <- mapM (fmap (displayT . renderOneLine) . hdlType . snd . snd) (outputs top)
      let compNames = map (componentName.snd) components
      return (m { portInNames    = topInNames
                , portInTypes    = topInTypes
                , portOutNames   = topOutNames
                , portOutTypes   = topOutTypes
                , componentNames = compNames
                })
    ) manifestE
  let manDoc = ( topName <.> "manifest"
               , text (Text.pack (show manifest)))
  return (manDoc:topFiles,manifest)

-- | Prepares the directory for writing HDL files. This means creating the
--   dir if it does not exist and removing all existing .hdl files from it.
prepareDir :: Bool -- ^ Remove existing HDL files
           -> String -- ^ File extension of the HDL files.
           -> String
           -> IO ()
prepareDir cleanhdl ext dir = do
  -- Create the dir if needed
  Directory.createDirectoryIfMissing True dir
  -- Clean the directory when needed
  when cleanhdl $ do
    -- Find all HDL files in the directory
    files <- Directory.getDirectoryContents dir
    let to_remove = filter ((==ext) . FilePath.takeExtension) files
    -- Prepend the dirname to the filenames
    let abs_to_remove = map (FilePath.combine dir) to_remove
    -- Remove the files
    mapM_ Directory.removeFile abs_to_remove

-- | Writes a HDL file to the given directory
writeHDL :: FilePath -> (String, Doc) -> IO ()
writeHDL dir (cname, hdl) = do
  handle <- IO.openFile (dir </> cname) IO.WriteMode
  hPutDoc handle hdl
  IO.hPutStr handle "\n"
  IO.hClose handle

copyDataFiles :: [FilePath] -> FilePath -> [(String,FilePath)] -> IO ()
copyDataFiles idirs dir = mapM_ (copyFile' idirs)
  where
    copyFile' dirs (nm,old) = do
      oldExists <- Directory.doesFileExist old
      if oldExists
        then Directory.copyFile old new
        else goImports dirs
      where
        new = dir FilePath.</> nm

        goImports [] = do
          oldExists <- Directory.doesFileExist old
          if oldExists
            then Directory.copyFile old new
            else unless (null old) (putStrLn ("WARNING: file " ++ show old ++ " does not exist"))
        goImports (d:ds) = do
          let old2 = d FilePath.</> old
          old2Exists <- Directory.doesFileExist old2
          if old2Exists
            then Directory.copyFile old2 new
            else goImports ds

-- | Get all the terms corresponding to a call graph
callGraphBindings
  :: BindingMap
  -- ^ All bindings
  -> TmOccName
  -- ^ Root of the call graph
  -> [Term]
callGraphBindings bindingsMap tm = map ((^. _5) . (bindingsMap HM.!) . fst) cg
  where
    cg = callGraph [] bindingsMap tm

-- | Normalize a complete hierarchy
normalizeEntity
  :: BindingMap
  -- ^ All bindings
  -> PrimMap BlackBoxTemplate
  -- ^ BlackBox HDL templates
  -> HashMap TyConOccName TyCon
  -- ^ TyCon cache
  -> IntMap TyConName
  -- ^ Tuple TyCon cache
  -> (HashMap TyConOccName TyCon -> Type -> Maybe (Either String HWType))
  -- ^ Hardcoded 'Type' -> 'HWType' translator
  -> (HashMap TyConOccName TyCon -> Bool -> Term -> Term)
  -- ^ Hardcoded evaluator (delta-reduction)
  -> [TmOccName]
  -- ^ TopEntities
  -> CLaSHOpts
  -- ^ Debug information level for the normalization process
  -> Supply.Supply
  -- ^ Unique supply
  -> TmOccName
  -- ^ root of the hierarchy
  -> BindingMap
normalizeEntity bindingsMap primMap tcm tupTcm typeTrans eval topEntities
  opts supply tm = transformedBindings
  where
    doNorm = do norm <- normalize [tm]
                let normChecked = checkNonRecursive norm
                cleanupGraph tm normChecked
    transformedBindings = runNormalization opts supply bindingsMap
                            typeTrans tcm tupTcm eval primMap HML.empty
                            topEntities doNorm
