{-|
  Copyright   :  (C) 2013-2016, University of Twente,
                     2016-2017, Myrtle Software Ltd,
                     2017     , Google Inc.
  License     :  BSD2 (see the file LICENSE)
  Maintainer  :  Christiaan Baaij <christiaan.baaij@gmail.com>
-}

{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Clash.GHC.LoadModules
  ( loadModules
  , ghcLibDir
  , setWantedLanguageExtensions
  )
where

#ifndef USE_GHC_PATHS
#ifndef TOOL_VERSION_ghc
#error TOOL_VERSION_ghc undefined
#endif
#endif

-- External Modules
import           Clash.Annotations.Primitive     (HDL, PrimitiveGuard)
import           Clash.Annotations.TopEntity     (TopEntity (..))
import           Clash.Primitives.Types          (UnresolvedPrimitive)
import           Clash.Util                      (ClashException(..), pkgIdFromTypeable)
import qualified Clash.Util.Interpolate          as I
import           Control.Arrow                   (first, second)
import           Control.DeepSeq                 (deepseq)
import           Control.Exception               (SomeException, throw)
import           Control.Monad                   (forM)
#if MIN_VERSION_ghc(8,6,0)
import           Control.Exception               (throwIO)
#endif
import           Control.Monad.IO.Class          (liftIO)
import           Data.Char                       (isDigit)
import           Data.Generics.Uniplate.DataOnly (transform)
import           Data.Data                       (Data)
import           Data.Typeable                   (Typeable)
import           Data.List                       (foldl', nub)
import           Data.Maybe                      (catMaybes, listToMaybe, fromMaybe)
import qualified Data.Text                       as Text
import qualified Data.Time.Clock                 as Clock
import           Language.Haskell.TH.Syntax      (lift)
import           GHC.Stack                       (HasCallStack)

#ifdef USE_GHC_PATHS
import           GHC.Paths                       (libdir)
#else
import           System.Exit                     (ExitCode (..))
import           System.IO                       (hGetLine)
import           System.IO.Error                 (tryIOError)
import           System.Process                  (runInteractiveCommand,
                                                  waitForProcess)
#endif

-- GHC API
import qualified Annotations
import qualified CoreFVs
import qualified CoreSyn
import qualified Digraph
#if MIN_VERSION_ghc(8,6,0)
import qualified DynamicLoading
#endif
import           DynFlags                        (GeneralFlag (..))
import qualified DynFlags
import qualified Exception
import qualified GHC
import qualified HscMain
import qualified HscTypes
import qualified MonadUtils
import qualified Panic
import qualified GhcPlugins                      (deserializeWithData, installedUnitIdString)
import qualified TcRnMonad
import qualified TcRnTypes
import qualified TidyPgm
import qualified Unique
import qualified UniqFM
import qualified FamInst
import qualified FamInstEnv
import qualified GHC.LanguageExtensions          as LangExt
import qualified Name
import qualified OccName
import           Outputable                      (ppr)
import qualified Outputable
import qualified UniqSet
import           Util (OverridingBool)
import qualified Var

-- Internal Modules
import           Clash.GHC.GHC2Core                           (modNameM, qualifiedNameString')
import           Clash.GHC.LoadInterfaceFiles
  (loadExternalExprs, getUnresolvedPrimitives, loadExternalBinders,
   LoadedBinders(..))
import           Clash.GHCi.Common                            (checkMonoLocalBindsMod)
import           Clash.Util                                   (curLoc, noSrcSpan, reportTimeDiff
                                                              ,wantedLanguageExtensions, unwantedLanguageExtensions)
import           Clash.Annotations.BitRepresentation.Internal
  (DataRepr', dataReprAnnToDataRepr')

ghcLibDir :: IO FilePath
#ifdef USE_GHC_PATHS
ghcLibDir = return libdir
#else
ghcLibDir = do
  (libDirM,exitCode) <- getProcessOutput $ "ghc-" ++ TOOL_VERSION_ghc ++ " --print-libdir"
  case exitCode of
     ExitSuccess   -> case libDirM of
       Just libDir -> return libDir
       Nothing     -> Panic.pgmError noGHC
     ExitFailure i -> case i of
       127         -> Panic.pgmError noGHC
       i'          -> Panic.pgmError $ "Calling GHC failed with error code: " ++ show i'
  where
    noGHC = "Clash needs the GHC compiler it was built with, ghc-" ++ TOOL_VERSION_ghc ++
            ", but it was not found. Make sure its location is in your PATH variable."

getProcessOutput :: String -> IO (Maybe String, ExitCode)
getProcessOutput command =
     -- Create the process
  do (_, pOut, _, handle) <- runInteractiveCommand command
     -- Wait for the process to finish and store its exit code
     exitCode <- waitForProcess handle
     -- Get the standard output.
     output   <- either (const Nothing) Just <$> tryIOError (hGetLine pOut)
     -- return both the output and the exit code.
     return (output, exitCode)
#endif

-- | Search databases for given module
loadExternalModule
  :: (HasCallStack, GHC.GhcMonad m)
  => HDL
  -> String
  -- ^ Module name. Can either be a filepath pointing to a .hs file, or a
  -- qualified module name (example: "Data.List").
  -> m (Either
          SomeException
          ( [CoreSyn.CoreBndr]                     -- Root binders
          , FamInstEnv.FamInstEnv                  -- Local type family instances
          , GHC.ModuleName                         -- Module name
          , LoadedBinders
          , [CoreSyn.CoreBind]                     -- All bindings
          ) )
loadExternalModule hdl modName0 = Exception.gtry $ do
  let modName1 = GHC.mkModuleName modName0
  foundMod <- GHC.findModule modName1 Nothing
  let errMsg = "Internal error: found  module, but could not load it"
  modInfo <- fromMaybe (error errMsg) <$> (GHC.getModuleInfo foundMod)
  tyThings <- catMaybes <$> mapM GHC.lookupGlobalName (GHC.modInfoExports modInfo)
  let rootIds = [id_ | GHC.AnId id_ <- tyThings]
  loaded <- loadExternalBinders hdl rootIds
  let allBinders = makeRecursiveGroups (lbBinders loaded)
  return (rootIds, FamInstEnv.emptyFamInstEnv, modName1, loaded, allBinders)

setupGhc
  :: GHC.GhcMonad m
  => OverridingBool
  -> Maybe GHC.DynFlags
  -> [FilePath]
  -> m ()
setupGhc useColor dflagsM idirs = do
  dflags <-
    case dflagsM of
      Just df -> return df
      Nothing -> do
#if MIN_VERSION_ghc(8,6,0)
        -- Make sure we read the .ghc environment files
        df <- do
          df <- GHC.getSessionDynFlags
          _ <- GHC.setSessionDynFlags df {DynFlags.pkgDatabase = Nothing}
          GHC.getSessionDynFlags
#else
        df <- GHC.getSessionDynFlags
#endif
        let df1 = setWantedLanguageExtensions df
            ghcTyLitNormPlugin = GHC.mkModuleName "GHC.TypeLits.Normalise"
            ghcTyLitExtrPlugin = GHC.mkModuleName "GHC.TypeLits.Extra.Solver"
            ghcTyLitKNPlugin   = GHC.mkModuleName "GHC.TypeLits.KnownNat.Solver"
            dfPlug = df1 { DynFlags.pluginModNames = nub $
                                ghcTyLitNormPlugin : ghcTyLitExtrPlugin :
                                ghcTyLitKNPlugin : DynFlags.pluginModNames df1
                           , DynFlags.useColor = useColor
                           , DynFlags.importPaths = idirs
                           }
        return dfPlug

  let dflags1 = dflags
                  { DynFlags.optLevel = 2
                  , DynFlags.ghcMode  = GHC.CompManager
                  , DynFlags.ghcLink  = GHC.LinkInMemory
                  , DynFlags.hscTarget
                      = if DynFlags.rtsIsProfiled
                           then DynFlags.HscNothing
                           else DynFlags.defaultObjectTarget $
#if !MIN_VERSION_ghc(8,10,0)
                                  DynFlags.targetPlatform
#endif
                                    dflags
                  , DynFlags.reductionDepth = 1000
                  }
  let dflags2 = unwantedOptimizationFlags dflags1
      ghcDynamic = case lookup "GHC Dynamic" (DynFlags.compilerInfo dflags) of
                    Just "YES" -> True
                    _          -> False
      dflags3 = if ghcDynamic then DynFlags.gopt_set dflags2 DynFlags.Opt_BuildDynamicToo
                              else dflags2
#if MIN_VERSION_ghc(8,6,0)
  hscenv <- GHC.getSession
  dflags4 <- MonadUtils.liftIO (DynamicLoading.initializePlugins hscenv dflags3)
  _ <- GHC.setSessionDynFlags dflags4
#else
  _ <- GHC.setSessionDynFlags dflags3
#endif

  return ()

-- | Load a module from a Haskell file. Function does NOT look in currently
-- loaded modules.
loadLocalModule
  :: GHC.GhcMonad m
  => HDL
  -> String
  -- ^ Module name. Can either be a filepath pointing to a .hs file, or a
  -- qualified module name (example: "Data.List").
  -> m ( [CoreSyn.CoreBndr]                     -- Root binders
       , FamInstEnv.FamInstEnv                  -- Local type family instances
       , GHC.ModuleName                         -- Module name
       , LoadedBinders
       , [CoreSyn.CoreBind]                     -- All bindings
       )
loadLocalModule hdl modName = do
  target <- GHC.guessTarget modName Nothing
  GHC.setTargets [target]
  modGraph <- GHC.depanal [] False
#if MIN_VERSION_ghc(8,4,1)
  let modGraph' = GHC.mapMG disableOptimizationsFlags modGraph
#else
  let modGraph' = map disableOptimizationsFlags modGraph
#endif
      -- 'topSortModuleGraph' ensures that modGraph2, and hence tidiedMods
      -- are in topological order, i.e. the root module is last.
      modGraph2 = Digraph.flattenSCCs (GHC.topSortModuleGraph True modGraph' Nothing)

  liftIO $ mapM_ checkMonoLocalBindsMod modGraph2

  tidiedMods <- forM modGraph2 $ \m -> do
    oldDFlags <- GHC.getSessionDynFlags
    pMod  <- parseModule m
    _ <- GHC.setSessionDynFlags (GHC.ms_hspp_opts (GHC.pm_mod_summary pMod))
    tcMod <- GHC.typecheckModule (removeStrictnessAnnotations pMod)

    -- The purpose of the home package table (HPT) is to track
    -- the already compiled modules, so subsequent modules can
    -- rely/use those compilation results
    --
    -- We need to update the home package table (HPT) ourselves
    -- as we can no longer depend on 'GHC.load' to create a
    -- proper HPT.
    --
    -- The reason we have to cannot rely on 'GHC.load' is that
    -- it runs the rename/type-checker, which we also run in
    -- the code above. This would mean that the renamer/type-checker
    -- is run twice, which in turn means that template haskell
    -- splices are run twice.
    --
    -- Given that TH splices can do non-trivial computation and I/O,
    -- running TH twice must be avoid.
    tcMod' <- GHC.loadModule tcMod
    dsMod <- fmap GHC.coreModule $ GHC.desugarModule tcMod'
    hsc_env <- GHC.getSession
#if MIN_VERSION_ghc(8,4,1)
    simpl_guts <- MonadUtils.liftIO $ HscMain.hscSimplify hsc_env [] dsMod
#else
    simpl_guts <- MonadUtils.liftIO $ HscMain.hscSimplify hsc_env dsMod
#endif
    checkForInvalidPrelude simpl_guts
    (tidy_guts,_) <- MonadUtils.liftIO $ TidyPgm.tidyProgram hsc_env simpl_guts
    let pgm        = HscTypes.cg_binds tidy_guts
    let modFamInstEnv = TcRnTypes.tcg_fam_inst_env $ fst $ GHC.tm_internals_ tcMod
    _ <- GHC.setSessionDynFlags oldDFlags
    return (pgm,modFamInstEnv)

  let (binders,modFamInstEnvs) = unzip tidiedMods
      binderIds                = map fst (CoreSyn.flattenBinds (concat binders))
      plusFamInst f1 f2        = FamInstEnv.extendFamInstEnvList f1 (FamInstEnv.famInstEnvElts f2)
      modFamInstEnvs'          = foldl' plusFamInst FamInstEnv.emptyFamInstEnv modFamInstEnvs
      rootModule               = GHC.ms_mod_name . last $ modGraph2

  -- Because tidiedMods is in topological order, binders is also, and hence
  -- the binders belonging to the "root" module are the last binders
  let rootIds = map fst . CoreSyn.flattenBinds $ last binders
  loaded0 <- loadExternalExprs hdl (UniqSet.mkUniqSet binderIds) (concat binders)

  -- Find local primitive annotations
  localPrims <- findPrimitiveAnnotations hdl binderIds
  let loaded1 = loaded0{lbPrims=lbPrims loaded0 ++ localPrims}

  let allBinders = concat binders ++ makeRecursiveGroups (lbBinders loaded0)
  pure (rootIds, modFamInstEnvs', rootModule, loaded1, allBinders)

loadModules
  :: OverridingBool
  -- ^ Use color
  -> HDL
  -- ^ HDL target
  -> String
  -- ^ Module name
  -> Maybe (DynFlags.DynFlags)
  -- ^ Flags to run GHC with
  -> [FilePath]
  -- ^ Import dirs to use when no DynFlags are provided
  -> IO ( [CoreSyn.CoreBind]                     -- Binders
        , [(CoreSyn.CoreBndr,Int)]               -- Class operations
        , [CoreSyn.CoreBndr]                     -- Unlocatable Expressions
        , FamInstEnv.FamInstEnvs
        , [( CoreSyn.CoreBndr                    -- topEntity bndr
           , Maybe TopEntity                     -- (maybe) TopEntity annotation
           , Maybe CoreSyn.CoreBndr)]            -- (maybe) testBench bndr
        , [Either UnresolvedPrimitive FilePath]
        , [DataRepr']
        , [(Text.Text, PrimitiveGuard ())]
        )
loadModules useColor hdl modName dflagsM idirs = do
  libDir <- MonadUtils.liftIO ghcLibDir
  startTime <- Clock.getCurrentTime
  GHC.runGhc (Just libDir) $ do
    -- 'mainFunIs' is set to Nothing due to issue #1304:
    -- https://github.com/clash-lang/clash-compiler/issues/1304
    setupGhc useColor ((\d -> d{GHC.mainFunIs=Nothing}) <$> dflagsM) idirs
    -- TODO: We currently load the transitive closure of _all_ bindings found
    -- TODO: in the top module. This is wasteful if one or more binders don't
    -- TODO: contribute to any top entities. This effect is worsened when using
    -- TODO: -main-is, which only synthesizes a single top entity (and all its
    -- TODO: dependencies).
    (rootIds, modFamInstEnvs, rootModule, LoadedBinders{..}, allBinders) <-
      -- We need to try and load external modules first, because we can't
      -- recover from errors in 'loadLocalModule'.
      loadExternalModule hdl modName >>= \case
        Left _loadExternalErr -> loadLocalModule hdl modName
        Right res -> pure res

    let allBinderIds = map fst (CoreSyn.flattenBinds allBinders)

    modTime <- startTime `deepseq` length allBinderIds `seq` MonadUtils.liftIO Clock.getCurrentTime
    let modStartDiff = reportTimeDiff modTime startTime
    MonadUtils.liftIO $ putStrLn $ "GHC: Parsing and optimising modules took: " ++ modStartDiff

    extTime <- modTime `deepseq` length lbUnlocatable `deepseq` MonadUtils.liftIO Clock.getCurrentTime
    let extModDiff = reportTimeDiff extTime modTime
    MonadUtils.liftIO $ putStrLn $ "GHC: Loading external modules from interface files took: " ++ extModDiff

    -- Get type family instances: accumulated by GhcMonad during
    -- 'loadExternalBinders' / 'loadExternalExprs'
    hscEnv <- GHC.getSession
#if MIN_VERSION_ghc(8,6,0)
    famInstEnvs <- do
      (msgs, m) <- TcRnMonad.liftIO $ TcRnMonad.initTcInteractive hscEnv FamInst.tcGetFamInstEnvs
      case m of
        Nothing -> TcRnMonad.liftIO $ throwIO (HscTypes.mkSrcErr (snd msgs))
        Just x  -> return x
#else
    famInstEnvs <- TcRnMonad.liftIO $ TcRnMonad.initTcForLookup hscEnv FamInst.tcGetFamInstEnvs
#endif

    -- Because tidiedMods is in topological order, binders is also, and hence
    -- allSyn is in topological order. This means that the "root" 'topEntity'
    -- will be compiled last.
    allSyn     <- map (second Just) <$> findSynthesizeAnnotations allBinderIds
    topSyn     <- map (second Just) <$> findSynthesizeAnnotations rootIds
    benchAnn   <- findTestBenchAnnotations rootIds
    reprs'     <- findCustomReprAnnotations
    primGuards <- findPrimitiveGuardAnnotations allBinderIds
    let topEntityName = fromMaybe "topEntity" (GHC.mainFunIs =<< dflagsM)
        varNameString = OccName.occNameString . Name.nameOccName . Var.varName
        topEntities = filter ((==topEntityName) . varNameString) rootIds
        benches     = filter ((== "testBench") . varNameString) rootIds
        mergeBench (x,y) = (x,y,lookup x benchAnn)
        allSyn'     = map mergeBench allSyn

    topEntities' <-
      case (topEntities, topSyn) of
        ([], []) ->
          let modName1 = Outputable.showSDocUnsafe (ppr rootModule) in
          if topEntityName /= "topEntity" then
            Panic.pgmError [I.i|
              No top-level function called '#{topEntityName}' found. Did you
              forget to export it?
            |]
          else
            Panic.pgmError [I.i|
              No top-level function called 'topEntity' found, nor a function with
              a 'Synthesize' annotation in module #{modName1}. Did you forget to
              export them?

              For more information on 'Synthesize' annotations, check out the
              documentation of "Clash.Annotations.TopEntity".
            |]
        ([], _) ->
          return allSyn'
        ([x], _) ->
          case lookup x topSyn of
            Nothing ->
              case lookup x benchAnn of
                Nothing -> return ((x,Nothing,listToMaybe benches):allSyn')
                Just y  -> return ((x,Nothing,Just y):allSyn')
            Just _ ->
              return allSyn'
        (_, _) ->
          Panic.pgmError $ $(curLoc) ++ "Multiple 'topEntities' found."

    let reprs1 = lbReprs ++ reprs'

    annTime <-
      extTime
        `deepseq` length topEntities'
        `deepseq` lbPrims
        `deepseq` reprs1
        `deepseq` primGuards
        `deepseq` MonadUtils.liftIO Clock.getCurrentTime

    let annExtDiff = reportTimeDiff annTime extTime
    MonadUtils.liftIO $ putStrLn $ "GHC: Parsing annotations took: " ++ annExtDiff

    return ( allBinders
           , lbClassOps
           , lbUnlocatable
           , (fst famInstEnvs, modFamInstEnvs)
           , topEntities'
           , lbPrims
           , reprs1
           , primGuards
           )

-- | Given a set of bindings, make explicit non-recursive bindings and
-- recursive binding groups.
--
-- Needed because:
-- 1. GHC does not preserve this information in interface files,
-- 2. Binders in Clash's BindingsMap are not allowed to be mutually recursive,
--    only self-recursive.
-- 3. Clash.GHC.GenerateBindings.mkBindings turns groups of mutually recursive
--    bindings into self-recursive bindings which can go into the BindingsMap.
makeRecursiveGroups
  :: [(CoreSyn.CoreBndr,CoreSyn.CoreExpr)]
  -> [CoreSyn.CoreBind]
makeRecursiveGroups
  = map makeBind
  . Digraph.stronglyConnCompFromEdgedVerticesUniq
  . map makeNode
  where
    makeNode
      :: (CoreSyn.CoreBndr,CoreSyn.CoreExpr)
      -> Digraph.Node Unique.Unique (CoreSyn.CoreBndr,CoreSyn.CoreExpr)
    makeNode (b,e) =
#if MIN_VERSION_ghc(8,4,1)
      Digraph.DigraphNode
        (b,e)
        (Var.varUnique b)
        (UniqSet.nonDetKeysUniqSet (CoreFVs.exprFreeIds e))
#else
      ((b,e)
      ,Var.varUnique b
      ,UniqSet.nonDetKeysUniqSet (CoreFVs.exprFreeIds e))
#endif

    makeBind
      :: Digraph.SCC (CoreSyn.CoreBndr,CoreSyn.CoreExpr)
      -> CoreSyn.CoreBind
    makeBind (Digraph.AcyclicSCC (b,e)) = CoreSyn.NonRec b e
    makeBind (Digraph.CyclicSCC bs)     = CoreSyn.Rec bs

errOnDuplicateAnnotations
  :: String
  -- ^ Name of annotation
  -> [CoreSyn.CoreBndr]
  -- ^ Binders searched for
  -> [[a]]
  -- ^ Parsed annotations
  -> [(CoreSyn.CoreBndr, a)]
errOnDuplicateAnnotations nm bndrs anns =
  go (zip bndrs anns)
 where
  go
    :: [(CoreSyn.CoreBndr, [a])]
    -> [(CoreSyn.CoreBndr, a)]
  go []             = []
  go ((_, []):ps)   = go ps
  go ((b, [p]):ps)  = (b, p) : (go ps)
  go ((b, _):_)  =
    Panic.pgmError $ "The following value has multiple "
                  ++ "'" ++ nm ++ "' annotations: "
                  ++ Outputable.showSDocUnsafe (ppr b)

-- | Find annotations by given targets
findAnnotationsByTargets
  :: GHC.GhcMonad m
  => Typeable a
  => Data a
  => [Annotations.AnnTarget Name.Name]
  -> m [[a]]
findAnnotationsByTargets targets =
  mapM (GHC.findGlobalAnns GhcPlugins.deserializeWithData) targets

-- | Find all annotations of a certain type in all modules seen so far.
findAllModuleAnnotations
  :: GHC.GhcMonad m
  => Data a
  => Typeable a
  => m [a]
findAllModuleAnnotations = do
  hsc_env <- GHC.getSession
  ann_env <- liftIO $ HscTypes.prepareAnnotations hsc_env Nothing
  return $ concat
         $ UniqFM.nonDetEltsUFM
         $ Annotations.deserializeAnns GhcPlugins.deserializeWithData ann_env

-- | Find all annotations belonging to all binders seen so far.
findNamedAnnotations
  :: GHC.GhcMonad m
  => Data a
  => Typeable a
  => [CoreSyn.CoreBndr]
  -> m [[a]]
findNamedAnnotations bndrs =
  findAnnotationsByTargets (map (Annotations.NamedTarget . Var.varName) bndrs)

findPrimitiveGuardAnnotations
  :: GHC.GhcMonad m
  => [CoreSyn.CoreBndr]
  -> m [(Text.Text, (PrimitiveGuard ()))]
findPrimitiveGuardAnnotations bndrs = do
  anns0 <- findNamedAnnotations bndrs
  let anns1 = errOnDuplicateAnnotations "PrimitiveGuard" bndrs anns0
  pure (map (first (qualifiedNameString' . Var.varName)) anns1)

-- | Find annotations of type @DataReprAnn@ and convert them to @DataRepr'@
findCustomReprAnnotations
  :: GHC.GhcMonad m
  => m [DataRepr']
findCustomReprAnnotations =
  map dataReprAnnToDataRepr' <$> findAllModuleAnnotations

-- | Find synthesize annotations and make sure each binder has no more than
-- a single annotation.
findSynthesizeAnnotations
  :: GHC.GhcMonad m
  => [CoreSyn.CoreBndr]
  -> m [(CoreSyn.CoreBndr, TopEntity)]
findSynthesizeAnnotations bndrs = do
  anns <- findNamedAnnotations bndrs
  pure (errOnDuplicateAnnotations "Synthesize" bndrs (map (filter isSyn) anns))
 where
  isSyn (Synthesize {}) = True
  isSyn _               = False

-- | Find testbench annotations and make sure that each binder has no more than
-- a single annotation.
findTestBenchAnnotations
  :: GHC.GhcMonad m
  => [CoreSyn.CoreBndr]
  -> m [(CoreSyn.CoreBndr,CoreSyn.CoreBndr)]
findTestBenchAnnotations bndrs = do
  anns0 <- findNamedAnnotations bndrs
  let anns1 = map (filter isTB) anns0
      anns2 = errOnDuplicateAnnotations "TestBench" bndrs anns1
  return (map (second findTB) anns2)
  where
    isTB (TestBench {}) = True
    isTB _              = False

    findTB :: TopEntity -> CoreSyn.CoreBndr
    findTB (TestBench tb) = case listToMaybe (filter (eqNm tb) bndrs) of
      Just tb' -> tb'
      Nothing  -> Panic.pgmError $
        "TestBench named: " ++ show tb ++ " not found"
    findTB _ = Panic.pgmError "Unexpected Synthesize"

    eqNm thNm bndr = Text.pack (show thNm) == qualNm
      where
        bndrNm  = Var.varName bndr
        qualNm  = maybe occName (\modName -> modName `Text.append` ('.' `Text.cons` occName)) (modNameM bndrNm)
        occName = Text.pack (OccName.occNameString (Name.nameOccName bndrNm))

-- | Find primitive annotations bound to given binders, or annotations made
-- in modules of those binders.
findPrimitiveAnnotations
  :: GHC.GhcMonad m
  => HDL
  -> [CoreSyn.CoreBndr]
  -> m [Either UnresolvedPrimitive FilePath]
findPrimitiveAnnotations hdl bndrs = do
  let
    annTargets =
     map
       (fmap Annotations.ModuleTarget . Name.nameModule_maybe)
       (map Var.varName bndrs)

  let
    targets =
      (catMaybes annTargets) ++
        (map (Annotations.NamedTarget . Var.varName) bndrs)

  anns <- findAnnotationsByTargets targets

  concat <$>
    mapM (getUnresolvedPrimitives hdl)
    (concat $ zipWith (\t -> map ((,) t)) targets anns)

parseModule :: GHC.GhcMonad m => GHC.ModSummary -> m GHC.ParsedModule
parseModule modSum = do
  (GHC.ParsedModule pmModSum pmParsedSource extraSrc anns) <-
    GHC.parseModule modSum
  return (GHC.ParsedModule
            (disableOptimizationsFlags pmModSum)
            pmParsedSource extraSrc anns)

disableOptimizationsFlags :: GHC.ModSummary -> GHC.ModSummary
disableOptimizationsFlags ms@(GHC.ModSummary {..})
  = ms {GHC.ms_hspp_opts = dflags}
  where
    dflags = unwantedOptimizationFlags (ms_hspp_opts
              { DynFlags.optLevel = 2
              , DynFlags.reductionDepth = 1000
              })

unwantedOptimizationFlags :: GHC.DynFlags -> GHC.DynFlags
unwantedOptimizationFlags df =
  foldl' DynFlags.xopt_unset
    (foldl' DynFlags.gopt_unset df unwanted) unwantedLang
  where
    unwanted = [ Opt_LiberateCase -- Perform unrolling of recursive RHS: avoid
               , Opt_SpecConstr -- Creates local-functions: avoid
               , Opt_IgnoreAsserts -- We don't care about assertions
               , Opt_DoEtaReduction -- We want eta-expansion
               , Opt_UnboxStrictFields -- Unboxed types are not handled properly: avoid
               , Opt_UnboxSmallStrictFields -- Unboxed types are not handled properly: avoid
#if !MIN_VERSION_ghc(8,6,0)
               , Opt_Vectorise -- Don't care
               , Opt_VectorisationAvoidance -- Don't care
#endif
               , Opt_RegsGraph -- Don't care
               , Opt_RegsGraph -- Don't care
               , Opt_PedanticBottoms -- Stops eta-expansion through case: avoid
               , Opt_LlvmTBAA -- Don't care
               , Opt_CmmSink -- Don't care
               , Opt_CmmElimCommonBlocks -- Don't care
               , Opt_OmitYields -- Don't care
               , Opt_IgnoreInterfacePragmas -- We need all the unfoldings we can get
               , Opt_OmitInterfacePragmas -- We need all the unfoldings we can get
               , Opt_IrrefutableTuples -- Introduce irrefutPatError: avoid
               , Opt_Loopification -- STG pass, don't care
               , Opt_CprAnal -- The worker/wrapper introduced by CPR breaks Clash, see [NOTE: CPR breaks Clash]
               , Opt_FullLaziness -- increases sharing, but seems to result in worse circuits (in both area and propagation delay)
               ]

    -- Coercions between Integer and Clash' numeric primitives cause Clash to
    -- fail. As strictness only affects simulation behavior, removing them
    -- is perfectly safe.
    unwantedLang = [ LangExt.Strict
                   , LangExt.StrictData
                   ]

-- [NOTE: CPR breaks Clash]
-- We used to completely disable strictness analysis because it causes GHC to
-- do the so-called "Constructed Product Result" (CPR) analysis, which in turn
-- creates an annoying worker/wrapper which does the following:
--
--   * Scrutinise a Signal, and pack the head and tail of the
--     Signal in an unboxed tuple.
--   * Scrutinise on the unboxed tuple, and recreate the Signal.
--
-- This is problematic because the 'Signal' type is essentially treated as a "transparent"
-- type by the Clash compiler, so observing its constructor leads to all kinds
-- of problems.
--
-- The current solution is to disable strictness analysis in "Clash.Signal.Internal"
-- so that functions manipulating 'Signal' constructor do not get a strictness/
-- demand/CPR annotation, which in turn ensures GHC doesn't create worker/wrappers
-- for when these functions are called in user code.
--
-- Ultimately we should stop treating Signal as a "transparent" type and deal
-- handling of the Signal type, and the involved co-recursive functions,
-- properly. At the moment, Clash cannot deal with this recursive type and the
-- recursive functions involved, and hence we need to disable this useful transformation. After
-- everything is done properly, we should enable it again.


setWantedLanguageExtensions :: GHC.DynFlags -> GHC.DynFlags
setWantedLanguageExtensions df =
   foldl' DynFlags.gopt_set
    (foldl' DynFlags.xopt_unset
      (foldl' DynFlags.xopt_set
        (DynFlags.wopt_set df DynFlags.Opt_WarnMonomorphism)
        wantedLanguageExtensions)
      unwantedLanguageExtensions)
    wantedOptimizations
 where
  wantedOptimizations =
    [ Opt_CSE -- CSE
    , Opt_Specialise -- Specialise on types, specialise type-class-overloaded function defined in this module for the types
    , Opt_DoLambdaEtaExpansion -- transform nested series of lambdas into one with multiple arguments, helps us achieve only top-level lambdas
    , Opt_CaseMerge -- We want fewer case-statements
    , Opt_DictsCheap -- Makes dictionaries seem cheap to optimizer: hopefully inline
    , Opt_ExposeAllUnfoldings -- We need all the unfoldings we can get
    , Opt_ForceRecomp -- Force recompilation: never bad
    , Opt_EnableRewriteRules -- Reduce number of functions
    , Opt_SimplPreInlining -- Inlines simple functions, we only care about the major first-order structure
    , Opt_StaticArgumentTransformation -- Turn on the static argument transformation, which turns a recursive function into a non-recursive one with a local recursive loop.
    , Opt_FloatIn -- Moves let-bindings inwards, although it defeats the normal-form with a single top-level let-binding, it helps with other transformations
    , Opt_DictsStrict -- Hopefully helps remove class method selectors
    , Opt_DmdTxDictSel -- I think demand and strictness are related, strictness helps with dead-code, enable
    , Opt_Strictness -- Strictness analysis helps with dead-code analysis. However, see [NOTE: CPR breaks Clash]
    , Opt_SpecialiseAggressively -- Needed to compile Fixed point number functions quickly
    , Opt_CrossModuleSpecialise -- Needed to compile Fixed point number functions quickly
    ]

-- | Remove all strictness annotations:
--
-- * Remove strictness annotations from data type declarations
--   (only works for data types that are currently being compiled, i.e.,
--    that are not part of a pre-compiled imported library)
--
-- We need to remove strictness annotations because GHC will introduce casts
-- between Integer and Clash' numeric primitives otherwise, where Clash will
-- error when it sees such casts. The reason it does this is because
-- Integer is a completely unconstrained integer type and is currently
-- (erroneously) translated to a 64-bit integer in the HDL; this means that
-- we could lose bits when the original numeric type had more bits than 64.
--
-- Removing these strictness annotations is perfectly safe, as they only
-- affect simulation behavior.
removeStrictnessAnnotations ::
     GHC.ParsedModule
  -> GHC.ParsedModule
removeStrictnessAnnotations pm =
    pm {GHC.pm_parsed_source = fmap rmPS (GHC.pm_parsed_source pm)}
  where
    -- rmPS :: GHC.DataId name => GHC.HsModule name -> GHC.HsModule name
    rmPS hsm = hsm {GHC.hsmodDecls = (fmap . fmap) rmHSD (GHC.hsmodDecls hsm)}

    -- rmHSD :: GHC.DataId name => GHC.HsDecl name -> GHC.HsDecl name
#if MIN_VERSION_ghc(8,6,0)
    rmHSD (GHC.TyClD x tyClDecl) = GHC.TyClD x (rmTyClD tyClDecl)
#else
    rmHSD (GHC.TyClD tyClDecl) = GHC.TyClD (rmTyClD tyClDecl)
#endif
    rmHSD hsd                  = hsd

    -- rmTyClD :: GHC.DataId name => GHC.TyClDecl name -> GHC.TyClDecl name
    rmTyClD dc@(GHC.DataDecl {}) = dc {GHC.tcdDataDefn = rmDataDefn (GHC.tcdDataDefn dc)}
    rmTyClD tyClD = tyClD

    -- rmDataDefn :: GHC.DataId name => GHC.HsDataDefn name -> GHC.HsDataDefn name
    rmDataDefn hdf = hdf {GHC.dd_cons = (fmap . fmap) rmCD (GHC.dd_cons hdf)}

    -- rmCD :: GHC.DataId name => GHC.ConDecl name -> GHC.ConDecl name
#if MIN_VERSION_ghc(8,6,0)
    rmCD gadt@(GHC.ConDeclGADT {}) = gadt {GHC.con_res_ty = rmHsType (GHC.con_res_ty gadt)
                                          ,GHC.con_args   = rmConDetails (GHC.con_args gadt)
                                          }
    rmCD h98@(GHC.ConDeclH98 {})   = h98  {GHC.con_args = rmConDetails (GHC.con_args h98)}
    rmCD xcon                      = xcon
#else
    rmCD gadt@(GHC.ConDeclGADT {}) = gadt {GHC.con_type = rmSigType (GHC.con_type gadt)}
    rmCD h98@(GHC.ConDeclH98 {})   = h98  {GHC.con_details = rmConDetails (GHC.con_details h98)}
#endif

    -- type LHsSigType name = HsImplicitBndrs name (LHsType name)
    -- rmSigType :: GHC.DataId name => GHC.LHsSigType name -> GHC.LHsSigType name
#if !MIN_VERSION_ghc(8,6,0)
    rmSigType hsIB = hsIB {GHC.hsib_body = rmHsType (GHC.hsib_body hsIB)}
#endif

    -- type HsConDeclDetails name = HsConDetails (LBangType name) (Located [LConDeclField name])
    -- rmConDetails :: GHC.DataId name => GHC.HsConDeclDetails name -> GHC.HsConDeclDetails name
    rmConDetails (GHC.PrefixCon args) = GHC.PrefixCon (fmap rmHsType args)
    rmConDetails (GHC.RecCon rec)     = GHC.RecCon ((fmap . fmap . fmap) rmConDeclF rec)
    rmConDetails (GHC.InfixCon l r)   = GHC.InfixCon (rmHsType l) (rmHsType r)

    -- rmHsType :: GHC.DataId name => GHC.Located (GHC.HsType name) -> GHC.Located (GHC.HsType name)
    rmHsType = transform go
      where
#if MIN_VERSION_ghc(8,6,0)
        go (GHC.unLoc -> GHC.HsBangTy _ _ ty) = ty
#else
        go (GHC.unLoc -> GHC.HsBangTy _ ty) = ty
#endif
        go ty                               = ty

    -- rmConDeclF :: GHC.DataId name => GHC.ConDeclField name -> GHC.ConDeclField name
    rmConDeclF cdf = cdf {GHC.cd_fld_type = rmHsType (GHC.cd_fld_type cdf)}

-- | The package id of the clash-prelude we were built with
preludePkgId :: String
preludePkgId = $(lift $ pkgIdFromTypeable (undefined :: TopEntity))

-- | Check that we're using the same clash-prelude as we were built with
--
-- Because if they differ clash won't be able to recognize any ANNotations.
checkForInvalidPrelude :: Monad m => HscTypes.ModGuts -> m ()
checkForInvalidPrelude guts =
  case filter isWrongPrelude pkgIds of
    []    -> return ()
    (x:_) -> throw (ClashException noSrcSpan (msgWrongPrelude x) Nothing)
  where
    pkgs = HscTypes.dep_pkgs . HscTypes.mg_deps $ guts
    pkgIds = map (GhcPlugins.installedUnitIdString . fst) pkgs
    prelude = "clash-prelude-"
    isPrelude pkg = case splitAt (length prelude) pkg of
      (x,y:_) | x == prelude && isDigit y -> True     -- check for a digit so we don't match clash-prelude-extras
      _ -> False
    isWrongPrelude pkg = isPrelude pkg && pkg /= preludePkgId
    msgWrongPrelude pkg = unlines ["Clash only works with the exact clash-prelude it was built with."
                                  ,"Clash was built with: " ++ preludePkgId
                                  ,"So can't run with:    " ++ pkg
                                  ]
