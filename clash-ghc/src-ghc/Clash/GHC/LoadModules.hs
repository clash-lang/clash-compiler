{-|
  Copyright   :  (C) 2013-2016, University of Twente,
                     2016-2017, Myrtle Software Ltd,
                     2017     , Google Inc.
                     2021     , QBayLogic B.V.
  License     :  BSD2 (see the file LICENSE)
  Maintainer  :  QBayLogic B.V. <devops@qbaylogic.com>
-}

{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
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
import           Control.Arrow                   (first)
import           Control.Exception               (SomeException, throw)
import           Control.Monad                   (forM, when)
import           Data.List.Extra                 (nubSort)
import           Control.Exception               (throwIO)
#if MIN_VERSION_ghc(9,0,0)
import           Control.Monad.Catch             as MC (try)
#endif
import           Control.Monad.IO.Class          (liftIO)
import           Data.Char                       (isDigit)
import           Data.Generics.Uniplate.DataOnly (transform)
import           Data.Data                       (Data)
import           Data.Functor                    ((<&>))
import           Data.Foldable                   (toList)
import           Data.HashMap.Strict             (HashMap)
import qualified Data.HashMap.Strict             as HashMap
import           Data.Typeable                   (Typeable)
import           Data.List                       (foldl', nub, find)
import qualified Data.Map                        as Map
import           Data.Maybe                      (catMaybes, fromMaybe, mapMaybe)
import qualified Data.Text                       as Text
import qualified Data.Text.Encoding              as Text
import qualified Data.Time.Clock                 as Clock
import qualified Data.Set                        as Set
import qualified Data.Sequence                   as Seq
import           Debug.Trace
import           Language.Haskell.TH.Syntax      (lift)
import           GHC.Natural                     (naturalFromInteger)
import           GHC.Stack                       (HasCallStack)
import           Text.Read                       (readMaybe)

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
#if MIN_VERSION_ghc(9,0,0)
#if MIN_VERSION_ghc(9,4,0)
import           GHC.Data.Bool (OverridingBool)
import           GHC.Driver.Config.Tidy (initTidyOpts)
import           GHC.Driver.Errors.Types (GhcMessage(GhcTcRnMessage))
import           GHC.Driver.Monad (modifySession)
import           GHC.Driver.Pipeline (compileOne')
import           GHC.Unit.Env (addHomeModInfoToHug)
#else
import           GHC.Utils.Misc (OverridingBool)
#endif
#if MIN_VERSION_ghc(9,2,0)
import qualified GHC.Driver.Env as HscTypes
import qualified GHC.Unit.Module.ModGuts as HscTypes
import qualified GHC.Types.SourceError as HscTypes
import qualified GHC.Unit.Module.Deps as HscTypes
import qualified GHC.Driver.Backend as Backend
import qualified GHC.Unit.Module.Graph as Graph
import qualified GHC.Platform.Ways as Ways
#if !MIN_VERSION_ghc(9,4,0)
import qualified GHC.Types.Error as Error
#endif
#else
import qualified GHC.Driver.Types as HscTypes
import qualified GHC.Driver.Ways as Ways
#endif
import qualified GHC.Types.Annotations as Annotations
import qualified GHC.Core.FVs as CoreFVs
import qualified GHC.Core as CoreSyn
import qualified GHC.Core.DataCon as DataCon
import qualified GHC.Data.Graph.Directed as Digraph
import qualified GHC.Runtime.Loader as DynamicLoading
import           GHC.Driver.Session (GeneralFlag (..))
import qualified GHC.Driver.Session as DynFlags
import qualified GHC.Data.FastString as FastString
import qualified GHC
import qualified GHC.Driver.Main as HscMain
import qualified GHC.Utils.Monad as MonadUtils
import qualified GHC.Utils.Panic as Panic
import qualified GHC.Serialized as Serialized (deserializeWithData)
import qualified GHC.Unit.Types as UnitTypes (unitIdString)
import qualified GHC.Tc.Utils.Monad as TcRnMonad
import qualified GHC.Tc.Types as TcRnTypes
import qualified GHC.Iface.Tidy as TidyPgm
import qualified GHC.Core.TyCon as TyCon
import qualified GHC.Core.Type as Type
import qualified GHC.Types.Unique as Unique
import qualified GHC.Tc.Instance.Family as FamInst
import qualified GHC.Core.FamInstEnv as FamInstEnv
import qualified GHC.LanguageExtensions as LangExt
import qualified GHC.Types.Name as Name
import qualified GHC.Types.Name.Occurrence as OccName
import           GHC.Utils.Outputable (ppr)
import qualified GHC.Utils.Outputable as Outputable
import qualified GHC.Types.Unique.Set as UniqSet
import qualified GHC.Types.Var as Var
import qualified GHC.Unit.Module.Env as ModuleEnv
import qualified GHC.Types.Name.Env as NameEnv
#else
import qualified Annotations
import qualified CoreFVs
import qualified CoreSyn
import qualified DataCon
import qualified Digraph
import qualified DynamicLoading
import           DynFlags                        (GeneralFlag (..))
import qualified DynFlags
import qualified Exception
import qualified FastString
import qualified GHC
import qualified HscMain
import qualified HscTypes
import qualified MonadUtils
import qualified Panic
import qualified GhcPlugins                      (deserializeWithData, installedUnitIdString)
import qualified TcRnMonad
import qualified TcRnTypes
import qualified TidyPgm
import qualified TyCon
import qualified Type
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
#endif

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

import           Clash.Signal.Internal

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
#if MIN_VERSION_ghc(9,0,0)
loadExternalModule hdl modName0 = MC.try $ do
#else
loadExternalModule hdl modName0 = Exception.gtry $ do
#endif
  let modName1 = GHC.mkModuleName modName0
  foundMod <- GHC.findModule modName1 Nothing
  let errMsg = "Internal error: found  module, but could not load it"
  modInfo <- fromMaybe (error errMsg) <$> (GHC.getModuleInfo foundMod)
  tyThings <- catMaybes <$> mapM GHC.lookupGlobalName (GHC.modInfoExports modInfo)
  let rootIds = [id_ | GHC.AnId id_ <- tyThings]
  loaded <- loadExternalBinders hdl rootIds
  let allBinders = makeRecursiveGroups (Map.assocs (lbBinders loaded))
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
        -- Make sure we read the .ghc environment files
        df <- do
          df <- GHC.getSessionDynFlags
#if MIN_VERSION_ghc(9,0,0)
#if MIN_VERSION_ghc(9,2,0)
          logger <- GHC.getLogger
          df1 <- liftIO (GHC.interpretPackageEnv logger df)
#else
          df1 <- liftIO (GHC.interpretPackageEnv df)
#endif
          _ <- GHC.setSessionDynFlags df1

#else
          _ <- GHC.setSessionDynFlags df {DynFlags.pkgDatabase = Nothing}
#endif
          GHC.getSessionDynFlags

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
                  { DynFlags.ghcMode  = GHC.CompManager
                  , DynFlags.ghcLink  = GHC.LinkInMemory
#if !MIN_VERSION_ghc(9,4,0)
                  , DynFlags.optLevel = 2
#endif
#if MIN_VERSION_ghc(9,2,0)
                  , DynFlags.backend  =
                      if Ways.hostIsProfiled
                         then Backend.NoBackend
                         else Backend.platformDefaultBackend (DynFlags.targetPlatform dflags)
#else
                  , DynFlags.hscTarget
#if MIN_VERSION_ghc(9,0,0)
                      = if Ways.hostIsProfiled
#else
                      = if DynFlags.rtsIsProfiled
#endif
                           then DynFlags.HscNothing
                           else DynFlags.defaultObjectTarget $
#if !MIN_VERSION_ghc(8,10,0)
                                  DynFlags.targetPlatform
#endif
                                    dflags
#endif
                  , DynFlags.reductionDepth = 1000
                  }
  let dflags2 = unwantedOptimizationFlags dflags1
      ghcDynamic = case lookup "GHC Dynamic" (DynFlags.compilerInfo dflags) of
                    Just "YES" -> True
                    _          -> False
      dflags3 = if ghcDynamic then DynFlags.gopt_set dflags2 DynFlags.Opt_BuildDynamicToo
                              else dflags2

  when (DynFlags.gopt DynFlags.Opt_WorkerWrapper dflags3) $
    trace
      (unlines ["WARNING:"
               ,"`-fworker-wrapper` option is globally enabled, this can result in incorrect code."
               ,"Are you compiling with `-O` or `-O2`? Consider adding `-fno-worker-wrapper`."
               ,"`-fworker-wrapper` can be use in a diligent manner on a file-by-file basis"
               ,"by using a `{-# OPTIONS_GHC -fworker-wrapper` #-} pragma."
               ])
      (return ())

#if MIN_VERSION_ghc(9,2,0)
  _ <- GHC.setSessionDynFlags dflags3
  hscenv <- GHC.getSession
  hscenv1 <- MonadUtils.liftIO (DynamicLoading.initializePlugins hscenv)
  GHC.setSession hscenv1
#elif MIN_VERSION_ghc(9,0,0)
  _ <- GHC.setSessionDynFlags dflags3
  hscenv <- GHC.getSession
  dflags4 <- MonadUtils.liftIO (DynamicLoading.initializePlugins hscenv dflags3)
  _ <- GHC.setSessionDynFlags dflags4
#else
  hscenv <- GHC.getSession
  dflags4 <- MonadUtils.liftIO (DynamicLoading.initializePlugins hscenv dflags3)
  _ <- GHC.setSessionDynFlags dflags4
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
#if MIN_VERSION_ghc(9,4,0)
  target <- GHC.guessTarget modName Nothing Nothing
#else
  target <- GHC.guessTarget modName Nothing
#endif
  GHC.setTargets [target]
  modGraph <- GHC.depanal [] False
  let modGraph' = GHC.mapMG disableOptimizationsFlags modGraph
      -- 'topSortModuleGraph' ensures that modGraph2, and hence tidiedMods
      -- are in topological order, i.e. the root module is last.
      modGraph2 = Digraph.flattenSCCs $
#if MIN_VERSION_ghc(9,2,0)
                  -- TODO: this might break backpack
                  Graph.filterToposortToModules $
#endif
                  GHC.topSortModuleGraph True modGraph' Nothing

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
#if MIN_VERSION_ghc(9,4,0)
    hsc_env_tc <- GHC.getSession
    mod_info <- liftIO $ compileOne' Nothing hsc_env_tc m 1 1 Nothing Nothing
    modifySession $ HscTypes.hscUpdateHUG (addHomeModInfoToHug mod_info)
    let tcMod' = tcMod
#else
    tcMod' <- GHC.loadModule tcMod
#endif
    dsMod <- fmap GHC.coreModule $ GHC.desugarModule tcMod'
    hsc_env <- GHC.getSession
    simpl_guts <- MonadUtils.liftIO $ HscMain.hscSimplify hsc_env [] dsMod
    checkForInvalidPrelude simpl_guts
#if MIN_VERSION_ghc(9,4,0)
    opts <- liftIO (initTidyOpts hsc_env)
    (tidy_guts,_) <- MonadUtils.liftIO $ TidyPgm.tidyProgram opts simpl_guts
#else
    (tidy_guts,_) <- MonadUtils.liftIO $ TidyPgm.tidyProgram hsc_env simpl_guts
#endif
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
  loaded0 <- loadExternalExprs hdl (concat binders)

  -- Find local primitive annotations
  localPrims <- findPrimitiveAnnotations hdl binderIds
  let loaded1 = loaded0{lbPrims=lbPrims loaded0 <> Seq.fromList localPrims}

  let allBinders = concat binders ++ makeRecursiveGroups (Map.assocs (lbBinders loaded0))
  pure (rootIds, modFamInstEnvs', rootModule, loaded1, allBinders)

nameString :: Name.Name -> String
nameString = OccName.occNameString . Name.nameOccName

varNameString :: Var.Var -> String
varNameString = nameString . Var.varName

loadModules
  :: GHC.Ghc ()
  -- ^ Allows us to have some initial action, such as sharing a linker state
  -- See https://github.com/clash-lang/clash-compiler/issues/1686 and
  -- https://mail.haskell.org/pipermail/ghc-devs/2021-March/019605.html
  -> OverridingBool
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
        , [(CoreSyn.CoreBndr, Maybe TopEntity, Bool)]  -- binder + synthesize annotation + is testbench?
        , [Either UnresolvedPrimitive FilePath]
        , [DataRepr']
        , [(Text.Text, PrimitiveGuard ())]
        , HashMap Text.Text VDomainConfiguration -- domain names to configuration
        )
loadModules startAction useColor hdl modName dflagsM idirs = do
  libDir <- MonadUtils.liftIO ghcLibDir
  startTime <- Clock.getCurrentTime
  GHC.runGhc (Just libDir) $ do
    startAction
    -- 'mainFunIs' is set to Nothing due to issue #1304:
    -- https://github.com/clash-lang/clash-compiler/issues/1304
    setupGhc useColor ((\d -> d{GHC.mainFunIs=Nothing}) <$> dflagsM) idirs
    setupTime <- MonadUtils.liftIO Clock.getCurrentTime
    let setupStartDiff = reportTimeDiff setupTime startTime
    MonadUtils.liftIO $ putStrLn $ "GHC: Setting up GHC took: " ++ setupStartDiff

    -- TODO: We currently load the transitive closure of _all_ bindings found
    -- TODO: in the top module. This is wasteful if one or more binders don't
    -- TODO: contribute to any top entities. This effect is worsened when using
    -- TODO: -main-is, which only synthesizes a single top entity (and all its
    -- TODO: dependencies).
    (rootIds, modFamInstEnvs, _rootModule, LoadedBinders{..}, allBinders) <-
      -- We need to try and load external modules first, because we can't
      -- recover from errors in 'loadLocalModule'.
      loadExternalModule hdl modName >>= \case
        Left _loadExternalErr -> loadLocalModule hdl modName
        Right res -> pure res

    let allBinderIds = map fst (CoreSyn.flattenBinds allBinders)

    modTime <- length allBinderIds `seq` MonadUtils.liftIO Clock.getCurrentTime
    let modStartDiff = reportTimeDiff modTime setupTime
    MonadUtils.liftIO $ putStrLn $ "GHC: Compiling and loading modules took: " ++ modStartDiff

    -- Get type family instances: accumulated by GhcMonad during
    -- 'loadExternalBinders' / 'loadExternalExprs'
    hscEnv <- GHC.getSession
    famInstEnvs <- do
      (msgs, m) <- TcRnMonad.liftIO $ TcRnMonad.initTcInteractive hscEnv FamInst.tcGetFamInstEnvs
      case m of
        Nothing -> TcRnMonad.liftIO $ throwIO
                                    $ HscTypes.mkSrcErr
#if MIN_VERSION_ghc(9,4,0)
                                    $ fmap GhcTcRnMessage msgs
#elif MIN_VERSION_ghc(9,2,0)
                                    $ Error.getErrorMessages msgs
#else
                                    $ snd msgs
#endif
        Just x  -> return x

    allSyn     <- Map.fromList <$> findSynthesizeAnnotations allBinderIds
    topSyn     <- map fst <$> findSynthesizeAnnotations rootIds
    benchAnn   <- findTestBenches rootIds
    reprs'     <- findCustomReprAnnotations
    primGuards <- findPrimitiveGuardAnnotations allBinderIds
    let
      -- All binders synthesized with Synthesize, all binders annotated with
      -- TestBench and the binders they're pointing to, plus magically named
      -- functions called "topEntity" or "testBench". Synthesized in case user
      -- didn't specify a particular target.
      isMagicName = (`elem` ["topEntity", "testBench"])
      allImplicit = nubSort $
           Map.keys benchAnn
        <> Map.keys allSyn
        <> concat (Map.elems benchAnn)
        <> filter (isMagicName . varNameString) rootIds
        <> topSyn

      -- Top entities we wish to synthesize. Users can filter these with -main-is.
      topEntities1 =
        case GHC.mainFunIs =<< dflagsM of
          Just mainIsNm ->
            -- Use requested top entity.
            --
            -- TODO: Look up associated test benches in 'benchAnn'. This would
            --       be wasted effort if implemented right now, as 'getMainTopEntity'
            --       would later remove them again. Functionality of that function
            --       should be moved here.
            --
            -- TODO: Handle fully qualified names to -main-is
            case find ((==mainIsNm) . varNameString) rootIds of
              Nothing ->
                Panic.pgmError [I.i|
                  No top-level function called '#{mainIsNm}' found. Did you
                  forget to export it?
                |]
              Just top ->
                -- Note that we return /all/ top entities here, even the ones
                -- we don't which to synthesize. 'getMainTopEntity' will later
                -- restrict this to just this top entity (and its dependencies,
                -- which is why we return everything in the first place).
                --
                -- This is quite wasteful though; als Clash will load all
                -- definitions even though it will end up using just a few. TODO
                nubSort (top:allImplicit)
          Nothing ->
            -- User didn't specify anything.
            case allImplicit of
              [] ->
                Panic.pgmError [I.i|
                  No top-level function called 'topEntity' or 'testBench' found,
                  nor any function annotated with a 'Synthesize' or 'TestBench'
                  annotation. If you want to synthesize a specific binder in
                  #{show modName}, use '-main-is myTopEntity'.
                |]
              _ ->
                allImplicit

      -- Include whether found top entity is a test bench
      allBenchIds = Set.fromList (concat (Map.elems benchAnn))
      topEntities2 = topEntities1 <&> \tid ->
        ( tid
        , tid `Map.lookup` allSyn       -- include top entity annotation (if any)
        , tid `Set.member` allBenchIds  -- indicate whether top entity is test bench
        )

    let reprs1 = lbReprs <> Seq.fromList reprs'

    let famInstEnvs' = (fst famInstEnvs, modFamInstEnvs)
        allTCInsts   = FamInstEnv.famInstEnvElts (fst famInstEnvs')
                         ++ FamInstEnv.famInstEnvElts (snd famInstEnvs')

        knownConfs   = filter (\x -> "KnownConf" == nameString (FamInstEnv.fi_fam x)) allTCInsts

#if MIN_VERSION_ghc(8,10,0)
        fsToText     = Text.decodeUtf8 . FastString.bytesFS
#else
        fsToText     = Text.decodeUtf8 . FastString.fastStringToByteString
#endif

        famToDomain  = fromMaybe (error "KnownConf: Expected Symbol at LHS of type family")
                         . fmap fsToText . Type.isStrLitTy . head . FamInstEnv.fi_tys
        famToConf    = unpackKnownConf . FamInstEnv.fi_rhs

        knownConfNms = fmap famToDomain knownConfs
        knownConfDs  = fmap famToConf knownConfs

        knownConfMap = HashMap.fromList (zip knownConfNms knownConfDs)

    return ( allBinders
           , Map.assocs lbClassOps
           , Set.toList lbUnlocatable
           , famInstEnvs'
           , topEntities2
           , toList lbPrims
           , toList reprs1
           , primGuards
           , knownConfMap
           )

-- | Given a type that represents the RHS of a KnownConf type family instance,
-- unpack the fields of the DomainConfiguration and make a VDomainConfiguration.
--
unpackKnownConf :: Type.Type -> VDomainConfiguration
unpackKnownConf ty
  | [d,p,ae,rk,ib,rp] <- Type.tyConAppArgs ty
    -- Domain name
  , Just dom <- fmap FastString.unpackFS (Type.isStrLitTy d)
    -- Period
  , Just period <- fmap naturalFromInteger (Type.isNumLitTy p)
    -- Active Edge
  , aeTc <- Type.tyConAppTyCon ae
  , Just aeDc <- TyCon.isPromotedDataCon_maybe aeTc
  , aeNm <- OccName.occNameString $ Name.nameOccName (DataCon.dataConName aeDc)
    -- Reset Kind
  , rkTc <- Type.tyConAppTyCon rk
  , Just rkDc <- TyCon.isPromotedDataCon_maybe rkTc
  , rkNm <- OccName.occNameString $ Name.nameOccName (DataCon.dataConName rkDc)
    -- Init Behavior
  , ibTc <- Type.tyConAppTyCon ib
  , Just ibDc <- TyCon.isPromotedDataCon_maybe ibTc
  , ibNm <- OccName.occNameString $ Name.nameOccName (DataCon.dataConName ibDc)
    -- Reset Polarity
  , rpTc <- Type.tyConAppTyCon rp
  , Just rpDc <- TyCon.isPromotedDataCon_maybe rpTc
  , rpNm <- OccName.occNameString $ Name.nameOccName (DataCon.dataConName rpDc)
  = VDomainConfiguration dom period
      (asActiveEdge aeNm)
      (asResetKind rkNm)
      (asInitBehavior ibNm)
      (asResetPolarity rpNm)

  | otherwise
  = error $ $(curLoc) ++ "Could not unpack domain configuration."
 where
  asActiveEdge :: HasCallStack => String -> ActiveEdge
  asActiveEdge x = fromMaybe (error $ $(curLoc) ++ "Unknown active edge: " ++ show x) (readMaybe x)

  asResetKind :: HasCallStack => String -> ResetKind
  asResetKind x = fromMaybe (error $ $(curLoc) ++ "Unknown reset kind: " ++ show x) (readMaybe x)

  asInitBehavior :: HasCallStack => String -> InitBehavior
  asInitBehavior x = fromMaybe (error $ $(curLoc) ++ "Unknown init behavior: " ++ show x) (readMaybe x)

  asResetPolarity :: HasCallStack => String -> ResetPolarity
  asResetPolarity x = fromMaybe (error $ $(curLoc) ++ "Unknown reset polarity: " ++ show x) (readMaybe x)

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
      Digraph.DigraphNode
        (b,e)
        (Var.varUnique b)
        (UniqSet.nonDetKeysUniqSet (CoreFVs.exprSomeFreeVars Var.isId e))

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
#if MIN_VERSION_ghc(9,0,0)
  mapM (GHC.findGlobalAnns Serialized.deserializeWithData) targets
#else
  mapM (GHC.findGlobalAnns GhcPlugins.deserializeWithData) targets
#endif

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
#if MIN_VERSION_ghc(9,4,0)
        $ (\(mEnv,nEnv) -> ModuleEnv.moduleEnvElts mEnv <> NameEnv.nonDetNameEnvElts nEnv)
#elif MIN_VERSION_ghc(9,0,0)
         $ (\(mEnv,nEnv) -> ModuleEnv.moduleEnvElts mEnv <> NameEnv.nameEnvElts nEnv)
#else
         $ UniqFM.nonDetEltsUFM
#endif
         $ Annotations.deserializeAnns
#if MIN_VERSION_ghc(9,0,0)
              Serialized.deserializeWithData
#else
              GhcPlugins.deserializeWithData
#endif
              ann_env

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

-- | Find test bench annotations and return a map tying top entities to their
-- test benches. If there is a binder called @testBench@ _without_ an annotation
-- it assumed to belong to a binder called @topEntity@. If the latter does not
-- exist, the function @testBench@ is left alone.
findTestBenches ::
  GHC.GhcMonad m =>
  -- | Root binders
  [CoreSyn.CoreBndr] ->
  -- | (design under test, associated test benches)
  m (Map.Map CoreSyn.CoreBndr [CoreSyn.CoreBndr])
findTestBenches bndrs0 = do
  anns <- findNamedAnnotations bndrs0
  let
    duts0 = foldl' insertTb Map.empty (concat (zipWith go0 bndrs0 anns))
    duts1 = specialCaseMagicName duts0
  pure duts1
 where
  insertTb m (dut, tb) = Map.insertWith (<>) dut [tb] m
  bndrsMap = HashMap.fromList (map (\x -> (toQualNm x, x)) bndrs0)

  -- Special case magic name 'testBench'. See function documentation.
  specialCaseMagicName m =
    let
      topEntM = find ((=="topEntity") . varNameString) bndrs0
      tbM = find ((=="testBench") . varNameString) bndrs0
    in
      case (topEntM, tbM) of
        (Just dut, Just tb) -> insertTb m (dut, tb)
        _ -> m

  -- go0 + go1: map over all annotations; look for test bench annotations and
  -- tie them to top entities indicated in the annotation.
  go0 bndr anns = mapMaybe (go1 bndr) anns
  go1 tbBndr (TestBench dutNm) =
    case HashMap.lookup (Text.pack (show dutNm)) bndrsMap of
      Nothing ->
        Panic.pgmError [I.i|
          Could not find design under test #{show (show dutNm)}, associated with
          test bench #{show (toQualNm tbBndr)}. Note that testbenches should be
          exported from the same module as the design under test.
        |]
      Just dutBndr ->
        Just (dutBndr, tbBndr)
  go1 _ _ = Nothing

-- | Create a fully qualified name from a var, excluding package. Example
-- output: @Clash.Sized.Internal.BitVector.low@.
toQualNm :: Var.Var -> Text.Text
toQualNm bndr =
  let
    bndrNm  = Var.varName bndr
    occName = Text.pack (OccName.occNameString (Name.nameOccName bndrNm))
  in
    maybe
      occName
      (\modName -> modName `Text.append` ('.' `Text.cons` occName))
      (modNameM bndrNm)

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
#if MIN_VERSION_ghc(9,2,0)
  (GHC.ParsedModule pmModSum pmParsedSource extraSrc) <-
    GHC.parseModule modSum
  return (GHC.ParsedModule
            (disableOptimizationsFlags pmModSum)
            pmParsedSource extraSrc)
#else
  (GHC.ParsedModule pmModSum pmParsedSource extraSrc anns) <-
    GHC.parseModule modSum
  return (GHC.ParsedModule
            (disableOptimizationsFlags pmModSum)
            pmParsedSource extraSrc anns)
#endif

disableOptimizationsFlags :: GHC.ModSummary -> GHC.ModSummary
disableOptimizationsFlags ms@(GHC.ModSummary {..})
  = ms {GHC.ms_hspp_opts = dflags}
  where
    dflags = unwantedOptimizationFlags (ms_hspp_opts
              { DynFlags.reductionDepth = 1000
#if !MIN_VERSION_ghc(9,4,0)
              , DynFlags.optLevel = 2
#endif
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
      (foldl' DynFlags.xopt_set df wantedLanguageExtensions)
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
    rmHSD (GHC.TyClD x tyClDecl) = GHC.TyClD x (rmTyClD tyClDecl)
    rmHSD hsd                  = hsd

    -- rmTyClD :: GHC.DataId name => GHC.TyClDecl name -> GHC.TyClDecl name
    rmTyClD dc@(GHC.DataDecl {}) = dc {GHC.tcdDataDefn = rmDataDefn (GHC.tcdDataDefn dc)}
    rmTyClD tyClD = tyClD

    -- rmDataDefn :: GHC.DataId name => GHC.HsDataDefn name -> GHC.HsDataDefn name
#if MIN_VERSION_ghc(9,2,0)
    rmDataDefn :: GHC.HsDataDefn GHC.GhcPs -> GHC.HsDataDefn GHC.GhcPs
#endif
    rmDataDefn hdf = hdf {GHC.dd_cons = (fmap . fmap) rmCD (GHC.dd_cons hdf)}

    -- rmCD :: GHC.DataId name => GHC.ConDecl name -> GHC.ConDecl name
    rmCD gadt@(GHC.ConDeclGADT {}) = gadt {GHC.con_res_ty = rmHsType (GHC.con_res_ty gadt)
#if MIN_VERSION_ghc(9,2,0)
                                          ,GHC.con_g_args = rmGConDetails (GHC.con_g_args gadt)
#else
                                          ,GHC.con_args   = rmConDetails (GHC.con_args gadt)
#endif
                                          }
    rmCD h98@(GHC.ConDeclH98 {})   = h98  {GHC.con_args = rmConDetails (GHC.con_args h98)}
#if !MIN_VERSION_ghc(9,0,0)
    rmCD xcon                      = xcon
#endif

#if MIN_VERSION_ghc(9,4,0)
    rmGConDetails :: GHC.HsConDeclGADTDetails GHC.GhcPs -> GHC.HsConDeclGADTDetails GHC.GhcPs
    rmGConDetails (GHC.PrefixConGADT args) = GHC.PrefixConGADT (fmap rmHsScaledType args)
    rmGConDetails (GHC.RecConGADT rec tkn) = GHC.RecConGADT ((fmap . fmap . fmap) rmConDeclF rec) tkn
#elif MIN_VERSION_ghc(9,2,0)
    rmGConDetails :: GHC.HsConDeclGADTDetails GHC.GhcPs -> GHC.HsConDeclGADTDetails GHC.GhcPs
    rmGConDetails (GHC.PrefixConGADT args) = GHC.PrefixConGADT (fmap rmHsScaledType args)
    rmGConDetails (GHC.RecConGADT rec)     = GHC.RecConGADT ((fmap . fmap . fmap) rmConDeclF rec)
#endif

    -- type HsConDeclDetails name = HsConDetails (LBangType name) (Located [LConDeclField name])
    -- rmConDetails :: _ => GHC.HsConDeclDetails name -> GHC.HsConDeclDetails name
#if MIN_VERSION_ghc(9,2,0)
    rmConDetails (GHC.PrefixCon tys args) = GHC.PrefixCon tys (fmap rmHsScaledType args)
    rmConDetails (GHC.InfixCon l r)   = GHC.InfixCon (rmHsScaledType l) (rmHsScaledType r)
#elif MIN_VERSION_ghc(9,0,0)
    rmConDetails (GHC.PrefixCon args) = GHC.PrefixCon (fmap rmHsScaledType args)
    rmConDetails (GHC.InfixCon l r)   = GHC.InfixCon (rmHsScaledType l) (rmHsScaledType r)
#else
    rmConDetails (GHC.PrefixCon args) = GHC.PrefixCon (fmap rmHsType args)
    rmConDetails (GHC.InfixCon l r)   = GHC.InfixCon (rmHsType l) (rmHsType r)
#endif
    rmConDetails (GHC.RecCon rec)     = GHC.RecCon ((fmap . fmap . fmap) rmConDeclF rec)


    -- rmHsType :: GHC.DataId name => GHC.Located (GHC.HsType name) -> GHC.Located (GHC.HsType name)
    rmHsType = transform go
      where
#if MIN_VERSION_ghc(9,2,0)
        go ::
          GHC.LBangType GHC.GhcPs ->
          GHC.LBangType GHC.GhcPs
#endif
        go (GHC.unLoc -> GHC.HsBangTy _ _ ty) = ty
        go ty                               = ty

#if MIN_VERSION_ghc(9,0,0)
    rmHsScaledType = transform go
      where
#if MIN_VERSION_ghc(9,2,0)
        go ::
          GHC.HsScaled GHC.GhcPs (GHC.LBangType GHC.GhcPs) ->
          GHC.HsScaled GHC.GhcPs (GHC.LBangType GHC.GhcPs)
#endif
        go (GHC.HsScaled m (GHC.unLoc -> GHC.HsBangTy _ _ ty)) = GHC.HsScaled m ty
        go ty = ty
#endif

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
#if MIN_VERSION_ghc(9,4,0)
    pkgs = HscTypes.dep_direct_pkgs . HscTypes.mg_deps $ guts
#else
    pkgs = HscTypes.dep_pkgs . HscTypes.mg_deps $ guts
#endif
#if MIN_VERSION_ghc(9,4,0)
    pkgIds = map (UnitTypes.unitIdString) (toList pkgs)
#elif MIN_VERSION_ghc(9,0,0)
    pkgIds = map (UnitTypes.unitIdString . fst) pkgs
#else
    pkgIds = map (GhcPlugins.installedUnitIdString . fst) pkgs
#endif
    prelude = "clash-prelude-"
    isPrelude pkg = case splitAt (length prelude) pkg of
      (x,y:_) | x == prelude && isDigit y -> True     -- check for a digit so we don't match clash-prelude-extras
      _ -> False
    isWrongPrelude pkg = isPrelude pkg && pkg /= preludePkgId
    msgWrongPrelude pkg = unlines ["Clash only works with the exact clash-prelude it was built with."
                                  ,"Clash was built with: " ++ preludePkgId
                                  ,"So can't run with:    " ++ pkg
                                  ]
