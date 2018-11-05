{-|
  Copyright   :  (C) 2013-2016, University of Twente,
                     2016-2017, Myrtle Software Ltd,
                     2017     , Google Inc.
  License     :  BSD2 (see the file LICENSE)
  Maintainer  :  Christiaan Baaij <christiaan.baaij@gmail.com>
-}

{-# LANGUAGE CPP                 #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE ViewPatterns        #-}

module Clash.GHC.LoadModules
  ( loadModules
  , ghcLibDir
  )
where

#ifndef TOOL_VERSION_ghc
#error TOOL_VERSION_ghc undefined
#endif

-- External Modules
import           Clash.Annotations.Primitive     (HDL, Primitive (..))
import           Clash.Annotations.TopEntity     (TopEntity (..))
import           Control.Arrow                   (second)
#if MIN_VERSION_ghc(8,6,0)
import           Control.Exception               (throwIO)
#endif
import           Control.Monad.IO.Class          (liftIO)
import           Data.Generics.Uniplate.DataOnly (transform)
import           Data.List                       (foldl', lookup, nub)
import           Data.Maybe                      (catMaybes, fromMaybe,
                                                  listToMaybe, mapMaybe)
import qualified Data.Text                       as Text
import           Data.Word                       (Word8)
import           System.Exit                     (ExitCode (..))
import           System.IO                       (hGetLine)
import           System.IO.Error                 (tryIOError)
import           System.Process                  (runInteractiveCommand,
                                                  waitForProcess)

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
import qualified GHC
import qualified HscMain
import qualified HscTypes
import qualified MonadUtils
import qualified Panic
import qualified GhcPlugins                      (deserializeWithData)
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
import qualified Var

-- Internal Modules
import           Clash.Annotations.BitRepresentation          (DataReprAnn)
import           Clash.GHC.GHC2Core                           (modNameM)
import           Clash.GHC.LoadInterfaceFiles                 (loadExternalExprs, primitiveFilePath)
import           Clash.Util                                   (curLoc)
import           Clash.Annotations.BitRepresentation.Internal
  (DataRepr', dataReprAnnToDataRepr')

ghcLibDir :: IO FilePath
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

loadModules
  :: HDL
  -> String
  -> Maybe (DynFlags.DynFlags)
  -> IO ( [CoreSyn.CoreBind]                     -- Binders
        , [(CoreSyn.CoreBndr,Int)]               -- Class operations
        , [CoreSyn.CoreBndr]                     -- Unlocatable Expressions
        , FamInstEnv.FamInstEnvs
        , [( CoreSyn.CoreBndr                    -- topEntity bndr
           , Maybe TopEntity                     -- (maybe) TopEntity annotation
           , Maybe CoreSyn.CoreBndr)]            -- (maybe) testBench bndr
        , [FilePath]
        , [DataRepr']
        )
loadModules hdl modName dflagsM = do
  libDir <- MonadUtils.liftIO ghcLibDir

  GHC.runGhc (Just libDir) $ do
    dflags <- case dflagsM of
                Just df -> return df
                Nothing -> do
#if MIN_VERSION_ghc(8,6,0)
                  -- Make sure we read the .ghc environment files
                  df <- do { df <- GHC.getSessionDynFlags
                           ; _ <- GHC.setSessionDynFlags df {DynFlags.pkgDatabase = Nothing}
                           ; GHC.getSessionDynFlags
                           }
#else
                  df <- GHC.getSessionDynFlags
#endif
                  let dfEn = foldl DynFlags.xopt_set df
                                [ LangExt.TemplateHaskell
                                , LangExt.TemplateHaskellQuotes
                                , LangExt.DataKinds
                                , LangExt.MonoLocalBinds
                                , LangExt.TypeOperators
                                , LangExt.FlexibleContexts
                                , LangExt.ConstraintKinds
                                , LangExt.TypeFamilies
                                , LangExt.BinaryLiterals
                                , LangExt.ExplicitNamespaces
                                , LangExt.KindSignatures
                                , LangExt.DeriveLift
                                , LangExt.TypeApplications
                                , LangExt.ScopedTypeVariables
                                , LangExt.MagicHash
                                , LangExt.ExplicitForAll
                                , LangExt.QuasiQuotes
                                ]
                  let dfDis = foldl DynFlags.xopt_unset dfEn
                                [ LangExt.ImplicitPrelude
                                , LangExt.MonomorphismRestriction
                                , LangExt.Strict
                                , LangExt.StrictData
                                ]
                  let ghcTyLitNormPlugin = GHC.mkModuleName "GHC.TypeLits.Normalise"
                      ghcTyLitExtrPlugin = GHC.mkModuleName "GHC.TypeLits.Extra.Solver"
                      ghcTyLitKNPlugin   = GHC.mkModuleName "GHC.TypeLits.KnownNat.Solver"
                  let dfPlug = dfDis { DynFlags.pluginModNames = nub $
                                          ghcTyLitNormPlugin : ghcTyLitExtrPlugin :
                                          ghcTyLitKNPlugin : DynFlags.pluginModNames dfDis
                                     }
                  return dfPlug

    let dflags1 = dflags
                    { DynFlags.optLevel = 2
                    , DynFlags.ghcMode  = GHC.CompManager
                    , DynFlags.ghcLink  = GHC.LinkInMemory
                    , DynFlags.hscTarget
                        = if DynFlags.rtsIsProfiled
                             then DynFlags.HscNothing
                             else DynFlags.defaultObjectTarget
                                    (DynFlags.targetPlatform dflags)
                    , DynFlags.reductionDepth = 1000
                    }
    let dflags2 = wantedOptimizationFlags dflags1
    let ghcDynamic = case lookup "GHC Dynamic" (DynFlags.compilerInfo dflags) of
                      Just "YES" -> True
                      _          -> False
    let dflags3 = if ghcDynamic then DynFlags.gopt_set dflags2 DynFlags.Opt_BuildDynamicToo
                                else dflags2
#if MIN_VERSION_ghc(8,6,0)
    hscenv <- GHC.getSession
    dflags4 <- MonadUtils.liftIO (DynamicLoading.initializePlugins hscenv dflags3)
    _ <- GHC.setSessionDynFlags dflags4
#else
    _ <- GHC.setSessionDynFlags dflags3
#endif
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
    tidiedMods <- mapM (\m -> do { pMod  <- parseModule m
                                 ; tcMod <- GHC.typecheckModule (removeStrictnessAnnotations pMod)
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
                                 ; tcMod' <- GHC.loadModule tcMod
                                 ; dsMod <- fmap GHC.coreModule $ GHC.desugarModule tcMod'
                                 ; hsc_env <- GHC.getSession
#if MIN_VERSION_ghc(8,4,1)
                                 ; simpl_guts <- MonadUtils.liftIO $ HscMain.hscSimplify hsc_env [] dsMod
#else
                                 ; simpl_guts <- MonadUtils.liftIO $ HscMain.hscSimplify hsc_env dsMod
#endif
                                 ; (tidy_guts,_) <- MonadUtils.liftIO $ TidyPgm.tidyProgram hsc_env simpl_guts
                                 ; let pgm        = HscTypes.cg_binds tidy_guts
                                 ; let modFamInstEnv = TcRnTypes.tcg_fam_inst_env $ fst $ GHC.tm_internals_ tcMod
                                 ; return (pgm,modFamInstEnv)
                                 }
                         ) modGraph2

    let (binders,modFamInstEnvs) = unzip tidiedMods
        bindersC                 = concat binders
        binderIds                = map fst (CoreSyn.flattenBinds bindersC)
        plusFamInst f1 f2        = FamInstEnv.extendFamInstEnvList f1 (FamInstEnv.famInstEnvElts f2)
        modFamInstEnvs'          = foldl' plusFamInst FamInstEnv.emptyFamInstEnv modFamInstEnvs

    (externalBndrs,clsOps,unlocatable,pFP,reprs) <-
      loadExternalExprs hdl (UniqSet.mkUniqSet binderIds) bindersC

    -- Find local primitive annotations
    pFP' <- findPrimitiveAnnotations hdl binderIds

    hscEnv <- GHC.getSession
#if MIN_VERSION_ghc(8,6,0)
    famInstEnvs <- do { (msgs,m) <- TcRnMonad.liftIO $ TcRnMonad.initTcInteractive hscEnv FamInst.tcGetFamInstEnvs
                      ; case m of
                          Nothing -> TcRnMonad.liftIO $ throwIO (HscTypes.mkSrcErr (snd msgs))
                          Just x  -> return x
                      }
#else
    famInstEnvs <- TcRnMonad.liftIO $ TcRnMonad.initTcForLookup hscEnv FamInst.tcGetFamInstEnvs
#endif

    -- Because tidiedMods is in topological order, binders is also, and hence
    -- the binders belonging to the "root" module are the last binders
    let rootModule = GHC.ms_mod_name . last $ modGraph2
        rootIds    = map fst . CoreSyn.flattenBinds $ last binders

    -- Because tidiedMods is in topological order, binders is also, and hence
    -- allSyn is in topological order. This means that the "root" 'topEntity'
    -- will be compiled last.
    allSyn   <- findSynthesizeAnnotations binderIds
    benchAnn <- findTestBenchAnnotations binderIds
    topSyn   <- findSynthesizeAnnotations rootIds
    reprs'   <- findCustomReprAnnotations
    let varNameString = OccName.occNameString . Name.nameOccName . Var.varName
        topEntities = filter ((== "topEntity") . varNameString) rootIds
        benches     = filter ((== "testBench") . varNameString) rootIds
        mergeBench (x,y) = (x,y,lookup x benchAnn)
        allSyn'     = map mergeBench allSyn

    topEntities' <-
      case (topEntities, topSyn) of
        ([], []) ->
          Panic.pgmError $ unwords [ "No 'topEntity', nor function with a"
                                   , "'Synthesize' annotation found in root"
                                   , "module:"
                                   , (Outputable.showSDocUnsafe (ppr rootModule)) ]
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

    return (bindersC ++ makeRecursiveGroups externalBndrs,clsOps,unlocatable,(fst famInstEnvs,modFamInstEnvs'),topEntities',nub $ pFP ++ pFP',reprs++reprs')

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

findCustomReprAnnotations
  :: GHC.GhcMonad m
  => m [DataRepr']
findCustomReprAnnotations = do
  hsc_env <- GHC.getSession
  ann_env <- liftIO $ HscTypes.prepareAnnotations hsc_env Nothing

  let deserializer = GhcPlugins.deserializeWithData :: [Word8] -> DataReprAnn
  let deserialized = Annotations.deserializeAnns deserializer ann_env
  let reprs        = concat $ UniqFM.nonDetEltsUFM deserialized

  return $ map dataReprAnnToDataRepr' reprs

findSynthesizeAnnotations
  :: GHC.GhcMonad m
  => [CoreSyn.CoreBndr]
  -> m [(CoreSyn.CoreBndr,Maybe TopEntity)]
findSynthesizeAnnotations bndrs = do
  let deserializer = GhcPlugins.deserializeWithData :: ([Word8] -> TopEntity)
      targets      = map (Annotations.NamedTarget . Var.varName) bndrs

  anns <- mapM (GHC.findGlobalAnns deserializer) targets
  let isSyn (Synthesize {}) = True
      isSyn _               = False
      anns'    = map (filter isSyn) anns
      annBndrs = filter (not . null . snd) (zip bndrs anns')
  case filter ((> 1) . length . snd) annBndrs of
    [] -> return $ map (second listToMaybe) annBndrs
    as -> Panic.pgmError $
            "The following functions have multiple 'Synthesize' annotations: " ++
            Outputable.showSDocUnsafe (ppr (map fst as))

findTestBenchAnnotations
  :: GHC.GhcMonad m
  => [CoreSyn.CoreBndr]
  -> m [(CoreSyn.CoreBndr,CoreSyn.CoreBndr)]
findTestBenchAnnotations bndrs = do
  let deserializer = GhcPlugins.deserializeWithData :: ([Word8] -> TopEntity)
      targets      = map (Annotations.NamedTarget . Var.varName) bndrs

  anns <- mapM (GHC.findGlobalAnns deserializer) targets
  let isTB (TestBench {}) = True
      isTB _              = False
      anns'     = map (filter isTB) anns
      annBndrs  = filter (not . null . snd) (zip bndrs anns')
      annBndrs' = case filter ((> 1) . length . snd) annBndrs of
        [] -> map (second head) annBndrs
        as -> Panic.pgmError $
          "The following functions have multiple 'TestBench' annotations: " ++
          Outputable.showSDocUnsafe (ppr (map fst as))
  return (map (second findTB) annBndrs')
  where
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

findPrimitiveAnnotations
  :: GHC.GhcMonad m
  => HDL
  -> [CoreSyn.CoreBndr]
  -> m [FilePath]
findPrimitiveAnnotations hdl bndrs = do
  dflags <- GHC.getSessionDynFlags

  let -- Using the stub directory as the output directory for inline primitives
      outDir = fromMaybe "." $ GHC.stubDir dflags
      deserializer = GhcPlugins.deserializeWithData :: ([Word8] -> Primitive)
      targets =
        concatMap
          ( (\v -> catMaybes
            [ Just $ Annotations.NamedTarget v
            , Annotations.ModuleTarget <$> Name.nameModule_maybe v
            ]) . Var.varName
          ) bndrs

  anns <- mapM (GHC.findGlobalAnns deserializer) targets
  sequence $
    mapMaybe (primitiveFilePath hdl outDir)
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
    dflags = wantedOptimizationFlags (ms_hspp_opts
              { DynFlags.optLevel = 2
              , DynFlags.reductionDepth = 1000
              })

wantedOptimizationFlags :: GHC.DynFlags -> GHC.DynFlags
wantedOptimizationFlags df =
  foldl' DynFlags.xopt_unset
    (foldl' DynFlags.gopt_unset
        (foldl' DynFlags.gopt_set df wanted) unwanted) unwantedLang
  where
    wanted = [ Opt_CSE -- CSE
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
    -- fail. As strictness only affects simulation behaviour, removing them
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
-- affect simulation behaviour.
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
