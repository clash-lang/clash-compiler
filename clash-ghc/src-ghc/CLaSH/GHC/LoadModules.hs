{-|
  Copyright   :  (C) 2013-2016, University of Twente,
                          2017, Google Inc.
  License     :  BSD2 (see the file LICENSE)
  Maintainer  :  Christiaan Baaij <christiaan.baaij@gmail.com>
-}

{-# LANGUAGE CPP                 #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE ViewPatterns        #-}

module CLaSH.GHC.LoadModules
  ( loadModules
  , ghcLibDir
  )
where

#ifndef TOOL_VERSION_ghc
#error TOOL_VERSION_ghc undefined
#endif

-- External Modules
import           Control.Arrow                (second)
import           Data.Generics.Uniplate.DataOnly (transform)
import           Data.List                    (foldl', lookup, nub)
import           Data.Maybe                   (listToMaybe)
import           Data.Word                    (Word8)
import           CLaSH.Annotations.Primitive  (HDL)
import           CLaSH.Annotations.TopEntity  (TopEntity, TestBench (..))
import           System.Exit                  (ExitCode (..))
import           System.IO                    (hGetLine)
import           System.IO.Error              (tryIOError)
import           System.Process               (runInteractiveCommand,
                                               waitForProcess)

-- GHC API
import qualified Annotations
import qualified CoreSyn
import qualified CoreFVs
import qualified Digraph
import           DynFlags                     (GeneralFlag (..))
import qualified DynFlags
import qualified GHC
import qualified HscMain
import qualified HscTypes
import qualified MonadUtils
import qualified Panic
import qualified Serialized
import qualified TidyPgm
import qualified TcRnMonad
import qualified TcRnTypes
import qualified Unique
#if MIN_VERSION_ghc(8,2,0)
import qualified UniqDFM
#else
import qualified UniqFM
#endif
import qualified UniqSet
import qualified Var
import qualified FamInst
import qualified FamInstEnv
import qualified Name
import           Outputable                   (ppr)
import qualified Outputable
import qualified OccName
import qualified GHC.LanguageExtensions       as LangExt

-- Internal Modules
import           CLaSH.GHC.GHC2Core           (modNameM)
import           CLaSH.GHC.LoadInterfaceFiles
import           CLaSH.Util                   (curLoc)

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
    noGHC = "CLaSH needs the GHC compiler it was built with, ghc-" ++ TOOL_VERSION_ghc ++
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
        )
loadModules hdl modName dflagsM = do
  libDir <- MonadUtils.liftIO ghcLibDir

  GHC.runGhc (Just libDir) $ do
    dflags <- case dflagsM of
                Just df -> return df
                Nothing -> do
                  df <- GHC.getSessionDynFlags
                  let dfEn = foldl DynFlags.xopt_set df
                                [ LangExt.TemplateHaskell
                                , LangExt.TemplateHaskellQuotes
                                , LangExt.DataKinds
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
#if __GLASGOW_HASKELL__ >= 711
                    { DynFlags.reductionDepth = 1000
#else
                    { DynFlags.ctxtStkDepth = 1000
#endif
                    , DynFlags.optLevel = 2
                    , DynFlags.ghcMode  = GHC.CompManager
                    , DynFlags.ghcLink  = GHC.LinkInMemory
                    , DynFlags.hscTarget = DynFlags.defaultObjectTarget
                                             (DynFlags.targetPlatform dflags)
                    }
    let dflags2 = wantedOptimizationFlags dflags1
    let ghcDynamic = case lookup "GHC Dynamic" (DynFlags.compilerInfo dflags) of
                      Just "YES" -> True
                      _          -> False
    let dflags3 = if ghcDynamic then DynFlags.gopt_set dflags2 DynFlags.Opt_BuildDynamicToo
                                else dflags2
    _ <- GHC.setSessionDynFlags dflags3
    target <- GHC.guessTarget modName Nothing
    GHC.setTargets [target]
    modGraph <- GHC.depanal [] False
    let modGraph' = map disableOptimizationsFlags modGraph
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
                                 ; simpl_guts <- MonadUtils.liftIO $ HscMain.hscSimplify hsc_env dsMod
                                 ; (tidy_guts,_) <- MonadUtils.liftIO $ TidyPgm.tidyProgram hsc_env simpl_guts
                                 ; let pgm        = HscTypes.cg_binds tidy_guts
                                 ; let modFamInstEnv = TcRnTypes.tcg_fam_inst_env $ fst $ GHC.tm_internals_ tcMod
                                 ; return (pgm,modFamInstEnv)
                                 }
                         ) modGraph2

    let (binders,modFamInstEnvs) = unzip tidiedMods
        bindersC                 = concat binders
        binderIds                = map fst (CoreSyn.flattenBinds bindersC)
#if MIN_VERSION_ghc(8,2,0)
        modFamInstEnvs'          = foldr UniqDFM.plusUDFM UniqDFM.emptyUDFM modFamInstEnvs
#else
        modFamInstEnvs'          = foldr UniqFM.plusUFM UniqFM.emptyUFM modFamInstEnvs
#endif

    (externalBndrs,clsOps,unlocatable,pFP) <-
      loadExternalExprs hdl (UniqSet.mkUniqSet binderIds) bindersC

    hscEnv <- GHC.getSession
    famInstEnvs <- TcRnMonad.liftIO $ TcRnMonad.initTcForLookup hscEnv FamInst.tcGetFamInstEnvs

    -- Because tidiedMods is in topological order, binders is also, and hence
    -- the binders belonging to the "root" module are the last binders
    let rootModule = GHC.ms_mod_name . last $ modGraph2
        rootIds    = map fst . CoreSyn.flattenBinds $ last binders

    -- Because tidiedMods is in topological order, binders is also, and hence
    -- allAnn is in topological order. This means that the "root" 'topEntity'
    -- will be compiled last.
    allAnn   <- findTopEntityAnnotations binderIds
    benchAnn <- findTestBenchAnnotations binderIds
    topAnn   <- findTopEntityAnnotations rootIds
    let varNameString = OccName.occNameString . Name.nameOccName . Var.varName
        topEntities = filter ((== "topEntity") . varNameString) rootIds
        benches     = filter ((== "testBench") . varNameString) rootIds
        mergeBench (x,y) = (x,y,lookup x benchAnn)
        allAnn'     = map mergeBench allAnn
    topEntities' <- case topEntities of
      [] -> case topAnn of
        [] -> Panic.pgmError $ "No 'topEntity', nor function with a 'TopEntity' annotation found in root module: " ++
                                (Outputable.showSDocUnsafe (ppr rootModule))
        _ -> return allAnn'
      [x] -> case lookup x topAnn of
        Nothing -> case lookup x benchAnn of
          Nothing -> return ((x,Nothing,listToMaybe benches):allAnn')
          Just y  -> return ((x,Nothing,Just y):allAnn')
        Just _  -> return allAnn'
      _ -> Panic.pgmError $ $(curLoc) ++ "Multiple 'topEntities' found."

    return (bindersC ++ makeRecursiveGroups externalBndrs,clsOps,unlocatable,(fst famInstEnvs,modFamInstEnvs'),topEntities',nub pFP)

-- | Given a set of bindings, make explicit non-recursive bindings and
-- recursive binding groups.
--
-- Needed because:
-- 1. GHC does not preserve this information in interface files,
-- 2. Binders in CLaSH's BindingsMap are not allowed to be mutually recursive,
--    only self-recursive.
-- 3. CLaSH.GHC.GenerateBindings.mkBindings turns groups of mutually recursive
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
    makeNode (b,e) = ((b,e)
                     ,Var.varUnique b
                     ,UniqSet.nonDetKeysUniqSet (CoreFVs.exprFreeIds e))

    makeBind
      :: Digraph.SCC (CoreSyn.CoreBndr,CoreSyn.CoreExpr)
      -> CoreSyn.CoreBind
    makeBind (Digraph.AcyclicSCC (b,e)) = CoreSyn.NonRec b e
    makeBind (Digraph.CyclicSCC bs)     = CoreSyn.Rec bs

findTopEntityAnnotations
  :: GHC.GhcMonad m
  => [CoreSyn.CoreBndr]
  -> m [(CoreSyn.CoreBndr,Maybe TopEntity)]
findTopEntityAnnotations bndrs = do
  let deserializer = Serialized.deserializeWithData :: ([Word8] -> TopEntity)
      targets      = map (Annotations.NamedTarget . Var.varName) bndrs

  anns <- mapM (GHC.findGlobalAnns deserializer) targets
  let annBndrs = filter (not . null . snd) (zip bndrs anns)
  case filter ((> 1) . length . snd) annBndrs of
    [] -> return (map (second listToMaybe) annBndrs)
    as -> Panic.pgmError $
            "The following functions have multiple 'TopEntity' annotations: " ++
            Outputable.showSDocUnsafe (ppr (map fst as))

findTestBenchAnnotations
  :: GHC.GhcMonad m
  => [CoreSyn.CoreBndr]
  -> m [(CoreSyn.CoreBndr,CoreSyn.CoreBndr)]
findTestBenchAnnotations bndrs = do
  let deserializer = Serialized.deserializeWithData :: ([Word8] -> TestBench)
      targets      = map (Annotations.NamedTarget . Var.varName) bndrs

  anns <- mapM (GHC.findGlobalAnns deserializer) targets
  let annBndrs  = filter (not . null . snd) (zip bndrs anns)
      annBndrs' = case filter ((> 1) . length . snd) annBndrs of
        [] -> map (second head) annBndrs
        as -> Panic.pgmError $
          "The following functions have multiple 'TestBench' annotations: " ++
          Outputable.showSDocUnsafe (ppr (map fst as))
  return (map (second findTB) annBndrs')
  where
    findTB :: TestBench -> CoreSyn.CoreBndr
    findTB (TestBench tb) = case listToMaybe (filter (eqNm tb) bndrs) of
      Just tb' -> tb'
      Nothing  -> Panic.pgmError $
        "TestBench named: " ++ show tb ++ " not found"

    eqNm thNm bndr = show thNm == qualNm
      where
        bndrNm  = Var.varName bndr
        qualNm  = maybe occName (\modName -> modName ++ ('.':occName)) (modNameM bndrNm)
        occName = OccName.occNameString (Name.nameOccName bndrNm)

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
#if __GLASGOW_HASKELL__ >= 711
              , DynFlags.reductionDepth = 1000
#else
              , DynFlags.ctxtStkDepth = 1000
#endif
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
             , Opt_Strictness -- Strictness analysis helps with dead-code analysis. However, see [NOTE: CPR breaks CLaSH]
             , Opt_SpecialiseAggressively -- Needed to compile Fixed point number functions quickly
             , Opt_CrossModuleSpecialise -- Needed to compile Fixed point number functions quickly
             ]

    unwanted = [ Opt_LiberateCase -- Perform unrolling of recursive RHS: avoid
               , Opt_SpecConstr -- Creates local-functions: avoid
               , Opt_IgnoreAsserts -- We don't care about assertions
               , Opt_DoEtaReduction -- We want eta-expansion
               , Opt_UnboxStrictFields -- Unboxed types are not handled properly: avoid
               , Opt_UnboxSmallStrictFields -- Unboxed types are not handled properly: avoid
               , Opt_Vectorise -- Don't care
               , Opt_VectorisationAvoidance -- Don't care
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
               , Opt_CprAnal -- The worker/wrapper introduced by CPR breaks CLaSH, see [NOTE: CPR breaks CLaSH]
               , Opt_FullLaziness -- increases sharing, but seems to result in worse circuits (in both area and propagation delay)
               ]

    -- Coercions between Integer and CLaSH' numeric primitives cause CLaSH to
    -- fail. As strictness only affects simulation behaviour, removing them
    -- is perfectly safe.
    unwantedLang = [ LangExt.Strict
                   , LangExt.StrictData
                   ]

-- [NOTE: CPR breaks CLaSH]
-- We used to completely disable strictness analysis because it causes GHC to
-- do the so-called "Constructed Product Result" (CPR) analysis, which in turn
-- creates an annoying worker/wrapper which does the following:
--
--   * Scrutinise a Signal, and pack the head and tail of the
--     Signal in an unboxed tuple.
--   * Scrutinise on the unboxed tuple, and recreate the Signal.
--
-- This is problematic because the 'Signal' type is essentially treated as a "transparent"
-- type by the CLaSH compiler, so observing its constructor leads to all kinds
-- of problems.
--
-- The current solution is to disable strictness analysis in "CLaSH.Signal.Internal"
-- so that functions manipulating 'Signal' constructor do not get a strictness/
-- demand/CPR annotation, which in turn ensures GHC doesn't create worker/wrappers
-- for when these functions are called in user code.
--
-- Ultimately we should stop treating Signal as a "transparent" type and deal
-- handling of the Signal type, and the involved co-recursive functions,
-- properly. At the moment, CLaSH cannot deal with this recursive type and the
-- recursive functions involved, and hence we need to disable this useful transformation. After
-- everything is done properly, we should enable it again.

-- | Remove all strictness annotations:
--
-- * Remove strictness annotations from data type declarations
--   (only works for data types that are currently being compiled, i.e.,
--    that are not part of a pre-compiled imported library)
--
-- We need to remove strictness annotations because GHC will introduce casts
-- between Integer and CLaSH' numeric primitives otherwise, where CLaSH will
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
    rmPS :: GHC.DataId name => GHC.HsModule name -> GHC.HsModule name
    rmPS hsm = hsm {GHC.hsmodDecls = (fmap . fmap) rmHSD (GHC.hsmodDecls hsm)}

    rmHSD :: GHC.DataId name => GHC.HsDecl name -> GHC.HsDecl name
    rmHSD (GHC.TyClD tyClDecl) = GHC.TyClD (rmTyClD tyClDecl)
    rmHSD hsd = hsd

    rmTyClD :: GHC.DataId name => GHC.TyClDecl name -> GHC.TyClDecl name
    rmTyClD dc@(GHC.DataDecl {}) = dc {GHC.tcdDataDefn = rmDataDefn (GHC.tcdDataDefn dc)}
    rmTyClD tyClD = tyClD

    rmDataDefn :: GHC.DataId name => GHC.HsDataDefn name -> GHC.HsDataDefn name
    rmDataDefn hdf = hdf {GHC.dd_cons = (fmap . fmap) rmCD (GHC.dd_cons hdf)}

    rmCD :: GHC.DataId name => GHC.ConDecl name -> GHC.ConDecl name
    rmCD gadt@(GHC.ConDeclGADT {}) = gadt {GHC.con_type = rmSigType (GHC.con_type gadt)}
    rmCD h98@(GHC.ConDeclH98 {})   = h98  {GHC.con_details = rmConDetails (GHC.con_details h98)}

    -- type LHsSigType name = HsImplicitBndrs name (LHsType name)
    rmSigType :: GHC.DataId name => GHC.LHsSigType name -> GHC.LHsSigType name
    rmSigType hsIB = hsIB {GHC.hsib_body = rmHsType (GHC.hsib_body hsIB)}

    -- type HsConDeclDetails name = HsConDetails (LBangType name) (Located [LConDeclField name])
    rmConDetails :: GHC.DataId name => GHC.HsConDeclDetails name -> GHC.HsConDeclDetails name
    rmConDetails (GHC.PrefixCon args) = GHC.PrefixCon (fmap rmHsType args)
    rmConDetails (GHC.RecCon rec)     = GHC.RecCon ((fmap . fmap . fmap) rmConDeclF rec)
    rmConDetails (GHC.InfixCon l r)   = GHC.InfixCon (rmHsType l) (rmHsType r)

    rmHsType :: GHC.DataId name => GHC.Located (GHC.HsType name) -> GHC.Located (GHC.HsType name)
    rmHsType = transform go
      where
        go (GHC.unLoc -> GHC.HsBangTy _ ty) = ty
        go ty = ty

    rmConDeclF :: GHC.DataId name => GHC.ConDeclField name -> GHC.ConDeclField name
    rmConDeclF cdf = cdf {GHC.cd_fld_type = rmHsType (GHC.cd_fld_type cdf)}
