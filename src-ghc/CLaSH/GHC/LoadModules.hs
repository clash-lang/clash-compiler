{-# LANGUAGE CPP                 #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
module CLaSH.GHC.LoadModules
  ( loadModules
  )
where

-- External Modules
#ifdef STANDALONE
import           System.Exit                  (ExitCode (..))
import           System.IO                    (hGetLine)
import           System.Process               (runInteractiveCommand,
                                               waitForProcess)
#else
import qualified GHC.Paths
#endif

-- GHC API
-- import qualified CorePrep
import           CLaSH.GHC.Compat.DynFlags    (dopt_set, dopt_unset)
import           CLaSH.GHC.Compat.GHC         (defaultErrorHandler)
import qualified CoreSyn
import           DynFlags                     (GeneralFlag (..))
import qualified DynFlags
import qualified GHC
-- import qualified HscMain
import qualified HscTypes
import qualified MonadUtils
import qualified Panic
import qualified TidyPgm
-- import qualified TyCons

-- Internal Modules
import           CLaSH.GHC.LoadInterfaceFiles
import           CLaSH.Util                   (curLoc)

#ifdef STANDALONE
ghcLibDir :: IO FilePath
ghcLibDir = do (libDir,exitCode) <- getProcessOutput "ghc --print-libdir"
               case exitCode of
                  ExitSuccess   -> return libDir
                  ExitFailure i -> error $ "Calling GHC failed with: " ++ show i

getProcessOutput :: String -> IO (String, ExitCode)
getProcessOutput command =
     -- Create the process
  do (_, pOut, _, handle) <- runInteractiveCommand command
     -- Wait for the process to finish and store its exit code
     exitCode <- waitForProcess handle
     -- Get the standard output.
     output   <- hGetLine pOut
     -- return both the output and the exit code.
     return (output, exitCode)
#else
ghcLibDir :: IO FilePath
ghcLibDir = return GHC.Paths.libdir
#endif

loadModules ::
  String
  -> IO ( [(CoreSyn.CoreBndr, CoreSyn.CoreExpr)]   -- Binders
        , [(CoreSyn.CoreBndr,Int)]                 -- Class operations
        , [CoreSyn.CoreBndr]                       -- Unlocatable Expressions
        )
loadModules modName = defaultErrorHandler $ do
  libDir <- MonadUtils.liftIO ghcLibDir

  GHC.runGhc (Just libDir) $ do
    dflags <- GHC.getSessionDynFlags
    let ghcDynamic = case lookup "GHC Dynamic" (DynFlags.compilerInfo dflags) of
                      Just "YES" -> True
                      _          -> False
    let dflags1 = foldl DynFlags.xopt_set
                    (dflags
                      { DynFlags.ctxtStkDepth = 1000
                      , DynFlags.optLevel = 2
                      , DynFlags.ghcMode  = GHC.CompManager
                      } )
                    [ DynFlags.Opt_TemplateHaskell
                    , DynFlags.Opt_Arrows
                    , DynFlags.Opt_DataKinds
                    , DynFlags.Opt_TypeOperators
                    , DynFlags.Opt_FlexibleContexts
                    , DynFlags.Opt_ConstraintKinds
                    , DynFlags.Opt_TypeFamilies
                    , DynFlags.Opt_NegativeLiterals
                    ]
    let dflags2 = wantedOptimizationFlags dflags1
    let dflags3 = if ghcDynamic then DynFlags.gopt_set dflags2 DynFlags.Opt_BuildDynamicToo
                                else dflags2
    _ <- GHC.setSessionDynFlags dflags3
    target <- GHC.guessTarget modName Nothing
    GHC.setTargets [target]
    ldRes <- GHC.load GHC.LoadAllTargets
    case ldRes of
      GHC.Succeeded -> do
        modGraph <- GHC.getModuleGraph
        let modGraph' = map disableOptimizationsFlags modGraph
        tidiedMods <- mapM (\m -> do { pMod  <- parseModule m
                                     ; tcMod <- GHC.typecheckModule pMod
                                     ; dsMod <- fmap GHC.coreModule $ GHC.desugarModule tcMod
                                     ; hsc_env <- GHC.getSession
                                     -- ; simpl_guts <- MonadUtils.liftIO $ HscMain.hscSimplify hsc_env dsMod
                                     ; (tidy_guts,_) <- MonadUtils.liftIO $ TidyPgm.tidyProgram hsc_env dsMod
                                     -- ; let tycons     = HscTypes.cg_tycons tidy_guts
                                     ; let pgm        = HscTypes.cg_binds tidy_guts
                                     -- ; let pgm = HscTypes.mg_binds dsMod
                                     -- ; let dataTyCons = filter TyCon.isDataTyCon tycons
                                     -- ; dflags'' <- GHC.getSessionDynFlags
                                     -- ; prepBinders <- MonadUtils.liftIO $ CorePrep.corePrepPgm dflags'' hsc_env pgm dataTyCons
                                     ; return (CoreSyn.flattenBinds pgm)
                                     }
                             ) modGraph'

        let binders = concat tidiedMods

        (externalBndrs,clsOps,unlocatable) <- loadExternalExprs
                                                (map snd binders)
                                                (map fst binders)

        return (binders ++ externalBndrs,clsOps,unlocatable)
      GHC.Failed -> Panic.pgmError $ $(curLoc) ++ "failed to load module: " ++ modName

parseModule :: GHC.GhcMonad m => GHC.ModSummary -> m GHC.ParsedModule
parseModule modSum = do
  (GHC.ParsedModule pmModSum pmParsedSource extraSrc) <-
    GHC.parseModule modSum
  return (GHC.ParsedModule
            (disableOptimizationsFlags pmModSum)
            pmParsedSource extraSrc)

disableOptimizationsFlags :: GHC.ModSummary -> GHC.ModSummary
disableOptimizationsFlags ms@(GHC.ModSummary {..})
  = ms {GHC.ms_hspp_opts = dflags}
  where
    dflags = wantedOptimizationFlags (ms_hspp_opts
              {DynFlags.optLevel = 2, DynFlags.ctxtStkDepth = 1000})

wantedOptimizationFlags :: GHC.DynFlags -> GHC.DynFlags
wantedOptimizationFlags df = foldl dopt_unset (foldl dopt_set df wanted) unwanted
  where
    wanted = [ Opt_Strictness -- [Wanted?] don't care about strictness
             , Opt_CSE -- CSE
             , Opt_FullLaziness -- Floats let-bindings outside enclosing lambdas
             , Opt_Specialise -- Specialise on types, specialise type-class-overloaded function defined in this module for the types
             , Opt_DoLambdaEtaExpansion -- We need eta-expansion anyway, so the more GHC does, the better
             , Opt_CaseMerge -- We want fewer case-statements
             , Opt_DictsCheap -- Makes dictionaries seem cheap to optimizer: hopefully inline
             , Opt_SimpleListLiterals -- Avoids 'build' rule
             , Opt_ExposeAllUnfoldings -- We need all the unfoldings we can get
             , Opt_ForceRecomp -- Force recompilation: never bad
             ]

    unwanted = [ Opt_FloatIn -- Moves let-bindings inwards: defeats the normal-form with a single top-level let-binding
               , Opt_StaticArgumentTransformation -- [Wanted?] Turn on the static argument transformation, which turns a recursive function into a non-recursive one with a local recursive loop.
               , Opt_LiberateCase -- Perform unrolling of recursive RHS: avoid
               , Opt_SpecConstr -- Creates local-functions: avoid
               , Opt_IgnoreAsserts -- We don't care about assertions
               , Opt_DoEtaReduction -- We want eta-expansion
               , Opt_UnboxStrictFields -- Unboxed types are not handled properly: avoid
               , Opt_UnboxSmallStrictFields -- Unboxed types are not handled properly: avoid
               , Opt_EnableRewriteRules -- Intermediate data-structures take up no space
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
               , Opt_SimplPreInlining -- Does inlining, which destroys function hierarchy: avoid
               , Opt_IrrefutableTuples -- Introduce irrefuntPatError: avoid
               ]
