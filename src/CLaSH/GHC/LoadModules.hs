{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
module CLaSH.GHC.LoadModules
  ( loadModules
  )
where

-- External Modules
import qualified GHC.Paths

-- GHC API
-- import qualified CorePrep
import qualified CoreSyn
import qualified DynFlags
import           CLaSH.GHC.Compat.DynFlags (dopt_unset)
import qualified GHC
import           CLaSH.GHC.Compat.GHC (defaultErrorHandler)
import qualified HscTypes
import qualified HsImpExp
import qualified MonadUtils
import           CLaSH.GHC.Compat.Outputable (showPpr)
import qualified Panic
import qualified SrcLoc
import qualified TidyPgm
-- import qualified TyCon
import qualified TysPrim
import qualified TysWiredIn

-- Internal Modules
import           CLaSH.GHC.LoadInterfaceFiles
import           CLaSH.Util (traceIf,curLoc,mapAccumLM,(><))

loadModules ::
  String
  -> IO ( [(CoreSyn.CoreBndr, CoreSyn.CoreExpr)]   -- Binders
        , [(CoreSyn.CoreBndr,[CoreSyn.CoreExpr])]  -- Dictionary Functions
        , [(CoreSyn.CoreBndr,Int)]                 -- Class operations
        , [CoreSyn.CoreBndr]                       -- Unlocatable Expressions
        , [GHC.TyCon]                              -- Type Constructors
        )
loadModules modName = defaultErrorHandler $
  GHC.runGhc (Just GHC.Paths.libdir) $ do
    dflags <- GHC.getSessionDynFlags
    let dflags' = foldl DynFlags.xopt_set
                    (dflags
                      {GHC.simplPhases = 0, DynFlags.ctxtStkDepth = 1000})
                    [DynFlags.Opt_TemplateHaskell,DynFlags.Opt_Arrows]
    _ <- GHC.setSessionDynFlags dflags'
    target <- GHC.guessTarget modName Nothing
    GHC.setTargets [target]
    ldRes <- GHC.load GHC.LoadAllTargets
    case ldRes of
      GHC.Succeeded -> do
        modGraph <- GHC.getModuleGraph
        let externalImports = concatMap ( map ( SrcLoc.unLoc
                                              . HsImpExp.ideclName
                                              . SrcLoc.unLoc
                                              )
                                        . GHC.ms_textual_imps
                                        ) modGraph

        externalTyCons <- fmap snd $
                            mapAccumLM getExternalTyCons [] externalImports

        let allExtTyCons = concat externalTyCons ++
                                  TysWiredIn.wiredInTyCons ++
                                  TysPrim.primTyCons

        let modGraph' = map disableOptimizationsFlags modGraph
        tidiedMods <- mapM (\m -> do { pMod  <- parseModule m
                                     ; tcMod <- GHC.typecheckModule pMod
                                     ; dsMod <- fmap GHC.coreModule $ GHC.desugarModule tcMod
                                     ; hsc_env <- GHC.getSession
                                     ; (tidy_guts,_) <- MonadUtils.liftIO $ TidyPgm.tidyProgram hsc_env dsMod
                                     -- ; dflags'' <- GHC.getSessionDynFlags
                                     ; let tycons     = HscTypes.cg_tycons tidy_guts
                                     -- ; let dataTyCons = filter TyCon.isDataTyCon tycons
                                     ; let pgm        = HscTypes.cg_binds tidy_guts
                                     -- ; prepBinders <- MonadUtils.liftIO $ CorePrep.corePrepPgm dflags'' hsc_env pgm dataTyCons
                                     ; return (CoreSyn.flattenBinds pgm,tycons)
                                     }
                             ) modGraph'

        let (binders,tyCons) = (concat >< concat) (unzip tidiedMods)

        -- let (binders,tyCons) = flattenModules
        --               $ map flattenDesugaredModule desugardMods

        (externalBndrs,dfuns,clsOps,unlocatable) <- loadExternalExprs
                                                (map snd binders)
                                                (map fst binders)

        traceIf (not $ null unlocatable) ("No exprs found for: " ++ showPpr unlocatable) $ return (binders ++ externalBndrs,dfuns,clsOps,unlocatable,tyCons ++ allExtTyCons)
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
    dflags = dopt_unset (ms_hspp_opts
              {DynFlags.optLevel = 0, DynFlags.ctxtStkDepth = 1000})
              DynFlags.Opt_EnableRewriteRules
