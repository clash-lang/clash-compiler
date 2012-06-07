{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
module CLaSH.GHC.LoadModules
  ( loadModules
  )
where

-- External Modules
import qualified GHC.Paths

-- GHC API
import qualified CoreSyn
import qualified DynFlags
import qualified GHC
import qualified HscTypes
import qualified HsImpExp
import qualified Panic
import qualified SrcLoc
import qualified TcRnTypes
import qualified TysPrim
import qualified TysWiredIn

-- Internal Modules
import           CLaSH.GHC.LoadInterfaceFiles
import           CLaSH.Util (curLoc,mapAccumLM,second)

loadModules ::
  String
  -> IO ([(CoreSyn.CoreBndr, CoreSyn.CoreExpr)],[GHC.TyCon])
loadModules modName = GHC.defaultErrorHandler DynFlags.defaultLogAction $
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
        desugardMods <- mapM (\m -> parseModule m >>=
                              GHC.typecheckModule >>=
                              GHC.desugarModule) modGraph'
        let flattened = flattenModules
                      $ map flattenDesugaredModule desugardMods
        return (second (++ allExtTyCons) flattened)
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
    dflags = DynFlags.dopt_unset (ms_hspp_opts
              {DynFlags.optLevel = 0, DynFlags.ctxtStkDepth = 1000})
              DynFlags.Opt_EnableRewriteRules

flattenDesugaredModule ::
  GHC.DesugaredModule
  -> ([(CoreSyn.CoreBndr, CoreSyn.CoreExpr)],[GHC.TyCon])
flattenDesugaredModule desugardMod =
  (CoreSyn.flattenBinds . HscTypes.mg_binds $ GHC.coreModule desugardMod
  ,TcRnTypes.tcg_tcs . fst . GHC.tm_internals_ $
    GHC.dm_typechecked_module desugardMod
  )

flattenModules ::
  [([(CoreSyn.CoreBndr, CoreSyn.CoreExpr)],[GHC.TyCon])]
  -> ([(CoreSyn.CoreBndr, CoreSyn.CoreExpr)],[GHC.TyCon])
flattenModules mods = (concat bndrss, concat tcss)
  where
    (bndrss,tcss) = unzip mods
