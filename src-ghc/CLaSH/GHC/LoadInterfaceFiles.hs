{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TupleSections       #-}
module CLaSH.GHC.LoadInterfaceFiles where

-- External Modules
import           Data.Either                 (partitionEithers)
import           Data.List                   (elemIndex, partition)
import           Data.Maybe                  (isJust, isNothing, mapMaybe)

-- GHC API
import qualified BasicTypes
import           CLaSH.GHC.Compat.Outputable (showPpr, showSDoc)
import qualified Class
import qualified CoreFVs
import qualified CoreSyn
import qualified Exception
import qualified FamInstEnv
import qualified GHC
import qualified HscTypes
import qualified Id
import qualified IdInfo
import qualified IfaceSyn
import qualified LoadIface
import qualified Maybes
import qualified MkCore
import qualified MonadUtils
import qualified Name
import           Outputable                  (text)
import qualified TcIface
import qualified TcRnMonad
import qualified TcRnTypes
import qualified UniqFM
import qualified Var
import qualified VarSet

-- Internal Modules
import           CLaSH.Util                  (curLoc, mapAccumLM, traceIf)

getExternalTyCons ::
  GHC.GhcMonad m
  => [GHC.ModuleName]
  -> GHC.ModuleName
  -> m ([GHC.ModuleName],[GHC.TyCon])
getExternalTyCons visited modName = (`Exception.gcatch` expCatch) $ do
  foundMod   <- GHC.findModule modName Nothing
  (tcs,used) <- runIfl foundMod $ do
                  ifaceM <- loadIface foundMod
                  case ifaceM of
                    Nothing -> return ([],[])
                    Just iface -> do
                      let used  = mapMaybe usageModuleName $ GHC.mi_usages iface
                      tcs <- ifaceTyCons iface
                      return (tcs,used)

  let visited' = modName:visited
  let used'    = filter (`notElem` visited') used
  (visited'',tcs') <- mapAccumLM getExternalTyCons (visited' ++ used')
                       used'
  return (visited'',tcs ++ concat tcs')
  where
    expCatch :: GHC.GhcMonad m
      => HscTypes.SourceError -> m ([GHC.ModuleName],[GHC.TyCon])
    expCatch _ = return (modName:visited,[])

    usageModuleName :: HscTypes.Usage -> Maybe GHC.ModuleName
    usageModuleName (HscTypes.UsagePackageModule {..}) = Just $ GHC.moduleName usg_mod
    usageModuleName (HscTypes.UsageHomeModule {..})    = Just usg_mod_name
    usageModuleName _                                  = Nothing

runIfl :: GHC.GhcMonad m => GHC.Module -> TcRnTypes.IfL a -> m a
runIfl modName action = do
  hscEnv <- GHC.getSession
  let localEnv = TcRnTypes.IfLclEnv modName (text "runIfl")
                   UniqFM.emptyUFM UniqFM.emptyUFM
  let globalEnv = TcRnTypes.IfGblEnv Nothing
  MonadUtils.liftIO $ TcRnMonad.initTcRnIf 'r' hscEnv globalEnv
                        localEnv action

loadDecl :: IfaceSyn.IfaceDecl -> TcRnTypes.IfL GHC.TyThing
loadDecl = TcIface.tcIfaceDecl False

ifaceTyCons :: HscTypes.ModIface -> TcRnTypes.IfL [GHC.TyCon]
ifaceTyCons = fmap (\md -> (HscTypes.typeEnvTyCons . HscTypes.md_types) md ++
                           (FamInstEnv.famInstsRepTyCons . HscTypes.md_fam_insts) md
                   ) . TcIface.typecheckIface

loadIface :: GHC.Module -> TcRnTypes.IfL (Maybe GHC.ModIface)
loadIface foundMod = do
  ifaceFailM <- LoadIface.findAndReadIface (Outputable.text "loadIface") foundMod False
  case ifaceFailM of
    Maybes.Succeeded (modInfo,_) -> return (Just modInfo)
    Maybes.Failed msg -> traceIf True ($(curLoc) ++ "Failed to load interface for module: " ++ showPpr foundMod ++ "\nReason: " ++ showSDoc msg) $ return Nothing

loadExternalExprs ::
  GHC.GhcMonad m
  => [CoreSyn.CoreExpr]
  -> [CoreSyn.CoreBndr]
  -> m ( [(CoreSyn.CoreBndr,CoreSyn.CoreExpr)] -- Binders
       , [(CoreSyn.CoreBndr,Int)]              -- Class Ops
       , [CoreSyn.CoreBndr]                    -- Unlocatable
       )
loadExternalExprs []           _       = return ([],[],[])
loadExternalExprs (expr:exprs) visited = do
  let fvs = VarSet.varSetElems $ CoreFVs.exprSomeFreeVars
              (\v -> Var.isId v &&
                     isNothing (Id.isDataConId_maybe v) &&
                     v `notElem` visited
              ) expr

  let (clsOps,fvs') = partition (isJust . Id.isClassOpId_maybe) fvs

  (locatedExprs,unlocated) <- fmap partitionEithers
                                $ mapM loadExprFromIface fvs'

  let visited' = concat [ map fst locatedExprs
                        , unlocated
                        , clsOps
                        , visited
                        ]

  (locatedExprs', clsOps', unlocated') <-
    loadExternalExprs
      (exprs ++ map snd locatedExprs)
      visited'

  let clsOps'' = map
       ( \v -> flip (maybe (error $ $(curLoc) ++ "Not a class op")) (Id.isClassOpId_maybe v) $ \c ->
           let clsIds = Class.classAllSelIds c
           in  maybe (error $ $(curLoc) ++ "Index not found")
                     (v,)
                     (elemIndex v clsIds)
       ) clsOps

  return ( locatedExprs ++ locatedExprs'
         , clsOps''     ++ clsOps'
         , unlocated    ++ unlocated'
         )

loadExprFromIface ::
  GHC.GhcMonad m
  => CoreSyn.CoreBndr
  -> m (Either
          (CoreSyn.CoreBndr,CoreSyn.CoreExpr)
          CoreSyn.CoreBndr
       )
loadExprFromIface bndr = do
  let moduleM = Name.nameModule_maybe $ Var.varName bndr
  case moduleM of
    Just nameMod -> runIfl nameMod $ do
      ifaceM <- loadIface nameMod
      case ifaceM of
        Nothing    -> return (Right bndr)
        Just iface -> do
          let decls = map snd (GHC.mi_decls iface)
          let nameFun = GHC.getOccName $ Var.varName bndr
          let declM = filter ((== nameFun) . IfaceSyn.ifName) decls
          case declM of
            [namedDecl] -> do
              tyThing <- loadDecl namedDecl
              return $ loadExprFromTyThing bndr tyThing
            _ -> return (Right bndr)
    Nothing -> return (Right bndr)

loadExprFromTyThing :: CoreSyn.CoreBndr
                    -> GHC.TyThing
                    -> Either
                         (CoreSyn.CoreBndr,CoreSyn.CoreExpr)  -- Located Binder
                         CoreSyn.CoreBndr                     -- unlocatable Var
loadExprFromTyThing bndr tyThing = case tyThing of
  GHC.AnId _id | Var.isId _id ->
    let unfolding  = IdInfo.unfoldingInfo $ Var.idInfo _id
        inlineInfo = IdInfo.inlinePragInfo $ Var.idInfo _id
    in case unfolding of
      (CoreSyn.CoreUnfolding {}) ->
        case BasicTypes.inl_inline inlineInfo of
          BasicTypes.NoInline -> Right bndr
          _ -> Left (bndr, CoreSyn.unfoldingTemplate unfolding)
      (CoreSyn.DFunUnfolding dfbndrs dc es) ->
        let dcApp  = MkCore.mkCoreConApps dc es
            dfExpr = MkCore.mkCoreLams dfbndrs dcApp
        in Left (bndr,dfExpr)
      _ -> Right bndr
  _ -> Right bndr
