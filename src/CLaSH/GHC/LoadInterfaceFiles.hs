{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
module CLaSH.GHC.LoadInterfaceFiles where

-- External Modules
import           Data.Either (partitionEithers)
import           Data.List   (elemIndex,partition)
import           Data.Maybe  (fromMaybe,isJust,mapMaybe)

-- GHC API
import qualified Class
import qualified CoreSyn
import qualified CoreFVs
import qualified Exception
import qualified GHC
import qualified HscTypes
import qualified Id
import qualified IdInfo
import qualified IfaceSyn
import qualified LoadIface
import qualified Maybes
import qualified MonadUtils
import           Outputable (showPpr,text)
import qualified TcIface
import qualified TcRnMonad
import qualified TcRnTypes
import qualified UniqFM
import qualified Var
import qualified VarSet

-- Internal Modules
import           CLaSH.Util (curLoc,mapAccumLM,traceIf)

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
                let used  = map fst $ HscTypes.dep_mods $ GHC.mi_deps iface
                let decls = map snd (GHC.mi_decls iface)
                tcs <- fmap (mapMaybe tyThingIsTyCon) $ mapM loadDecl decls
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

    tyThingIsTyCon :: GHC.TyThing -> Maybe GHC.TyCon
    tyThingIsTyCon (GHC.ATyCon tc) = Just tc
    tyThingIsTyCon _               = Nothing

runIfl :: GHC.GhcMonad m => GHC.Module -> TcRnTypes.IfL a -> m a
runIfl modName action = do
  hscEnv <- GHC.getSession
  let localEnv = TcRnTypes.IfLclEnv modName (text "runIfl")
                   UniqFM.emptyUFM UniqFM.emptyUFM
  let globalEnv = TcRnTypes.IfGblEnv Nothing
  MonadUtils.liftIO $ TcRnMonad.initTcRnIf 'r' hscEnv globalEnv
                        localEnv action

loadDecl :: IfaceSyn.IfaceDecl -> TcRnTypes.IfL GHC.TyThing
loadDecl decl = TcIface.tcIfaceDecl False decl

loadIface :: GHC.Module -> TcRnTypes.IfL (Maybe GHC.ModIface)
loadIface foundMod = do
  ifaceFailM <- LoadIface.findAndReadIface (Outputable.text "loadIface") foundMod False
  case ifaceFailM of
    Maybes.Succeeded (modInfo,_) -> return (Just modInfo)
    Maybes.Failed _ -> traceIf True ("failed to load interface for module: " ++ showPpr foundMod) $ return Nothing

loadExternalExprs ::
  GHC.GhcMonad m
  => [CoreSyn.CoreExpr]
  -> [CoreSyn.CoreBndr]
  -> m ( [(CoreSyn.CoreBndr,CoreSyn.CoreExpr)]    -- Binders
       , [(CoreSyn.CoreBndr,[CoreSyn.CoreExpr])]  -- Dictionary functions
       , [(CoreSyn.CoreBndr,Int)]                 -- Class Ops
       , [CoreSyn.CoreBndr]                       -- Unlocatable
       )
loadExternalExprs [] _ = return ([],[],[],[])
loadExternalExprs (expr:exprs) visited = do
  let fvs = VarSet.varSetElems $ CoreFVs.exprSomeFreeVars
              (\v -> Var.isId v &&
                     v `notElem` visited
              ) expr

  let (clsOps,fvs') = partition (isJust . Id.isClassOpId_maybe) fvs

  (located,unlocated) <- fmap partitionEithers
                       $ mapM loadExprFromIface fvs'

  let (locatedDFuns,locatedExprs) = partitionEithers located
  let visited' = map fst locatedExprs ++ map fst locatedDFuns
                  ++ unlocated ++ clsOps ++ visited

  (locatedExprs', locatedDFuns', clsOps', unlocated') <-
    loadExternalExprs
      ( exprs ++
        map snd locatedExprs ++
        concatMap snd locatedDFuns
      ) visited'

  let clsOps'' = map ( \v ->
                          (v,)
                          . fromMaybe (error $ $(curLoc) ++ "Index not found")
                          . elemIndex v
                          . Class.classAllSelIds
                          . fromMaybe (error $ $(curLoc) ++ "Not a class op")
                          $ Id.isClassOpId_maybe v
                     ) clsOps

  return ( locatedExprs ++ locatedExprs'
         , locatedDFuns ++ locatedDFuns'
         , clsOps''     ++ clsOps'
         , unlocated    ++ unlocated'
         )

loadExprFromIface ::
  GHC.GhcMonad m
  => CoreSyn.CoreBndr
  -> m (Either
          (Either
            (CoreSyn.CoreBndr,[CoreSyn.CoreExpr])
            (CoreSyn.CoreBndr,CoreSyn.CoreExpr))
          CoreSyn.CoreBndr)
loadExprFromIface bndr = do
    nameModM <- GHC.gcatch
                  ( fmap Just
                  $ GHC.findModule
                      (GHC.moduleName . GHC.nameModule $ Var.varName bndr)
                      Nothing
                  ) (\(_::Exception.SomeException) -> return Nothing)

    case nameModM of
      Just nameMod -> runIfl nameMod $ do
        ifaceM <- loadIface nameMod
        case ifaceM of
          Nothing    -> return $ Right bndr
          Just iface -> do
            let decls = map snd (GHC.mi_decls iface)
            let nameFun = GHC.getOccName $ Var.varName bndr
            let declM = filter ((== nameFun) . IfaceSyn.ifName) decls
            case declM of
              [namedDecl] -> do
                tyThing <- loadDecl namedDecl
                return $ loadExprFromTyThing bndr tyThing
              _ -> return $ Right bndr
      Nothing -> return $ Right bndr

loadExprFromTyThing ::
  CoreSyn.CoreBndr
  -> GHC.TyThing
  -> Either
      (Either
        (CoreSyn.CoreBndr,[CoreSyn.CoreExpr]) -- Lcated DFun
        (CoreSyn.CoreBndr,CoreSyn.CoreExpr))  -- Located Binder
      CoreSyn.CoreBndr                        -- Unlocated Var
loadExprFromTyThing bndr tyThing = case tyThing of
  GHC.AnId _id | Var.isId _id -> do
    let unfolding = IdInfo.unfoldingInfo $ Var.idInfo _id
    case unfolding of
      (CoreSyn.CoreUnfolding {}) ->
        Left $ Right (bndr, CoreSyn.unfoldingTemplate unfolding)
      (CoreSyn.DFunUnfolding _ _ es) ->
        Left $ Left (bndr, es)
      _ -> traceIf True ("Unwanted unfolding for " ++ show bndr ++ ": " ++ showPpr unfolding) $ Right bndr
  _ -> traceIf True ("Unwanted tyThing for " ++ show bndr ++ ": " ++ showPpr tyThing) $ Right bndr
