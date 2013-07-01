{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TupleSections       #-}
module CLaSH.GHC.LoadInterfaceFiles where

-- External Modules
import           Data.Either (partitionEithers)
import           Data.List   (elemIndex,partition)
import           Data.Maybe  (isJust,isNothing,mapMaybe)

-- GHC API
import qualified BasicTypes
import qualified Class
import qualified CoreSyn
import           CLaSH.GHC.Compat.CoreSyn (dfunArgExprs)
import qualified CoreFVs
import qualified Exception
import qualified FamInstEnv
import qualified GHC
import qualified HscTypes
import qualified Id
import qualified IdInfo
import qualified IfaceSyn
import qualified LoadIface
import qualified Maybes
import qualified MonadUtils
import qualified Name
import           CLaSH.GHC.Compat.Outputable (showPpr,showSDoc)
import           Outputable (text)
import qualified TcIface
import qualified TcRnMonad
import qualified TcRnTypes
import qualified UniqFM
import UniqSupply (UniqSupply)
import qualified Var
import qualified VarSet

-- Internal Modules
import           CLaSH.GHC.Types
import           CLaSH.Util (curLoc,mapAccumLM,maybe',second,traceIf)

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
loadDecl decl = TcIface.tcIfaceDecl False decl

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
  => UniqSupply
  -> [CoreSyn.CoreExpr]
  -> [CoreSyn.CoreBndr]
  -> m ( [(CoreSyn.CoreBndr,CoreSyn.CoreExpr)]    -- Binders
       , [CoreDFUN]  -- Dictionary functions
       , [(CoreSyn.CoreBndr,Int)]                 -- Class Ops
       , [CoreSyn.CoreBndr]                       -- Unlocatable
       )
loadExternalExprs _ [] _ = return ([],[],[],[])
loadExternalExprs us (expr:exprs) visited = do
  let fvs = VarSet.varSetElems $ CoreFVs.exprSomeFreeVars
              (\v -> Var.isId v &&
                     isNothing (Id.isDataConId_maybe v) &&
                     v `notElem` visited
              ) expr

  let (clsOps,fvs') = partition (isJust . Id.isClassOpId_maybe) fvs

  (us',(located,unlocated)) <- fmap (second partitionEithers)
                                  $ mapAccumLM loadExprFromIface us fvs'

  let (locatedDFuns,locatedExprs) = partitionEithers located
  let visited' = concat [ map fst locatedExprs
                        , map fst locatedDFuns
                        , concatMap (snd . fst . snd) locatedDFuns
                        , unlocated
                        , clsOps
                        , visited
                        ]

  (locatedExprs', locatedDFuns', clsOps', unlocated') <-
    loadExternalExprs
      us'
      ( exprs ++
        map snd locatedExprs ++
        concatMap (snd . snd) locatedDFuns
      ) visited'

  let clsOps'' = map
       ( \v -> maybe' (error $ $(curLoc) ++ "Not a class op") (Id.isClassOpId_maybe v) $ \c ->
           let clsIds = Class.classAllSelIds c
               l      = Class.classArity c
           in maybe' (error $ $(curLoc) ++ "Index not found") (elemIndex v clsIds) ((v,) . (+l))
   --         (v,)
   --         . fromMaybe (error $ $(curLoc) ++ "Index not found")
   --         . elemIndex v
   --         . Class.classAllSelIds
   --         . fromMaybe (error $ $(curLoc) ++ "Not a class op")
   --         $ Id.isClassOpId_maybe v
       ) clsOps

  return ( locatedExprs ++ locatedExprs'
         , locatedDFuns ++ locatedDFuns'
         , clsOps''     ++ clsOps'
         , unlocated    ++ unlocated'
         )

loadExprFromIface ::
  GHC.GhcMonad m
  => UniqSupply
  -> CoreSyn.CoreBndr
  -> m (UniqSupply
       ,Either
          (Either
            CoreDFUN
            (CoreSyn.CoreBndr,CoreSyn.CoreExpr))
          CoreSyn.CoreBndr
       )
loadExprFromIface us bndr = do
  let moduleM = Name.nameModule_maybe $ Var.varName bndr
  case moduleM of
    Just nameMod -> runIfl nameMod $ do
      ifaceM <- loadIface nameMod
      case ifaceM of
        Nothing    -> return $! (us,Right bndr)
        Just iface -> do
          let decls = map snd (GHC.mi_decls iface)
          let nameFun = GHC.getOccName $ Var.varName bndr
          let declM = filter ((== nameFun) . IfaceSyn.ifName) decls
          case declM of
            [namedDecl] -> do
              tyThing <- loadDecl namedDecl
              return $ loadExprFromTyThing us bndr tyThing
            _ -> return $! (us,Right bndr)
    Nothing -> return $! (us,Right bndr)

loadExprFromTyThing ::
  UniqSupply
  -> CoreSyn.CoreBndr
  -> GHC.TyThing
  -> (UniqSupply
     ,Either
       (Either
         CoreDFUN                              -- Located DFun
         (CoreSyn.CoreBndr,CoreSyn.CoreExpr))  -- Located Binder
       CoreSyn.CoreBndr                        -- unlocatable Var
     )
loadExprFromTyThing us bndr tyThing = case tyThing of
  GHC.AnId _id | Var.isId _id ->
    let unfolding  = IdInfo.unfoldingInfo $ Var.idInfo _id
        inlineInfo = IdInfo.inlinePragInfo $ Var.idInfo _id
        dfunTy     = Id.idType _id
    in case unfolding of
      (CoreSyn.CoreUnfolding {}) ->
        case BasicTypes.inl_inline inlineInfo of
          BasicTypes.NoInline -> (us,Right bndr)
          _ -> (us,Left $! (Right (bndr, CoreSyn.unfoldingTemplate unfolding)))
      (CoreSyn.DFunUnfolding dfbndrs _ es) ->
        let (exprs,us') = dfunArgExprs us dfunTy es
        in (us',Left $! Left (bndr, (partition Var.isTyVar dfbndrs,exprs)))
      _ -> (us,Right bndr)
  _ -> (us,Right bndr)
