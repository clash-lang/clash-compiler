{-|
  Copyright   :  (C) 2013-2016, University of Twente
  License     :  BSD2 (see the file LICENSE)
  Maintainer  :  Christiaan Baaij <christiaan.baaij@gmail.com>
-}

{-# LANGUAGE CPP                 #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TupleSections       #-}

module CLaSH.GHC.LoadInterfaceFiles
  (loadExternalExprs)
where

-- External Modules
import           Data.Either (partitionEithers)
import           Data.List   (elemIndex, partition)
import           Data.Maybe  (isJust, isNothing)

-- GHC API
import qualified BasicTypes
import qualified Class
import qualified CoreFVs
import qualified CoreSyn
import qualified Demand
import           DynFlags    (unsafeGlobalDynFlags)
import qualified GHC
import qualified Id
import qualified IdInfo
import qualified IfaceSyn
import qualified LoadIface
import qualified Maybes
import qualified MkCore
import qualified MonadUtils
import qualified Name
import           Outputable  (showPpr, showSDoc, text, empty)
import qualified TcIface
import qualified TcRnMonad
import qualified TcRnTypes
import qualified UniqFM
import qualified Var
import qualified VarSet

-- Internal Modules
import           CLaSH.Util  (curLoc, traceIf)

runIfl :: GHC.GhcMonad m => GHC.Module -> TcRnTypes.IfL a -> m a
runIfl modName action = do
  hscEnv <- GHC.getSession
  let localEnv = TcRnTypes.IfLclEnv modName (text "runIfl")
                   UniqFM.emptyUFM UniqFM.emptyUFM
#if MIN_VERSION_GLASGOW_HASKELL(8,0,1,20161117)
  let globalEnv = TcRnTypes.IfGblEnv empty Nothing
#else
  let globalEnv = TcRnTypes.IfGblEnv Nothing
#endif
  MonadUtils.liftIO $ TcRnMonad.initTcRnIf 'r' hscEnv globalEnv
                        localEnv action

loadDecl :: IfaceSyn.IfaceDecl -> TcRnTypes.IfL GHC.TyThing
loadDecl = TcIface.tcIfaceDecl False

loadIface :: GHC.Module -> TcRnTypes.IfL (Maybe GHC.ModIface)
loadIface foundMod = do
  ifaceFailM <- LoadIface.findAndReadIface (Outputable.text "loadIface") foundMod False
  case ifaceFailM of
    Maybes.Succeeded (modInfo,_) -> return (Just modInfo)
    Maybes.Failed msg -> let msg' = concat [ $(curLoc)
                                           , "Failed to load interface for module: "
                                           , showPpr unsafeGlobalDynFlags foundMod
                                           , "\nReason: "
                                           , showSDoc unsafeGlobalDynFlags msg
                                           ]
                         in traceIf True msg' (return Nothing)

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
    let _idInfo    = Var.idInfo _id
        unfolding  = IdInfo.unfoldingInfo _idInfo
        inlineInfo = IdInfo.inlinePragInfo _idInfo
    in case unfolding of
      (CoreSyn.CoreUnfolding {}) ->
        case (BasicTypes.inl_inline inlineInfo,BasicTypes.inl_act inlineInfo) of
          (BasicTypes.NoInline,BasicTypes.AlwaysActive) -> Right bndr
          (BasicTypes.NoInline,BasicTypes.NeverActive)  -> Right bndr
          (BasicTypes.NoInline,_) -> Left (bndr, CoreSyn.unfoldingTemplate unfolding)
          _ -> Left (bndr, CoreSyn.unfoldingTemplate unfolding)
      (CoreSyn.DFunUnfolding dfbndrs dc es) ->
        let dcApp  = MkCore.mkCoreConApps dc es
            dfExpr = MkCore.mkCoreLams dfbndrs dcApp
        in Left (bndr,dfExpr)
      CoreSyn.NoUnfolding
        | Demand.isBottomingSig $ IdInfo.strictnessInfo _idInfo
        -> Left (bndr, MkCore.mkRuntimeErrorApp MkCore.aBSENT_ERROR_ID
                                                (Var.varType _id)
                                                "no_unfolding"
                )
      _ -> Right bndr
  _ -> Right bndr
