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
import           Data.List   (elemIndex, foldl', partition)
import           Data.Maybe  (isJust, isNothing, mapMaybe)
import           Data.Word   (Word8)

import           CLaSH.Annotations.Primitive

-- GHC API
import qualified Annotations
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
#if MIN_VERSION_ghc(8,2,0)
import qualified Module
#endif
import qualified MonadUtils
import qualified Name
import           Outputable  (showPpr, showSDoc, text)
import qualified Serialized
import qualified TcIface
import qualified TcRnMonad
import qualified TcRnTypes
import qualified UniqFM
import qualified UniqSet
import qualified Var
#if !MIN_VERSION_ghc(8,2,0)
import qualified VarSet
#endif

-- Internal Modules
import           CLaSH.Util  ((***), curLoc, traceIf)

runIfl :: GHC.GhcMonad m => GHC.Module -> TcRnTypes.IfL a -> m a
runIfl modName action = do
  hscEnv <- GHC.getSession
#if MIN_VERSION_ghc(8,2,0)
  let localEnv = TcRnTypes.IfLclEnv modName False (text "runIfl") Nothing
                   Nothing UniqFM.emptyUFM UniqFM.emptyUFM
#else
  let localEnv = TcRnTypes.IfLclEnv modName (text "runIfl")
                   UniqFM.emptyUFM UniqFM.emptyUFM
#endif
  let globalEnv = TcRnTypes.IfGblEnv (text "CLaSH.runIfl") Nothing
  MonadUtils.liftIO $ TcRnMonad.initTcRnIf 'r' hscEnv globalEnv
                        localEnv action

loadDecl :: IfaceSyn.IfaceDecl -> TcRnTypes.IfL GHC.TyThing
loadDecl = TcIface.tcIfaceDecl False

loadIface :: GHC.Module -> TcRnTypes.IfL (Maybe GHC.ModIface)
loadIface foundMod = do
#if MIN_VERSION_ghc(8,2,0)
  ifaceFailM <- LoadIface.findAndReadIface (Outputable.text "loadIface")
                  (fst (Module.splitModuleInsts foundMod)) foundMod False
#else
  ifaceFailM <- LoadIface.findAndReadIface (Outputable.text "loadIface") foundMod False
#endif
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
  => HDL
  -> UniqSet.UniqSet CoreSyn.CoreBndr
  -> [CoreSyn.CoreBind]
  -> m ( [(CoreSyn.CoreBndr,CoreSyn.CoreExpr)] -- Binders
       , [(CoreSyn.CoreBndr,Int)]              -- Class Ops
       , [CoreSyn.CoreBndr]                    -- Unlocatable
       , [FilePath]
       )
loadExternalExprs hdl = go [] [] [] []
  where
    go locatedExprs clsOps unlocated pFP _ [] =
      return (locatedExprs,clsOps,unlocated,pFP)

    go locatedExprs clsOps unlocated pFP visited (CoreSyn.NonRec _ e:bs) = do
      (locatedExprs',clsOps',unlocated',pFP',visited') <-
        go' locatedExprs clsOps unlocated pFP visited [e]
      go locatedExprs' clsOps' unlocated' pFP' visited' bs

    go locatedExprs clsOps unlocated pFP visited (CoreSyn.Rec bs:bs') = do
      (locatedExprs',clsOps',unlocated',pFP',visited') <-
        go' locatedExprs clsOps unlocated pFP visited (map snd bs)
      go locatedExprs' clsOps' unlocated' pFP' visited' bs'

    go' locatedExprs clsOps unlocated pFP visited [] =
      return (locatedExprs,clsOps,unlocated,pFP,visited)

    go' locatedExprs clsOps unlocated pFP visited (e:es) = do
      let fvs = CoreFVs.exprSomeFreeVarsList
                  (\v -> Var.isId v &&
                         isNothing (Id.isDataConId_maybe v) &&
                         not (v `UniqSet.elementOfUniqSet` visited)
                  ) e

          (clsOps',fvs') = partition (isJust . Id.isClassOpId_maybe) fvs

          clsOps'' = map
            ( \v -> flip (maybe (error $ $(curLoc) ++ "Not a class op")) (Id.isClassOpId_maybe v) $ \c ->
                let clsIds = Class.classAllSelIds c
                in  maybe (error $ $(curLoc) ++ "Index not found")
                          (v,)
                          (elemIndex v clsIds)
            ) clsOps'

      ((locatedExprs',unlocated'),pFP') <-
         ((partitionEithers *** concat) . unzip) <$> mapM (loadExprFromIface hdl) fvs'

      let visited' = foldl' UniqSet.addListToUniqSet visited
                       [ map fst locatedExprs'
                       , unlocated'
                       , clsOps'
                       ]

      go' (locatedExprs'++locatedExprs)
          (clsOps''++clsOps)
          (unlocated'++unlocated)
          (pFP'++pFP)
          visited'
          (es ++ map snd locatedExprs')

loadExprFromIface ::
  GHC.GhcMonad m
  => HDL
  -> CoreSyn.CoreBndr
  -> m (Either
          (CoreSyn.CoreBndr,CoreSyn.CoreExpr)
          CoreSyn.CoreBndr
       ,[FilePath]
       )
loadExprFromIface hdl bndr = do
  let moduleM = Name.nameModule_maybe $ Var.varName bndr
  case moduleM of
    Just nameMod -> runIfl nameMod $ do
      ifaceM <- loadIface nameMod
      case ifaceM of
        Nothing    -> return (Right bndr,[])
        Just iface -> do
          let decls = map snd (GHC.mi_decls iface)
          let nameFun = GHC.getOccName $ Var.varName bndr
#if MIN_VERSION_ghc(8,2,0)
          let declM = filter ((== nameFun) . Name.nameOccName . IfaceSyn.ifName) decls
#else
          let declM = filter ((== nameFun) . IfaceSyn.ifName) decls
#endif
          anns <- TcIface.tcIfaceAnnotations (GHC.mi_anns iface)
          let primFPs = loadPrimitiveAnnotations hdl anns
          case declM of
            [namedDecl] -> do
              tyThing <- loadDecl namedDecl
              return (loadExprFromTyThing bndr tyThing,primFPs)
            _ -> return (Right bndr,primFPs)
    Nothing -> return (Right bndr,[])

loadPrimitiveAnnotations
  :: HDL
  -> [Annotations.Annotation]
  -> [FilePath]
loadPrimitiveAnnotations hdl anns = mapMaybe toFP (concat prims)
  where
    annEnv       = Annotations.mkAnnEnv anns
    prims        = UniqFM.eltsUFM (Annotations.deserializeAnns deserializer annEnv)
    deserializer = Serialized.deserializeWithData :: ([Word8] -> Primitive)
    toFP (Primitive hdl' fp)
      | hdl == hdl'
      = Just fp
    toFP _ = Nothing

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
