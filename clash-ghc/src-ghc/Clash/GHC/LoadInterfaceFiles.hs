{-|
  Copyright   :  (C) 2013-2016, University of Twente,
                     2016-2017, Myrtle Software Ltd
  License     :  BSD2 (see the file LICENSE)
  Maintainer  :  Christiaan Baaij <christiaan.baaij@gmail.com>
-}

{-# LANGUAGE CPP #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Clash.GHC.LoadInterfaceFiles
  ( loadExternalExprs
  , loadExternalBinders
  , getUnresolvedPrimitives
  )
where

-- External Modules
import           Control.Monad.IO.Class      (MonadIO (..))
import qualified Data.ByteString.Lazy.UTF8   as BLU
import qualified Data.ByteString.Lazy        as BL
import           Data.Either                 (partitionEithers)
import           Data.List                   (elemIndex, foldl', partition)
import qualified Data.Text                   as Text
import           Data.Maybe                  (isJust, isNothing,
                                              mapMaybe, catMaybes)
import           Data.Word                   (Word8)

-- GHC API
import           Annotations (Annotation(..), getAnnTargetName_maybe)
import qualified Annotations
import qualified Class
import qualified CoreFVs
import qualified CoreSyn
import qualified Demand
import           DynFlags                    (unsafeGlobalDynFlags)
import qualified GHC
import qualified Id
import qualified IdInfo
import qualified IfaceSyn
import qualified LoadIface
import qualified Maybes
import qualified MkCore
import qualified Module
import qualified MonadUtils
import qualified Name
import           Outputable                  (showPpr, showSDoc, text)
import qualified GhcPlugins                  (deserializeWithData, fromSerialized)
import qualified TcIface
import qualified TcRnMonad
import qualified TcRnTypes
import qualified UniqFM
import qualified UniqSet
import qualified Var

-- Internal Modules
import           Clash.Annotations.BitRepresentation.Internal
  (DataRepr', dataReprAnnToDataRepr')
import           Clash.Annotations.Primitive
import           Clash.Annotations.BitRepresentation (DataReprAnn)
import           Clash.Debug                         (traceIf)
import           Clash.Primitives.Types              (UnresolvedPrimitive, name)
import           Clash.Primitives.Util               (decodeOrErr)
import           Clash.GHC.GHC2Core                  (qualifiedNameString')
import           Clash.Util                          (curLoc)

runIfl :: GHC.GhcMonad m => GHC.Module -> TcRnTypes.IfL a -> m a
runIfl modName action = do
  hscEnv <- GHC.getSession
  let localEnv = TcRnTypes.IfLclEnv modName False (text "runIfl") Nothing
                   Nothing UniqFM.emptyUFM UniqFM.emptyUFM
  let globalEnv = TcRnTypes.IfGblEnv (text "Clash.runIfl") Nothing
  MonadUtils.liftIO $ TcRnMonad.initTcRnIf 'r' hscEnv globalEnv
                        localEnv action

loadDecl :: IfaceSyn.IfaceDecl -> TcRnTypes.IfL GHC.TyThing
loadDecl = TcIface.tcIfaceDecl False

loadIface :: GHC.Module -> TcRnTypes.IfL (Maybe GHC.ModIface)
loadIface foundMod = do
  ifaceFailM <- LoadIface.findAndReadIface (Outputable.text "loadIface")
                  (fst (Module.splitModuleInsts foundMod)) foundMod False
  case ifaceFailM of
    Maybes.Succeeded (modInfo,_) -> return (Just modInfo)
    Maybes.Failed msg -> let msg' = concat [ $(curLoc)
                                           , "Failed to load interface for module: "
                                           , showPpr unsafeGlobalDynFlags foundMod
                                           , "\nReason: "
                                           , showSDoc unsafeGlobalDynFlags msg
                                           ]
                         in traceIf True msg' (return Nothing)

loadExternalBinders
  :: GHC.GhcMonad m
  => HDL
  -> [CoreSyn.CoreBndr]
  -> m ( [(CoreSyn.CoreBndr,CoreSyn.CoreExpr)] -- Binders
       , [(CoreSyn.CoreBndr,Int)]              -- Class Ops
       , [CoreSyn.CoreBndr]                    -- Unlocatable
       , [Either UnresolvedPrimitive FilePath]
       , [DataRepr']
       )
loadExternalBinders hdl bndrs = do
  (locatedAndUnlocated, prims0, reprs0) <- unzip3 <$> mapM (loadExprFromIface hdl) bndrs
  let (located0, unlocated0) = partitionEithers locatedAndUnlocated
  (located1, classOps, unlocated1, prims1, reprs1, _seen) <-
    loadExternalExprs'
      hdl located0 [] unlocated0 (concat prims0) (concat reprs0)
      (UniqSet.mkUniqSet (map fst located0)) (map snd located0)
  pure (located1, classOps, unlocated1, prims1, reprs1)

loadExternalExprs
  :: GHC.GhcMonad m
  => HDL
  -> UniqSet.UniqSet CoreSyn.CoreBndr
  -> [CoreSyn.CoreBind]
  -> m ( [(CoreSyn.CoreBndr,CoreSyn.CoreExpr)] -- Binders
       , [(CoreSyn.CoreBndr,Int)]              -- Class Ops
       , [CoreSyn.CoreBndr]                    -- Unlocatable
       , [Either UnresolvedPrimitive FilePath]
       , [DataRepr']
       )
loadExternalExprs hdl = go [] [] [] [] []
  where
    go locatedExprs clsOps unlocated pFP reprs _ [] =
      return (locatedExprs,clsOps,unlocated,pFP,reprs)

    go locatedExprs clsOps unlocated pFP reprs visited (CoreSyn.NonRec _ e:bs) = do
      (locatedExprs',clsOps',unlocated',pFP',reprs',visited') <-
        loadExternalExprs' hdl locatedExprs clsOps unlocated pFP reprs visited [e]
      go locatedExprs' clsOps' unlocated' pFP' reprs' visited' bs

    go locatedExprs clsOps unlocated pFP reprs visited (CoreSyn.Rec bs:bs') = do
      (locatedExprs',clsOps',unlocated',pFP',reprs',visited') <-
        loadExternalExprs' hdl locatedExprs clsOps unlocated pFP reprs visited (map snd bs)
      go locatedExprs' clsOps' unlocated' pFP' reprs' visited' bs'

-- | Used by entry points: 'loadExternalExprs', 'loadExternalBinders'
loadExternalExprs'
  :: GHC.GhcMonad m
  => HDL
  -> [(CoreSyn.CoreBndr, CoreSyn.CoreExpr)]
  -> [(Var.Id, Int)]
  -> [CoreSyn.CoreBndr]
  -> [Either UnresolvedPrimitive FilePath]
  -> [DataRepr']
  -> UniqSet.UniqSet CoreSyn.CoreBndr
  -> [CoreSyn.CoreExpr]
  -> m ( [(CoreSyn.CoreBndr, CoreSyn.CoreExpr)]
       , [(Var.Id, Int)]
       , [CoreSyn.CoreBndr]
       , [Either UnresolvedPrimitive FilePath]
       , [DataRepr']
       , UniqSet.UniqSet CoreSyn.CoreBndr
       )
loadExternalExprs' _hdl locatedExprs clsOps unlocated pFP reprs visited [] =
  return (locatedExprs, clsOps, unlocated, pFP, reprs, visited)

loadExternalExprs' hdl locatedExprs clsOps unlocated pFP reprs visited (e:es) = do
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

  (locatedAndUnlocated, pFP', reprs') <- unzip3 <$> mapM (loadExprFromIface hdl) fvs'
  let (locatedExprs', unlocated') = partitionEithers locatedAndUnlocated

  let visited' = foldl' UniqSet.addListToUniqSet visited
                   [ map fst locatedExprs'
                   , unlocated'
                   , clsOps'
                   ]

  loadExternalExprs' hdl
      (locatedExprs'++locatedExprs)
      (clsOps''++clsOps)
      (unlocated'++unlocated)
      (concat pFP'++pFP)
      (concat reprs'++reprs)
      visited'
      (es ++ map snd locatedExprs')

loadExprFromIface
  :: GHC.GhcMonad m
  => HDL
  -> CoreSyn.CoreBndr
  -> m (Either
          (CoreSyn.CoreBndr,CoreSyn.CoreExpr) -- Located
          CoreSyn.CoreBndr                    -- Unlocated
       ,[Either UnresolvedPrimitive FilePath]
       ,[DataRepr']
       )
loadExprFromIface hdl bndr = do
  let moduleM = Name.nameModule_maybe $ Var.varName bndr
  case moduleM of
    Just nameMod -> runIfl nameMod $ do
      ifaceM <- loadIface nameMod
      case ifaceM of
        Nothing    -> return (Right bndr,[],[])
        Just iface -> do
          let decls = map snd (GHC.mi_decls iface)
          let nameFun = GHC.getOccName $ Var.varName bndr
          let declM = filter ((== nameFun) . Name.nameOccName . IfaceSyn.ifName) decls
          anns <- TcIface.tcIfaceAnnotations (GHC.mi_anns iface)
          primFPs   <- loadPrimitiveAnnotations hdl anns
          let reprs  = loadCustomReprAnnotations anns
          case declM of
            [namedDecl] -> do
              tyThing <- loadDecl namedDecl
              return (loadExprFromTyThing bndr tyThing,primFPs,reprs)
            _ -> return (Right bndr,primFPs,reprs)
    Nothing -> return (Right bndr,[],[])


loadCustomReprAnnotations
  :: [Annotations.Annotation]
  -> [DataRepr']
loadCustomReprAnnotations anns =
  catMaybes $ map go $ catMaybes $ zipWith filterNameless anns reprs
    where
        env         = Annotations.mkAnnEnv anns
        deserialize = GhcPlugins.deserializeWithData :: [Word8] -> DataReprAnn
        reprs       = UniqFM.eltsUFM (Annotations.deserializeAnns deserialize env)

        filterNameless
          :: Annotation
          -> [DataReprAnn]
          -> Maybe (Name.Name, [DataReprAnn])
        filterNameless (Annotation ann_target _) reprs' =
          (,reprs') <$> getAnnTargetName_maybe ann_target

        go
          :: (Name.Name, [DataReprAnn])
          -> Maybe DataRepr'
        go (_name, [])      = Nothing
        go (_name,  [repr]) = Just $ dataReprAnnToDataRepr' repr
        go (name, reprs')   =
          error $ $(curLoc) ++ "Multiple DataReprAnn annotations for same type: \n\n"
                            ++ (Outputable.showPpr DynFlags.unsafeGlobalDynFlags name)
                            ++ "\n\nReprs:\n\n"
                            ++ show reprs'

loadPrimitiveAnnotations ::
  MonadIO m
  => HDL
  -> [Annotations.Annotation]
  -> m [Either UnresolvedPrimitive FilePath]
loadPrimitiveAnnotations hdl anns =
  concat <$> mapM (getUnresolvedPrimitives hdl) prims
  where
    prims = mapMaybe filterPrim anns
    filterPrim (Annotations.Annotation target value) =
      (target,) <$> deserialize value
    deserialize =
      GhcPlugins.fromSerialized
        (GhcPlugins.deserializeWithData :: [Word8] -> Primitive)

getUnresolvedPrimitives
  :: MonadIO m
  => HDL
  -> (Annotations.CoreAnnTarget, Primitive)
  -> m ([Either UnresolvedPrimitive FilePath])
getUnresolvedPrimitives hdl targetPrim =
  case targetPrim of
    (_, Primitive hdls fp) | hdl `elem` hdls -> pure [Right fp]

    (target, InlinePrimitive hdls contentOrFp) | hdl `elem` hdls ->
      case target of
        -- Module annotation, can house many primitives
        Annotations.ModuleTarget _ ->
          liftIO (decodeOrErr contentOrFp <$> BL.readFile contentOrFp)
        Annotations.NamedTarget targetName0 ->
          let targetName1 = Text.unpack (qualifiedNameString' targetName0)
              prim =
                case decodeOrErr targetName1 (BLU.fromString contentOrFp) of
                  [] -> error $ "No annotations found for " ++ targetName1
                     ++ " even though it had an InlinePrimitive annotation."
                  [p] -> p
                  _ -> error $ "Multiple primitive definitions found in "
                    ++ "InlinePrimitive annotation for " ++ targetName1 ++ ". "
                    ++ "Expected a single one."

              primName = Text.unpack (name prim) in

          if primName /= targetName1 then
            error $ concat
              [ "Function " ++ targetName1 ++ " was annotated with an inline "
              , "primitive for " ++ primName ++ ". These names "
              , "should be the same." ]
          else
            pure [Left prim]
    _ ->
      -- Only consider the HDL (Verilog/SystemVerilog/VHDL) annotation we're
      -- currently targeting.
      pure []

loadExprFromTyThing
  :: CoreSyn.CoreBndr
  -> GHC.TyThing
  -> Either
       (CoreSyn.CoreBndr,CoreSyn.CoreExpr)  -- Located Binder
       CoreSyn.CoreBndr                     -- unlocatable Var
loadExprFromTyThing bndr tyThing = case tyThing of
  GHC.AnId _id | Var.isId _id ->
    let _idInfo    = Var.idInfo _id
        unfolding  = IdInfo.unfoldingInfo _idInfo
    in case unfolding of
      CoreSyn.CoreUnfolding {} ->
        Left (bndr, CoreSyn.unfoldingTemplate unfolding)
      (CoreSyn.DFunUnfolding dfbndrs dc es) ->
        let dcApp  = MkCore.mkCoreConApps dc es
            dfExpr = MkCore.mkCoreLams dfbndrs dcApp
        in Left (bndr,dfExpr)
      CoreSyn.NoUnfolding
        | Demand.isBottomingSig $ IdInfo.strictnessInfo _idInfo
        -> Left
            ( bndr
#if MIN_VERSION_ghc(8,2,2)
            , MkCore.mkAbsentErrorApp
#else
            , MkCore.mkRuntimeErrorApp
                MkCore.aBSENT_ERROR_ID
#endif
                (Var.varType _id)
                ("no_unfolding " ++ showPpr unsafeGlobalDynFlags bndr)
            )
      _ -> Right bndr
  _ -> Right bndr
