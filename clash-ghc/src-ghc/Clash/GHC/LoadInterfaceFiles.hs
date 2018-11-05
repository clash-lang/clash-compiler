{-|
  Copyright   :  (C) 2013-2016, University of Twente,
                     2016-2017, Myrtle Software Ltd
  License     :  BSD2 (see the file LICENSE)
  Maintainer  :  Christiaan Baaij <christiaan.baaij@gmail.com>
-}

{-# LANGUAGE CPP                 #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TupleSections       #-}

module Clash.GHC.LoadInterfaceFiles
  ( loadExternalExprs
  , primitiveFilePath
  )
where

-- External Modules
import           Control.Monad.IO.Class      (MonadIO (..))
import           Data.Char                   (toLower)
import           Data.Either                 (partitionEithers)
import           Data.List                   (elemIndex, foldl', partition)
import           Data.Maybe                  (fromMaybe, isJust, isNothing,
                                              mapMaybe, catMaybes)
import           Data.Word                   (Word8)
import           System.Directory            (createDirectoryIfMissing)
import           System.FilePath.Posix       ((<.>), (</>))

-- GHC API
import           Annotations (Annotation(..), getAnnTargetName_maybe)
import qualified Annotations
import qualified BasicTypes
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
import           Clash.Util                          (curLoc, traceIf)

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

loadExternalExprs ::
  GHC.GhcMonad m
  => HDL
  -> UniqSet.UniqSet CoreSyn.CoreBndr
  -> [CoreSyn.CoreBind]
  -> m ( [(CoreSyn.CoreBndr,CoreSyn.CoreExpr)] -- Binders
       , [(CoreSyn.CoreBndr,Int)]              -- Class Ops
       , [CoreSyn.CoreBndr]                    -- Unlocatable
       , [FilePath]
       , [DataRepr']
       )
loadExternalExprs hdl = go [] [] [] [] []
  where
    go locatedExprs clsOps unlocated pFP reprs _ [] =
      return (locatedExprs,clsOps,unlocated,pFP,reprs)

    go locatedExprs clsOps unlocated pFP reprs visited (CoreSyn.NonRec _ e:bs) = do
      (locatedExprs',clsOps',unlocated',pFP',reprs',visited') <-
        go' locatedExprs clsOps unlocated pFP reprs visited [e]
      go locatedExprs' clsOps' unlocated' pFP' reprs' visited' bs

    go locatedExprs clsOps unlocated pFP reprs visited (CoreSyn.Rec bs:bs') = do
      (locatedExprs',clsOps',unlocated',pFP',reprs',visited') <-
        go' locatedExprs clsOps unlocated pFP reprs visited (map snd bs)
      go locatedExprs' clsOps' unlocated' pFP' reprs' visited' bs'

    go' locatedExprs clsOps unlocated pFP reprs visited [] =
      return (locatedExprs,clsOps,unlocated,pFP,reprs,visited)

    go' locatedExprs clsOps unlocated pFP reprs visited (e:es) = do
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

      go' (locatedExprs'++locatedExprs)
          (clsOps''++clsOps)
          (unlocated'++unlocated)
          (concat pFP'++pFP)
          (concat reprs'++reprs)
          visited'
          (es ++ map snd locatedExprs')

loadExprFromIface ::
  GHC.GhcMonad m
  => HDL
  -> CoreSyn.CoreBndr
  -> m (Either
          (CoreSyn.CoreBndr,CoreSyn.CoreExpr) -- Located
          CoreSyn.CoreBndr                    -- Unlocated
       ,[FilePath]
       ,[DataRepr']
       )
loadExprFromIface hdl bndr = do
  dflags <- GHC.getSessionDynFlags

  let -- Using the stub directory as the output directory for inline primitives
      outDir = fromMaybe "." $ GHC.stubDir dflags
      moduleM = Name.nameModule_maybe $ Var.varName bndr
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
          primFPs   <- loadPrimitiveAnnotations hdl outDir anns
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
  -> FilePath
  -> [Annotations.Annotation]
  -> m [FilePath]
loadPrimitiveAnnotations hdl outDir anns =
  sequence $ mapMaybe (primitiveFilePath hdl outDir) prims
  where
    prims = mapMaybe filterPrim anns
    filterPrim (Annotations.Annotation target value) =
      (target,) <$> deserialize value
    deserialize =
      GhcPlugins.fromSerialized
        (GhcPlugins.deserializeWithData :: [Word8] -> Primitive)

primitiveFilePath ::
  MonadIO m
  => HDL
  -> FilePath
  -> (Annotations.CoreAnnTarget, Primitive)
  -> Maybe (m FilePath)
primitiveFilePath hdl outDir targetPrim =
  case targetPrim of
    (_, Primitive hdl' fp)
      | hdl == hdl' -> Just $ pure fp
    (target, InlinePrimitive hdl' content)
      | hdl == hdl' -> Just . liftIO $ do
        let qualifiedName =
              case target of
                Annotations.NamedTarget name -> Name.nameStableString name
                Annotations.ModuleTarget mod' -> Module.moduleStableString mod'
            inlinePrimsDir =
              outDir </> "inline_primitives" </> map toLower (show hdl)
            primFile = inlinePrimsDir </> qualifiedName <.> "json"
        createDirectoryIfMissing True inlinePrimsDir
        writeFile primFile content
        return inlinePrimsDir
    _ -> Nothing

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
      CoreSyn.CoreUnfolding {} ->
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
