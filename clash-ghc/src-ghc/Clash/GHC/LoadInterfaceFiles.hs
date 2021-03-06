{-|
  Copyright   :  (C) 2013-2016, University of Twente,
                     2016-2017, Myrtle Software Ltd
  License     :  BSD2 (see the file LICENSE)
  Maintainer  :  Christiaan Baaij <christiaan.baaij@gmail.com>
-}

{-# LANGUAGE CPP #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Clash.GHC.LoadInterfaceFiles
  ( loadExternalExprs
  , loadExternalBinders
  , getUnresolvedPrimitives
  , LoadedBinders(..)
  , mergeLoadedBinders
  , emptyLb
  )
where

-- External Modules
import           Control.Monad.IO.Class      (MonadIO (..))
import qualified Data.ByteString.Lazy.UTF8   as BLU
import qualified Data.ByteString.Lazy        as BL
import           Data.Either                 (partitionEithers)
import           Data.List                   (elemIndex, foldl')
import qualified Data.Text                   as Text
import           Data.Maybe                  (isNothing, mapMaybe, catMaybes)
import           Data.Word                   (Word8)

-- GHC API
#if MIN_VERSION_ghc(9,0,0)
import           GHC.Types.Annotations (Annotation(..))
import qualified GHC.Types.Annotations as Annotations
import qualified GHC.Core.Class as Class
import qualified GHC.Core.FVs as CoreFVs
import qualified GHC.Core as CoreSyn
import qualified GHC.Types.Demand as Demand
import           GHC.Driver.Session as DynFlags (unsafeGlobalDynFlags)
import qualified GHC
import qualified GHC.Types.Id as Id
import qualified GHC.Types.Id.Info as IdInfo
import qualified GHC.Iface.Syntax as IfaceSyn
import qualified GHC.Iface.Load as LoadIface
import qualified GHC.Data.Maybe as Maybes
import qualified GHC.Core.Make as MkCore
import qualified GHC.Unit.Module as Module
import qualified GHC.Unit.Module.Env as ModuleEnv
import qualified GHC.Utils.Monad as MonadUtils
import qualified GHC.Types.Name as Name
import qualified GHC.Types.Name.Env as NameEnv
import           GHC.Utils.Outputable as Outputable (showPpr, showSDoc, text)
import qualified GHC.Plugins as GhcPlugins (deserializeWithData, fromSerialized)
import qualified GHC.IfaceToCore as TcIface
import qualified GHC.Tc.Utils.Monad as TcRnMonad
import qualified GHC.Tc.Types as TcRnTypes
import qualified GHC.Types.Unique.FM as UniqFM
import qualified GHC.Types.Unique.Set as UniqSet
import qualified GHC.Types.Var as Var
import qualified GHC.Unit.Types as UnitTypes
#else
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
#endif

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
import qualified Clash.Util.Interpolate              as I

-- | Data structure tracking loaded binders (and their related data)
data LoadedBinders = LoadedBinders
  { lbBinders :: [(CoreSyn.CoreBndr, CoreSyn.CoreExpr)]
  -- ^ Binder + expression it's binding
  , lbClassOps :: [(CoreSyn.CoreBndr, Int)]
  -- ^ Type class dict projection functions
  , lbUnlocatable :: [CoreSyn.CoreBndr]
  -- ^ Binders with missing unfoldings
  , lbPrims :: [Either UnresolvedPrimitive FilePath]
  -- ^ Primitives; either an primitive data structure or a path to a directory
  -- containing json files
  , lbReprs :: [DataRepr']
  -- ^ Custom data representations
  }

mergeLoadedBinders :: [LoadedBinders] -> LoadedBinders
mergeLoadedBinders lbs =
  LoadedBinders {
    lbBinders=concat (map lbBinders lbs)
  , lbClassOps=concat (map lbClassOps lbs)
  , lbUnlocatable=concat (map lbUnlocatable lbs)
  , lbPrims=concat (map lbPrims lbs)
  , lbReprs=concat (map lbReprs lbs)
  }

emptyLb :: LoadedBinders
emptyLb = LoadedBinders [] [] [] [] []

collectLbBinders :: LoadedBinders -> [CoreSyn.CoreBndr]
collectLbBinders LoadedBinders{lbBinders, lbUnlocatable, lbClassOps} =
  concat [map fst lbBinders, lbUnlocatable, map fst lbClassOps]

runIfl :: GHC.GhcMonad m => GHC.Module -> TcRnTypes.IfL a -> m a
runIfl modName action = do
  hscEnv <- GHC.getSession
  let localEnv = TcRnTypes.IfLclEnv modName
#if MIN_VERSION_ghc(9,0,0)
                   UnitTypes.NotBoot
#else
                   False
#endif
                   (text "runIfl") Nothing Nothing UniqFM.emptyUFM UniqFM.emptyUFM
  let globalEnv = TcRnTypes.IfGblEnv (text "Clash.runIfl") Nothing
  MonadUtils.liftIO $ TcRnMonad.initTcRnIf 'r' hscEnv globalEnv
                        localEnv action

loadDecl :: IfaceSyn.IfaceDecl -> TcRnTypes.IfL GHC.TyThing
loadDecl = TcIface.tcIfaceDecl False

loadIface :: GHC.Module -> TcRnTypes.IfL (Maybe GHC.ModIface)
loadIface foundMod = do
  ifaceFailM <- LoadIface.findAndReadIface (Outputable.text "loadIface")
#if MIN_VERSION_ghc(9,0,0)
                  (fst (Module.getModuleInstantiation foundMod)) foundMod UnitTypes.NotBoot
#else
                  (fst (Module.splitModuleInsts foundMod)) foundMod False
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

loadExternalBinders
  :: GHC.GhcMonad m
  => HDL
  -> [CoreSyn.CoreBndr]
  -> m LoadedBinders
loadExternalBinders hdl bndrs = do
  loaded <- mergeLoadedBinders <$> mapM (loadExprFromIface hdl) bndrs
  fst <$>
    loadExternalExprs'
      hdl
      loaded
      (UniqSet.mkUniqSet (collectLbBinders loaded))
      (map snd (lbBinders loaded))

loadExternalExprs
  :: GHC.GhcMonad m
  => HDL
  -> UniqSet.UniqSet CoreSyn.CoreBndr
  -> [CoreSyn.CoreBind]
  -> m LoadedBinders
loadExternalExprs hdl = go emptyLb
  where
    go loaded _ [] =
      return loaded
    go loaded0 visited0 (CoreSyn.NonRec _ e:bs) = do
      (loaded1, visited1) <- loadExternalExprs' hdl loaded0 visited0 [e]
      go loaded1 visited1 bs
    go loaded0 visited0 (CoreSyn.Rec bs:bs') = do
      (loaded1, visited1) <- loadExternalExprs' hdl loaded0 visited0 (map snd bs)
      go loaded1 visited1 bs'

-- | Used by entry points: 'loadExternalExprs', 'loadExternalBinders'
loadExternalExprs'
  :: GHC.GhcMonad m
  => HDL
  -> LoadedBinders
  -> UniqSet.UniqSet CoreSyn.CoreBndr
  -> [CoreSyn.CoreExpr]
  -> m ( LoadedBinders, UniqSet.UniqSet CoreSyn.CoreBndr)
loadExternalExprs' _hdl loaded visited [] =
  return (loaded, visited)
loadExternalExprs' hdl loaded0 visited0 (e:es) = do
  let
    isInteresting v =
         Var.isId v
      && not (v `UniqSet.elementOfUniqSet` visited0)
      && isNothing (Id.isDataConId_maybe v)

    fvs0 = CoreFVs.exprSomeFreeVarsList isInteresting e
    fvs1 = map (\v -> maybe (Left v) (Right . (v,)) (Id.isClassOpId_maybe v)) fvs0
    (fvs2, clsOps0) = partitionEithers fvs1
    clsOps1 = map goClsOp clsOps0

  loaded1 <- mergeLoadedBinders <$> mapM (loadExprFromIface hdl) fvs2

  loadExternalExprs'
    hdl
    (mergeLoadedBinders [loaded0, loaded1, emptyLb{lbClassOps=clsOps1}])
    (foldl' UniqSet.addListToUniqSet visited0 [collectLbBinders loaded1, map fst clsOps0])
    (es ++ map snd (lbBinders loaded1))
 where
  goClsOp :: (Var.Var, GHC.Class) -> (CoreSyn.CoreBndr, Int)
  goClsOp (v, c) =
    case elemIndex v (Class.classAllSelIds c) of
      Nothing -> error [I.i|
        Internal error: couldn't find class-method

          #{showPpr DynFlags.unsafeGlobalDynFlags v}

        in class

          #{showPpr DynFlags.unsafeGlobalDynFlags c}
      |]
      Just n -> (v, n)

loadExprFromIface
  :: GHC.GhcMonad m
  => HDL
  -> CoreSyn.CoreBndr
  -> m LoadedBinders
loadExprFromIface hdl bndr = do
  let moduleM = Name.nameModule_maybe $ Var.varName bndr
  case moduleM of
    Just nameMod -> runIfl nameMod $ do
      ifaceM <- loadIface nameMod
      case ifaceM of
        Nothing ->
          return (emptyLb{lbUnlocatable=[bndr]})
        Just iface -> do
          let decls = map snd (GHC.mi_decls iface)
          let nameFun = GHC.getOccName $ Var.varName bndr
          let declM = filter ((== nameFun) . Name.nameOccName . IfaceSyn.ifName) decls
          anns <- TcIface.tcIfaceAnnotations (GHC.mi_anns iface)
          primFPs   <- loadPrimitiveAnnotations hdl anns
          let reprs  = loadCustomReprAnnotations anns
              lb     = emptyLb{lbPrims=primFPs, lbReprs=reprs}
          case declM of
            [namedDecl] -> do
              tyThing <- loadDecl namedDecl
              case loadExprFromTyThing bndr tyThing of
                Left bndr1 -> return (lb{lbBinders=[bndr1]})
                Right unloc -> return (lb{lbUnlocatable=[unloc]})
            _ -> return (lb{lbUnlocatable=[bndr]})
    Nothing ->
      return (emptyLb{lbUnlocatable=[bndr]})


loadCustomReprAnnotations
  :: [Annotations.Annotation]
  -> [DataRepr']
loadCustomReprAnnotations anns =
  catMaybes $ map go $ catMaybes $ zipWith filterNameless anns reprs
    where
        env         = Annotations.mkAnnEnv anns
        deserialize = GhcPlugins.deserializeWithData :: [Word8] -> DataReprAnn
#if MIN_VERSION_ghc(9,0,0)
        reprs       = let (mEnv,nEnv) = Annotations.deserializeAnns deserialize env
                       in ModuleEnv.moduleEnvElts mEnv <> NameEnv.nameEnvElts nEnv
#else
        reprs       = UniqFM.eltsUFM (Annotations.deserializeAnns deserialize env)
#endif

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
#if MIN_VERSION_ghc(9,0,0)
        | Demand.isDeadEndSig $ IdInfo.strictnessInfo _idInfo
#else
        | Demand.isBottomingSig $ IdInfo.strictnessInfo _idInfo
#endif
        -> Left
            ( bndr
            , MkCore.mkAbsentErrorApp
                (Var.varType _id)
                ("no_unfolding " ++ showPpr unsafeGlobalDynFlags bndr)
            )
      _ -> Right bndr
  _ -> Right bndr

#if MIN_VERSION_ghc(9,0,0)
-- | Get the 'name' of an annotation target if it exists.
getAnnTargetName_maybe :: Annotations.AnnTarget name -> Maybe name
getAnnTargetName_maybe (Annotations.NamedTarget nm) = Just nm
getAnnTargetName_maybe _                            = Nothing
#endif
