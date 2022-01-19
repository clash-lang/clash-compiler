{-|
  Copyright   :  (C) 2013-2016, University of Twente,
                     2016-2017, Myrtle Software Ltd
                     2022,      QBayLogic B.V.
  License     :  BSD2 (see the file LICENSE)
  Maintainer  :  QBayLogic B.V. <devops@qbaylogic.com>
-}

{-# LANGUAGE CPP #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}

module Clash.GHC.LoadInterfaceFiles
  ( loadExternalExprs
  , loadExternalBinders
  , getUnresolvedPrimitives
  , LoadedBinders(..)
  )
where

-- External Modules
import           Control.Monad.IO.Class      (MonadIO (..))
import           Control.Monad               (forM_, join)
import           Control.Monad.State.Strict
  (StateT, gets, MonadState (get), MonadTrans (lift), execStateT)
import           Control.Monad.Trans.State.Strict (modify)
import           Control.Monad.Extra         (unlessM)
import qualified Data.ByteString.Lazy.UTF8   as BLU
import qualified Data.ByteString.Lazy        as BL
import qualified Data.Sequence               as Seq
import           Data.Sequence               (Seq)
import           Data.Either                 (partitionEithers)
import           Data.Foldable               (foldl')
import           Data.List                   (elemIndex)
import qualified Data.Text                   as Text
import           Data.Maybe                  (isNothing, mapMaybe, catMaybes)
import           Data.Map.Strict             (Map)
import qualified Data.Map.Strict             as Map
import           Data.Set                    (Set)
import qualified Data.Set                    as Set
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
import qualified Var
#endif

-- Internal Modules
import           Clash.Annotations.BitRepresentation.Internal
  (DataRepr', dataReprAnnToDataRepr')
import           Clash.Annotations.Primitive
import           Clash.Annotations.BitRepresentation (DataReprAnn)
import           Clash.Debug                         (traceIf)
import           Clash.Primitives.Types              (UnresolvedPrimitive, name)
import           Clash.Primitives.Util               (decodeOrErrJson, decodeOrErrYaml)
import           Clash.GHC.GHC2Core                  (qualifiedNameString')
import           Clash.Util                          (curLoc)
import qualified Clash.Util.Interpolate              as I

-- | Data structure tracking loaded binders (and their related data)
data LoadedBinders = LoadedBinders
  { lbBinders :: !(Map CoreSyn.CoreBndr CoreSyn.CoreExpr)
  -- ^ Binder + expression it's binding
  , lbClassOps :: !(Map CoreSyn.CoreBndr Int)
  -- ^ Type class dict projection functions
  , lbUnlocatable :: !(Set CoreSyn.CoreBndr)
  -- ^ Binders with missing unfoldings
  , lbPrims :: !(Seq (Either UnresolvedPrimitive FilePath))
  -- ^ Primitives; either an primitive data structure or a path to a directory
  -- containing json files
  , lbReprs :: !(Seq DataRepr')
  -- ^ Custom data representations
  , lbCache :: !DeclCache
  -- ^ Loaded module cache
  }

type LoadedBinderT m a = StateT LoadedBinders m a

-- | Stores modules with easy binder lookup
type DeclCache = Map GHC.Module (Maybe (Map GHC.Name IfaceSyn.IfaceDecl))


-- | Collects free variables in an expression, and splits them into "normal"
-- free variables and class ops.
bndrsInExpr :: CoreSyn.CoreExpr -> ([CoreSyn.CoreBndr], [(CoreSyn.CoreBndr, Int)])
bndrsInExpr e = partitionEithers (map go freeVars)
 where
  freeVars = CoreFVs.exprSomeFreeVarsList isInteresting e
  isInteresting v = Var.isId v && isNothing (Id.isDataConId_maybe v)

  go :: Var.Var -> Either Var.Id (CoreSyn.CoreBndr, Int)
  go v = case Id.isClassOpId_maybe v of
    Just cls -> Right (v, goClsOp v cls)
    Nothing -> Left v

  goClsOp :: Var.Var -> GHC.Class -> Int
  goClsOp v c =
    case elemIndex v (Class.classAllSelIds c) of
      Nothing -> error [I.i|
        Internal error: couldn't find class method

          #{showPpr DynFlags.unsafeGlobalDynFlags v}

        in class

          #{showPpr DynFlags.unsafeGlobalDynFlags c}
      |]
      Just n -> n

-- | Add a binder to the appropriate fields of 'LoadedBinders', and recursively
-- load binders found in the optionally supplied expression.
addBndrM ::
  GHC.GhcMonad m =>
  HDL ->
  CoreSyn.CoreBndr ->
  Maybe CoreSyn.CoreExpr ->
  LoadedBinderT m ()
addBndrM hdl bndr exprM =
  case exprM of
    Nothing ->
      modify $ \lb@LoadedBinders{..} ->
        lb{lbUnlocatable=Set.insert bndr lbUnlocatable}
    Just expr -> do
      -- Add current expression and its class ops
      let (fvs, clsOps) = bndrsInExpr expr
      modify $ \lb@LoadedBinders{..} ->
        lb { lbBinders=Map.insert bndr expr lbBinders
           , lbClassOps=mapInsertAll lbClassOps clsOps }

      -- Load all free variables - if not yet loaded
      forM_ fvs $ \v ->
        unlessM (isLoadedBinderM v) (loadExprFromIface hdl v)
 where
  -- Insert a list of keys and values into a 'Map'
  mapInsertAll :: Ord k => Map k a -> [(k, a)] -> Map k a
  mapInsertAll = foldl' (\m (k, v) -> Map.insert k v m)


isLoadedBinderM :: Monad m => CoreSyn.CoreBndr -> LoadedBinderT m Bool
isLoadedBinderM bndr = gets $ \LoadedBinders{..} ->
     Map.member bndr lbBinders
  || Map.member bndr lbClassOps
  || Set.member bndr lbUnlocatable

emptyLb :: LoadedBinders
emptyLb = LoadedBinders
  { lbBinders = mempty
  , lbClassOps = mempty
  , lbUnlocatable = mempty
  , lbPrims = mempty
  , lbReprs = mempty
  , lbCache = mempty
  }

#if MIN_VERSION_ghc(9,0,0)
notBoot :: UnitTypes.IsBootInterface
notBoot = UnitTypes.NotBoot
#else
notBoot :: Bool
notBoot = False
#endif

runIfl :: GHC.GhcMonad m => GHC.Module -> TcRnTypes.IfL a -> m a
runIfl modName action = do
  let
    localEnv = TcRnTypes.IfLclEnv
      { TcRnTypes.if_mod = modName
      , TcRnTypes.if_boot = notBoot
      , TcRnTypes.if_loc = text "runIfl"
      , TcRnTypes.if_nsubst = Nothing
      , TcRnTypes.if_implicits_env = Nothing
      , TcRnTypes.if_tv_env = UniqFM.emptyUFM
      , TcRnTypes.if_id_env = UniqFM.emptyUFM
      }

    globalEnv = TcRnTypes.IfGblEnv
      { TcRnTypes.if_doc = text "Clash.runIfl"
      , TcRnTypes.if_rec_types = Nothing
      }

  hscEnv <- GHC.getSession
  MonadUtils.liftIO $
    TcRnMonad.initTcRnIf 'r' hscEnv globalEnv localEnv action

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

-- | Given a list of top-level binders, recursively load all the binders,
-- primitives, and type classes it is using. (Exported function.)
loadExternalBinders :: GHC.GhcMonad m => HDL -> [CoreSyn.CoreBndr] -> m LoadedBinders
loadExternalBinders hdl bndrs =
  flip execStateT emptyLb $
    mapM_ (loadExprFromIface hdl) bndrs

-- Given a list of binds, recursively load all its binders, primitives, and
-- type classes it is using. (Exported function.)
loadExternalExprs :: GHC.GhcMonad m => HDL -> [CoreSyn.CoreBind] -> m LoadedBinders
loadExternalExprs hdl binds0 =
  flip execStateT initLb $
    mapM_ (\(b, e) -> addBndrM hdl b (Just e)) binds1
 where
  -- 'lbBinders' is preinitialized with all binders in given binds, as the given
  -- binders can't be loaded from precompiled modules
  initLb = emptyLb{lbBinders=Map.fromList binds1}
  binds1 = CoreSyn.flattenBinds binds0

-- | Try to fetch a IfaceDecl from a 'DeclCache'. If a module has not been loaded
-- before, load it using GHC. Additionally, add annotations mentioned in the
-- module to 'LoadedBinders'.
getIfaceDeclM ::
  forall m.
  GHC.GhcMonad m =>
  HDL ->
  -- | Binder to load
  CoreSyn.CoreBndr ->
  -- | Declaration, if found
  LoadedBinderT m (Maybe (GHC.Module, IfaceSyn.IfaceDecl))
getIfaceDeclM hdl bndr = do
  let modM = Name.nameModule_maybe bndrName
  join <$> mapM go modM
 where
  bndrName = Var.varName bndr

  go :: GHC.Module -> LoadedBinderT m (Maybe (GHC.Module, IfaceSyn.IfaceDecl))
  go nameMod = do
    LoadedBinders{lbCache} <- get
    case Map.lookup nameMod lbCache of
      Nothing -> do
        -- Not loaded before
        ifaceM <- lift (runIfl nameMod (loadIface nameMod))
        case ifaceM of
          Just iface -> do
            -- Add binder : decl map to cache
            let
              decls = map snd (GHC.mi_decls iface)
              names = map IfaceSyn.ifName decls
            let declMap = Just (Map.fromList (zip names decls))
            modify (\lb -> lb{lbCache=Map.insert nameMod declMap lbCache})

            -- Load annotations and add them to state
            loadAnnotationsM hdl nameMod iface
          Nothing ->
            -- XXX: 'runIfl' should probably error hard if this happens?
            modify (\lb -> lb{lbCache=Map.insert nameMod Nothing lbCache})

        -- Update cache and try again
        go nameMod

      Just Nothing ->
        -- Loaded before, but couldn't find decl
        pure Nothing
      Just (Just declMap) ->
        -- Loaded before, decl found
        pure ((nameMod,) <$> Map.lookup bndrName declMap)

loadAnnotationsM ::
  GHC.GhcMonad m =>
  HDL ->
  GHC.Module ->
  GHC.ModIface ->
  StateT LoadedBinders m ()
loadAnnotationsM hdl modName iface = do
  anns <- lift (runIfl modName (TcIface.tcIfaceAnnotations (GHC.mi_anns iface)))
  primFPs <- loadPrimitiveAnnotations hdl anns
  let reprs = loadCustomReprAnnotations anns
  modify $ \lb@LoadedBinders{..} -> lb
    { lbPrims = lbPrims <> Seq.fromList primFPs
    , lbReprs = lbReprs <> Seq.fromList reprs
    }

loadExprFromIface ::
  GHC.GhcMonad m =>
  HDL ->
  CoreSyn.CoreBndr ->
  LoadedBinderT m ()
loadExprFromIface hdl bndr = do
  namedDeclM <- getIfaceDeclM hdl bndr
  case namedDeclM of
    Nothing -> addBndrM hdl bndr Nothing
    Just (nameMod, namedDecl) -> do
      tyThing <- lift (runIfl nameMod (loadDecl namedDecl))
      addBndrM hdl bndr (loadExprFromTyThing bndr tyThing)


loadCustomReprAnnotations :: [Annotations.Annotation] -> [DataRepr']
loadCustomReprAnnotations anns =
  mapMaybe go $ catMaybes $ zipWith filterNameless anns reprs
 where
  env = Annotations.mkAnnEnv anns
  deserialize = GhcPlugins.deserializeWithData :: [Word8] -> DataReprAnn

#if MIN_VERSION_ghc(9,0,0)
  (mEnv, nEnv) = Annotations.deserializeAnns deserialize env
  reprs = ModuleEnv.moduleEnvElts mEnv <> NameEnv.nameEnvElts nEnv
#else
  reprs = UniqFM.eltsUFM (Annotations.deserializeAnns deserialize env)
#endif

  filterNameless :: Annotation -> [DataReprAnn] -> Maybe (Name.Name, [DataReprAnn])
  filterNameless (Annotation ann_target _) reprs' =
    (,reprs') <$> getAnnTargetName_maybe ann_target

  go :: (Name.Name, [DataReprAnn]) -> Maybe DataRepr'
  go (_name, []) = Nothing
  go (_name,  [repr]) = Just $ dataReprAnnToDataRepr' repr
  go (name, reprs')   =
    error [I.i|
      Multiple DataReprAnn annotations for same type:

        #{Outputable.showPpr DynFlags.unsafeGlobalDynFlags name}

      Reprs:

        #{reprs'}
    |]

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
  -> m [Either UnresolvedPrimitive FilePath]
getUnresolvedPrimitives hdl (target, prim) | hdl `elem` primHdls prim =
  case prim of
    Primitive _ fp -> pure [Right fp]

    InlineYamlPrimitive _ contentOrFp ->
      case target of
        -- Module annotation, can house many primitives
        Annotations.ModuleTarget _ ->
          liftIO (decodeOrErrYaml contentOrFp <$> BL.readFile contentOrFp)
        Annotations.NamedTarget targetName0 ->
          let targetName1 = Text.unpack (qualifiedNameString' targetName0)
              primOrErr = decodeOrErrYaml targetName1 (BLU.fromString contentOrFp)
              primName = Text.unpack (name primOrErr) in

          if primName /= targetName1
          then inlineNameError targetName1 primName
          else pure [Left primOrErr]

    InlinePrimitive _ contentOrFp ->
      case target of
        -- Module annotation, can house many primitives
        Annotations.ModuleTarget _ ->
          liftIO (decodeOrErrJson contentOrFp <$> BL.readFile contentOrFp)
        Annotations.NamedTarget targetName0 ->
          let targetName1 = Text.unpack (qualifiedNameString' targetName0)
              primOrErr =
                case decodeOrErrJson targetName1 (BLU.fromString contentOrFp) of
                  [] -> error $ "No annotations found for " ++ targetName1
                     ++ " even though it had an InlinePrimitive annotation."
                  [p] -> p
                  _ -> error $ "Multiple primitive definitions found in "
                    ++ "InlinePrimitive annotation for " ++ targetName1 ++ ". "
                    ++ "Expected a single one."

              primName = Text.unpack (name primOrErr) in

          if primName /= targetName1
          then inlineNameError targetName1 primName
          else pure [Left primOrErr]
 where
  inlineNameError targetName primName =
    error $ concat
      [ "Function " ++ targetName ++ " was annotated with an inline "
      , "primitive for " ++ primName ++ ". These names "
      , "should be the same." ]

  primHdls = \case
    Primitive hdls _ -> hdls
    InlinePrimitive hdls _ -> hdls
    InlineYamlPrimitive hdls _ -> hdls

getUnresolvedPrimitives _ _  = return []

loadExprFromTyThing :: CoreSyn.CoreBndr -> GHC.TyThing -> Maybe CoreSyn.CoreExpr
loadExprFromTyThing bndr tyThing = case tyThing of
  GHC.AnId _id | Var.isId _id ->
    let _idInfo    = Var.idInfo _id
        unfolding  = IdInfo.unfoldingInfo _idInfo
    in case unfolding of
      CoreSyn.CoreUnfolding {} ->
        Just (CoreSyn.unfoldingTemplate unfolding)
      CoreSyn.DFunUnfolding dfbndrs dc es ->
        Just (MkCore.mkCoreLams dfbndrs (MkCore.mkCoreConApps dc es))
      CoreSyn.NoUnfolding
#if MIN_VERSION_ghc(9,0,0)
        | Demand.isDeadEndSig $ IdInfo.strictnessInfo _idInfo
#else
        | Demand.isBottomingSig $ IdInfo.strictnessInfo _idInfo
#endif
        -> do
          let noUnfoldingErr = "no_unfolding " ++ showPpr unsafeGlobalDynFlags bndr
          Just (MkCore.mkAbsentErrorApp (Var.varType _id) noUnfoldingErr)
      _ -> Nothing
  _ -> Nothing

#if MIN_VERSION_ghc(9,0,0)
-- | Get the 'name' of an annotation target if it exists.
getAnnTargetName_maybe :: Annotations.AnnTarget name -> Maybe name
getAnnTargetName_maybe (Annotations.NamedTarget nm) = Just nm
getAnnTargetName_maybe _                            = Nothing
#endif
