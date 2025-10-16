{-|
  Copyright   :  (C) 2012-2016, University of Twente,
                     2016-2017, Myrtle Software Ltd,
                     2017-2018, Google Inc.,
                     2021-2024, QBayLogic B.V.
                     2022     , Google Inc.
  License     :  BSD2 (see the file LICENSE)
  Maintainer  :  QBayLogic B.V. <devops@qbaylogic.com>

  Create Netlists out of normalized CoreHW Terms
-}

{-# LANGUAGE CPP #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Clash.Netlist where

import           Control.Exception                (throw)
import           Control.Lens                     ((.=), (<~))
import qualified Control.Lens                     as Lens
import           Control.Monad                    (zipWithM)
import           Control.Monad.Extra              (concatMapM, mapMaybeM)
import           Control.Monad.Reader             (runReaderT)
import           Control.Monad.State.Strict       (State, runStateT, runState)
import           Data.Bifunctor                   (first, second)
import           Data.Char                        (ord)
import           Data.Either                      (partitionEithers, rights)
import           Data.Foldable                    (foldlM)
import           Data.List                        (elemIndex, partition)
import           Data.List.Extra                  (zipEqual)
import           Data.List.NonEmpty               (NonEmpty (..))
import qualified Data.List.NonEmpty.Extra         as NE
import           Data.Maybe
  (listToMaybe, fromMaybe)
import qualified Data.Map.Ordered                 as OMap
import qualified Data.Set                         as Set
import qualified Data.Text                        as StrictText
import           GHC.Stack                        (HasCallStack)

#if MIN_VERSION_ghc(9,0,0)
import           GHC.Utils.Outputable             (ppr, showSDocUnsafe)
import           GHC.Types.SrcLoc                 (isGoodSrcSpan)
#else
import           Outputable                       (ppr, showSDocUnsafe)
import           SrcLoc                           (isGoodSrcSpan)
#endif

import           Clash.Annotations.Primitive      (HDL)
import           Clash.Annotations.BitRepresentation.ClashLib
  (coreToType')
import           Clash.Annotations.BitRepresentation.Internal
  (CustomReprs, DataRepr'(..), ConstrRepr'(..), getDataRepr, getConstrRepr)
import           Clash.Core.DataCon               (DataCon (..))
import           Clash.Core.HasType
import           Clash.Core.Literal               (Literal (..))
import           Clash.Core.Name                  (Name(..))
import           Clash.Core.Pretty                (showPpr)
import           Clash.Core.Term
  (IsMultiPrim (..), PrimInfo (..), mpi_resultTypes,  Alt, Pat (..), Term (..),
   TickInfo (..), collectArgs, collectArgsTicks,
   collectTicks, mkApps, mkTicks, stripTicks)
import qualified Clash.Core.Term                  as Core
import           Clash.Core.TermInfo              (multiPrimInfo', splitMultiPrimArgs)
import           Clash.Core.Type
  (Type (..), coreView1, splitFunForallTy, splitCoreFunForallTy)
import           Clash.Core.TyCon                 (TyConMap)
import           Clash.Core.Util                  (splitShouldSplit)
import           Clash.Core.Var                   (Id, Var (..), isGlobalId)
import           Clash.Core.VarEnv
  (VarEnv, emptyInScopeSet, emptyVarEnv, extendVarEnv, lookupVarEnv,
   lookupVarEnv')
import           Clash.Driver.Types               (BindingMap, Binding(..), ClashEnv(..), ClashOpts (..))
import           Clash.Netlist.BlackBox
import qualified Clash.Netlist.Id                 as Id
import           Clash.Netlist.Types              as HW
import           Clash.Netlist.Util
import           Clash.Primitives.Types           as P
import           Clash.Util
import qualified Clash.Util.Interpolate           as I

-- | Generate a hierarchical netlist out of a set of global binders with
-- @topEntity@ at the top.
genNetlist
  :: ClashEnv
  -> Bool
  -- ^ Whether this we're compiling a testbench (suppresses certain warnings)
  -> BindingMap
  -- ^ Global binders
  -> VarEnv TopEntityT
  -- ^ TopEntity annotations
  -> VarEnv Identifier
  -- ^ Top entity names
  -> (CustomReprs -> TyConMap -> Type ->
      State HWMap (Maybe (Either String FilteredHWType)))
  -- ^ Hardcoded Type -> HWType translator
  -> Bool
  -- ^ Whether the backend supports ifThenElse expressions
  -> SomeBackend
  -- ^ The current HDL backend
  -> IdentifierSet
  -- ^ Seen components
  -> FilePath
  -- ^ HDL dir
  -> Maybe StrictText.Text
  -- ^ Component name prefix
  -> Id
  -- ^ Name of the @topEntity@
  -> IO (Component, ComponentMap, IdentifierSet)
genNetlist env isTb globals tops topNames typeTrans ite be seen0 dir prefixM topEntity = do
  ((_meta, topComponent), s) <-
    runNetlistMonad env isTb globals tops typeTrans ite be seen1 dir componentNames_
      $ genComponent topEntity
  return (topComponent, _components s, seen1)
 where
  (componentNames_, seen1) =
    genNames (opt_newInlineStrat (envOpts env)) prefixM seen0 topNames globals

-- | Run a NetlistMonad action in a given environment
runNetlistMonad
  :: ClashEnv
  -> Bool
  -- ^ Whether this we're compiling a testbench (suppresses certain warnings)
  -> BindingMap
  -- ^ Global binders
  -> VarEnv TopEntityT
  -- ^ TopEntity annotations
  -> (CustomReprs -> TyConMap -> Type ->
      State HWMap (Maybe (Either String FilteredHWType)))
  -- ^ Hardcode Type -> HWType translator
  -> Bool
  -- ^ Whether the backend supports ifThenElse expressions
  -> SomeBackend
  -- ^ The current HDL backend
  -> IdentifierSet
  -- ^ Seen components
  -> FilePath
  -- ^ HDL dir
  -> VarEnv Identifier
  -- ^ Seen components
  -> NetlistMonad a
  -- ^ Action to run
  -> IO (a, NetlistState)
runNetlistMonad env isTb s tops typeTrans ite be seenIds_ dir componentNames_
  = flip runReaderT (NetlistEnv env "" "" Nothing)
  . flip runStateT s'
  . runNetlist
  where
    s' =
      NetlistState
        { _bindings=s
        , _components=OMap.empty
        , _typeTranslator=typeTrans
        , _curCompNm=(error "genComponent should have set _curCompNm", noSrcSpan)
        , _seenIds=seenIds_
        , _seenComps=seenIds_
        , _seenPrimitives=Set.empty
        , _componentNames=componentNames_
        , _topEntityAnns=tops
        , _hdlDir=dir
        , _curBBlvl=0
        , _isTestBench=isTb
        , _backEndITE=ite
        , _backend=be
        , _htyCache=mempty
        , _usages=mempty
        }

-- | Generate names for all binders in "BindingMap", except for the ones already
-- present in given identifier varenv.
genNames
  :: Bool
  -- ^ New inline strategy enabled?
  -> Maybe StrictText.Text
  -- ^ Prefix
  -> IdentifierSet
  -- ^ Identifier set to extend
  -> VarEnv Identifier
  -- ^ Pre-generated names
  -> BindingMap
  -> (VarEnv Identifier, IdentifierSet)
genNames newInlineStrat prefixM is env bndrs =
  runState (foldlM go env bndrs) is
 where
  go env_ (bindingId -> id_) =
    case lookupVarEnv id_ env_ of
      Just _ -> pure env_
      Nothing -> do
        nm <- Id.makeBasic (genComponentName newInlineStrat prefixM id_)
        pure (extendVarEnv id_ nm env_)

-- | Generate names for top entities. Should be executed at the very start of
-- the synthesis process and shared between all passes.
genTopNames
  :: ClashOpts
  -> HDL
  -- ^ HDL to generate identifiers for
  -> [TopEntityT]
  -> (VarEnv Identifier, IdentifierSet)
genTopNames opts hdl tops =
  -- TODO: Report error if fixed top entities have conflicting names
  flip runState (Id.emptyIdentifierSet esc lw hdl) $ do
    env0 <- foldlM goFixed emptyVarEnv fixedTops
    env1 <- foldlM goNonFixed env0 nonFixedTops
    pure env1
 where
  prefixM = opt_componentPrefix opts
  esc = opt_escapedIds opts
  lw = opt_lowerCaseBasicIds opts

  fixedTops = [(topId, ann) | TopEntityT{topId, topAnnotation=Just ann} <- tops]
  nonFixedTops = [topId | TopEntityT{topId, topAnnotation=Nothing} <- tops]

  goFixed env (topId, ann) = do
    topNm <- genTopName prefixM ann
    pure (extendVarEnv topId topNm env)

  goNonFixed env id_ = do
    topNm <- Id.makeBasic (genComponentName True prefixM id_)
    pure (extendVarEnv id_ topNm env)

-- | Generate a component for a given function (caching)
genComponent
  :: HasCallStack
  => Id
  -- ^ Name of the function
  -> NetlistMonad (ComponentMeta, Component)
genComponent compName = do
  compExprM <- lookupVarEnv compName <$> Lens.use bindings
  case compExprM of
    Nothing -> do
      (_,sp) <- Lens.use curCompNm
      throw (ClashException sp ($(curLoc) ++ "No normalized expression found for: " ++ show compName) Nothing)
    Just b -> do
      makeCachedO compName components $ genComponentT compName (bindingTerm b)

-- | Generate a component for a given function
genComponentT
  :: HasCallStack
  => Id
  -- ^ Name of the function
  -> Term
  -- ^ Corresponding term
  -> NetlistMonad (ComponentMeta, Component)
genComponentT compName0 componentExpr = do
  tcm <- Lens.view tcCache
  compName1 <- (`lookupVarEnv'` compName0) <$> Lens.use componentNames
  sp <- (bindingLoc . (`lookupVarEnv'` compName0)) <$> Lens.use bindings
  curCompNm .= (compName1, sp)
  usages .= mempty

  topEntityTM <- lookupVarEnv compName0 <$> Lens.use topEntityAnns
  let topAnnMM = topAnnotation <$> topEntityTM
      topVarTypeM = snd . splitCoreFunForallTy tcm . coreTypeOf . topId <$> topEntityTM

  seenIds <~ Lens.use seenComps
  (wereVoids,compInps,argWrappers,compOutps,resUnwrappers,binders,resultM) <-
    case splitNormalized tcm componentExpr of
      Right (args, binds, res) -> do
        let varType1 = fromMaybe (coreTypeOf res) topVarTypeM
        mkUniqueNormalized
          emptyInScopeSet
          topAnnMM
          -- HACK: Determine resulttype of this function by looking at its definition
          -- instead of looking at its last binder (which obscures any attributes
          -- [see: Clash.Annotations.SynthesisAttributes]).
          ((args, binds, res{varType=varType1}))
      Left err ->
        throw (ClashException sp ($curLoc ++ err) Nothing)

  netDecls <- concatMapM mkNetDecl (filter (maybe (const True) (/=) resultM . fst) binders)
  decls    <- concat <$> mapM (uncurry mkDeclarations) binders

  case resultM of
    Just result -> do
      [NetDecl' _ _ _ rIM] <- case filter ((==result) . fst) binders of
        b:_ -> mkNetDecl b
        _ -> error "internal error: couldn't find result binder"

      u <- Lens.use usages
      let useOf i = fromMaybe Cont $ lookupUsage (fst i) u

      let (compOutps',resUnwrappers') = case compOutps of
            [oport] -> ([(useOf oport,oport,rIM)],resUnwrappers)
            _ -> case resUnwrappers of
              NetDecl n res resTy:_ ->
                (map (\op -> (useOf op,op,Nothing)) compOutps
                ,NetDecl' n res resTy Nothing:drop 1 resUnwrappers
                )
              _ -> error "internal error: insufficient resUnwrappers"
          component      = Component compName1 compInps compOutps'
                             (netDecls ++ argWrappers ++ decls ++ resUnwrappers')
      ids <- Lens.use seenIds
      return (ComponentMeta wereVoids sp ids u, component)
    -- No result declaration means that the result is empty, this only happens
    -- when the TopEntity has an empty result. We just create an empty component
    -- in this case.
    Nothing -> do
      let component = Component compName1 compInps [] (netDecls ++ argWrappers ++ decls)
      ids <- Lens.use seenIds
      u <- Lens.use usages
      return (ComponentMeta wereVoids sp ids u, component)

mkNetDecl :: (Id, Term) -> NetlistMonad [Declaration]
mkNetDecl (id_,tm) = preserveVarEnv $ do
  hwTy <- unsafeCoreTypeToHWTypeM' $(curLoc) (coreTypeOf id_)

  if | not (shouldRenderDecl hwTy tm) -> return []
     | (Prim pInfo@PrimInfo{primMultiResult=MultiResult}, args) <- collectArgs tm ->
          multiDecls pInfo args
     | otherwise -> pure <$> singleDecl hwTy

  where
    multiDecls pInfo args0 = do
      tcm <- Lens.view tcCache
      resInits0 <- getResInits (id_, tm)
      let
        resInits1 = map Just resInits0 <> repeat Nothing
        mpInfo = multiPrimInfo' tcm pInfo
        (_, res) = splitMultiPrimArgs mpInfo args0

        netdecl i typ resInit =
          NetDecl' srcNote (Id.unsafeFromCoreId i) typ resInit

      hwTys <- mapM (unsafeCoreTypeToHWTypeM' $(curLoc)) (mpi_resultTypes mpInfo)
      pure (zipWith3 netdecl res hwTys resInits1)

    singleDecl hwTy = do
      rIM <- listToMaybe <$> getResInits (id_, tm)
      return (NetDecl' srcNote (Id.unsafeFromCoreId id_) hwTy rIM)

    addSrcNote loc
      | isGoodSrcSpan loc = Just (StrictText.pack (showSDocUnsafe (ppr loc)))
      | otherwise = Nothing

    srcNote = addSrcNote $ case tm of
      Tick (SrcSpan s) _ -> s
      _ -> nameLoc (varName id_)

    isMultiPrimSelect :: Term -> Bool
    isMultiPrimSelect t = case collectArgs t of
      (Prim (primName -> "c$multiPrimSelect"), _) -> True
      _ -> False

    shouldRenderDecl :: HWType -> Term -> Bool
    shouldRenderDecl ty t
      | isVoid ty = False
      | isMultiPrimSelect t = False
      | otherwise = True

    -- Set the initialization value of a signal when a primitive wants to set it
    getResInits :: (Id, Term) -> NetlistMonad [Expr]
    getResInits (i,collectArgsTicks -> (k,args0,ticks)) = case k of
      Prim p -> extractPrimWarnOrFail (primName p) >>= go p
      _ -> return []
     where
      go pInfo (BlackBox {resultInits=nmDs, multiResult=True}) = withTicks ticks $ \_ -> do
        tcm <- Lens.view tcCache
        let (args1, res) = splitMultiPrimArgs (multiPrimInfo' tcm pInfo) args0
        (bbCtx, _) <- mkBlackBoxContext (primName pInfo) Concurrent res args1
        mapM (go' (primName pInfo) bbCtx) nmDs
      go pInfo (BlackBox {resultInits=nmDs}) = withTicks ticks $ \_ -> do
        (bbCtx, _) <- mkBlackBoxContext (primName pInfo) Concurrent [i] args0
        mapM (go' (primName pInfo) bbCtx) nmDs
      go _ _ = pure []

      go' pNm bbCtx nmD = do
        (bbTempl, templDecl) <- prepareBlackBox pNm nmD bbCtx
        case templDecl of
          [] ->
            return (BlackBoxE pNm [] [] [] bbTempl bbCtx False)
          _  -> do
            (_,sloc) <- Lens.use curCompNm
            throw (ClashException sloc [I.i|
              Initial values cannot produce declarations, but saw:

                #{templDecl}

              after rendering initial values for blackbox:

                #{pNm}

              Given template:

                #{nmD}
            |] Nothing)

-- | Generate a list of concurrent Declarations for a let-binder, return an
-- empty list if the bound expression is represented by 0 bits
mkDeclarations
  :: HasCallStack
  => Id
  -- ^ LHS of the let-binder
  -> Term
  -- ^ RHS of the let-binder
  -> NetlistMonad [Declaration]
mkDeclarations = mkDeclarations' Concurrent

-- | Generate a list of Declarations for a let-binder, return an empty list if
-- the bound expression is represented by 0 bits
mkDeclarations'
  :: HasCallStack
  => DeclarationType
  -- ^ Concurrent of sequential declaration
  -> Id
  -- ^ LHS of the let-binder
  -> Term
  -- ^ RHS of the let-binder
  -> NetlistMonad [Declaration]
mkDeclarations' declType bndr (collectTicks -> (Var v,ticks)) =
  withTicks ticks (mkFunApp declType (Id.unsafeFromCoreId bndr) v [])

mkDeclarations' _declType _bndr e@(collectTicks -> (Case _ _ [],_)) = do
  (_,sp) <- Lens.use curCompNm
  throw $ ClashException
          sp
          ( unwords [ $(curLoc)
                    , "Not in normal form: Case-decompositions with an"
                    , "empty list of alternatives not supported:\n\n"
                    , showPpr e
                    ])
          Nothing

mkDeclarations' declType bndr (collectTicks -> (Case scrut altTy (alt:alts@(_:_)),ticks)) =
  withTicks ticks (mkSelection declType (CoreId bndr) scrut altTy (alt :| alts))

mkDeclarations' declType bndr app = do
  let (appF,args0,ticks) = collectArgsTicks app
      (args,tyArgs) = partitionEithers args0
  case appF of
    Var f
      | null tyArgs ->
        withTicks ticks (mkFunApp declType (Id.unsafeFromCoreId bndr) f args)
      | otherwise   -> do
        (_,sp) <- Lens.use curCompNm
        throw (ClashException sp ($(curLoc) ++ "Not in normal form: Var-application with Type arguments:\n\n" ++ showPpr app) Nothing)
    _ -> do
      (exprApp,declsApp0) <- mkExpr False declType (CoreId bndr) app
      let dstId = Id.unsafeFromCoreId bndr
      assn  <- case exprApp of
                 Identifier _ Nothing ->
                   -- Supplied 'bndr' was used to assign a result to, so we
                   -- don't have to manually turn it into a declaration
                   pure []

                 Noop ->
                   -- Rendered expression rendered a "noop" - a list of
                   -- declarations without a result. Used for things like
                   -- mealy IO / inline assertions / multi result primitives.
                   pure []

                 _ -> do
                   -- Turn returned expression into declaration by assigning
                   -- it to 'dstId'
                   assn <- case declType of
                     Concurrent -> contAssign dstId exprApp
                     Sequential -> procAssign Blocking dstId exprApp
                   pure [assn]

      declsApp1 <- if null declsApp0
                   then withTicks ticks return
                   else pure declsApp0
      return (declsApp1 ++ assn)

-- | Generate a declaration that selects an alternative based on the value of
-- the scrutinee
mkSelection
  :: DeclarationType
  -> NetlistId
  -> Term
  -> Type
  -> NonEmpty Alt
  -> [Declaration]
  -> NetlistMonad [Declaration]
mkSelection declType bndr scrut altTy alts0 tickDecls = do
  let dstId = netlistId1 id Id.unsafeFromCoreId bndr
  tcm <- Lens.view tcCache
  let scrutTy = inferCoreTypeOf tcm scrut
  scrutHTy <- unsafeCoreTypeToHWTypeM' $(curLoc) scrutTy
  scrutId  <- Id.suffix dstId "selection"
  (_,sp) <- Lens.use curCompNm
  ite <- Lens.use backEndITE
  altHTy <- unsafeCoreTypeToHWTypeM' $(curLoc) altTy
  case iteAlts scrutHTy (NE.toList alts0) of
    Just (altT,altF)
      | ite
      , Concurrent <- declType
      -> do
      (scrutExpr,scrutDecls) <- case scrutHTy of
        SP {} -> first (mkScrutExpr sp scrutHTy (fst (NE.last alts0))) <$>
                   mkExpr True declType (NetlistId scrutId scrutTy) scrut
        _ -> mkExpr False declType (NetlistId scrutId scrutTy) scrut
      altTId <- Id.suffix dstId "sel_alt_t"
      altFId <- Id.suffix dstId "sel_alt_f"
      (altTExpr,altTDecls) <- mkExpr False declType (NetlistId altTId altTy) altT
      (altFExpr,altFDecls) <- mkExpr False declType (NetlistId altFId altTy) altF
      -- This logic (and the same logic a few lines below) is faulty in the
      -- sense that it won't generate "void decls" if the alternatives' type
      -- is void, but the type of the scrut isn't. Ideally, we'd like to pass
      -- a boolean to 'mkExpr' indicating that it should only render "void decls"
      -- but that it should skip any others.
      --
      -- TODO: Fix ^
      if | isVoid altHTy && isVoid scrutHTy
          -> return $! scrutDecls ++ altTDecls ++ altFDecls
         | isVoid altHTy
          -> return $! altTDecls ++ altFDecls
         | otherwise
          -> do dstAssign <- contAssign dstId (IfThenElse scrutExpr altTExpr altFExpr)
                return $! scrutDecls ++ altTDecls ++ altFDecls ++ tickDecls ++ [dstAssign]
    _ -> do
      reprs <- Lens.view customReprs
      let alts1 = (reorderDefault . reorderCustom tcm reprs scrutTy) alts0
      (scrutExpr,scrutDecls) <- first (mkScrutExpr sp scrutHTy (fst (NE.head alts1))) <$>
                                  mkExpr True declType (NetlistId scrutId scrutTy) scrut
      (exprs,altsDecls)      <- unzip <$> mapM (mkCondExpr scrutHTy) (NE.toList alts1)
      case declType of
        Sequential -> do
          -- Assign to the result in every branch
          (altNets,exprAlts) <- fmap unzip (zipWithM (altAssign dstId) exprs altsDecls)
          return $! scrutDecls ++ tickDecls ++ concat altNets ++
                    [Seq [Branch scrutExpr scrutHTy exprAlts]]
        Concurrent ->
          if | isVoid altHTy && isVoid scrutHTy
              -> return $! concat altsDecls ++ scrutDecls
             | isVoid altHTy
              -> return $! concat altsDecls
             | otherwise
              -> do assign <- condAssign dstId altHTy scrutExpr scrutHTy exprs
                    return $! scrutDecls ++ concat altsDecls ++ tickDecls ++ [assign]
 where
  mkCondExpr :: HWType -> (Pat,Term) -> NetlistMonad ((Maybe HW.Literal,Expr),[Declaration])
  mkCondExpr scrutHTy (pat,alt) = do
    altId <- Id.suffix (netlistId1 id Id.unsafeFromCoreId bndr) "sel_alt"
    (altExpr,altDecls) <- mkExpr False declType (NetlistId altId altTy) alt
    (,altDecls) <$> case pat of
      DefaultPat           -> return (Nothing,altExpr)
      DataPat dc _ _ -> return (Just (dcToLiteral scrutHTy (dcTag dc)),altExpr)
      LitPat  (IntegerLiteral i) -> return (Just (NumLit i),altExpr)
      LitPat  (IntLiteral i) -> return (Just (NumLit i), altExpr)
      LitPat  (WordLiteral w) -> return (Just (NumLit w), altExpr)
      LitPat  (CharLiteral c) -> return (Just (NumLit . toInteger $ ord c), altExpr)
      LitPat  (Int64Literal i) -> return (Just (NumLit i), altExpr)
      LitPat  (Word64Literal w) -> return (Just (NumLit w), altExpr)
#if MIN_VERSION_base(4,16,0)
      LitPat  (Int8Literal i) -> return (Just (NumLit i), altExpr)
      LitPat  (Int16Literal i) -> return (Just (NumLit i), altExpr)
      LitPat  (Int32Literal i) -> return (Just (NumLit i), altExpr)
      LitPat  (Word8Literal w) -> return (Just (NumLit w), altExpr)
      LitPat  (Word16Literal w) -> return (Just (NumLit w), altExpr)
      LitPat  (Word32Literal w) -> return (Just (NumLit w), altExpr)
#endif
      LitPat  (NaturalLiteral n) -> return (Just (NumLit n), altExpr)
      _  -> do
        (_,sp) <- Lens.use curCompNm
        throw (ClashException sp ($(curLoc) ++ "Not an integer literal in LitPat:\n\n" ++ showPpr pat) Nothing)

  mkScrutExpr :: SrcSpan -> HWType -> Pat -> Expr -> Expr
  mkScrutExpr sp scrutHTy pat scrutE = case pat of
    DataPat dc _ _ -> let modifier = Just (DC (scrutHTy,dcTag dc - 1))
                      in case scrutE of
                          Identifier scrutId Nothing -> Identifier scrutId modifier
                          _ -> throw (ClashException sp ($(curLoc) ++ "Not in normal form: Not a variable reference or primitive as subject of a case-statement:\n\n" ++ show scrutE) Nothing)
    _ -> scrutE

  altAssign
    :: Identifier
    -> (Maybe HW.Literal,Expr)
    -> [Declaration]
    -> NetlistMonad ([Declaration],(Maybe HW.Literal,[Seq]))
  altAssign i (m,expr) ds = do
    let (nets,rest) = partition isNet ds
    assn <- case expr of
              Noop -> pure []
              _ -> do assn <- procAssign Blocking i expr
                      pure [assn]
    pure (nets,(m,map SeqDecl (rest ++ assn)))
   where
    isNet NetDecl' {} = True
    isNet _ = False

-- GHC puts default patterns in the first position, we want them in the
-- last position.
reorderDefault
  :: NonEmpty (Pat, Term)
  -> NonEmpty (Pat, Term)
reorderDefault ((DefaultPat,e) :| alts') =
  case alts' of
    [] -> (DefaultPat,e) :| []
    x:xs -> x :| (xs <> [(DefaultPat,e)])
reorderDefault alts' = alts'

reorderCustom
  :: TyConMap
  -> CustomReprs
  -> Type
  -> NonEmpty (Pat, Term)
  -> NonEmpty (Pat, Term)
reorderCustom tcm reprs (coreView1 tcm -> Just ty) alts =
  reorderCustom tcm reprs ty alts
reorderCustom _tcm reprs (coreToType' -> Right typeName) alts =
  case getDataRepr typeName reprs of
    Just (DataRepr' _name _size _constrReprs) ->
      NE.sortOn (patPos reprs . fst) alts
    Nothing ->
      alts
reorderCustom _tcm _reprs _type alts =
  alts

patPos
  :: CustomReprs
  -> Pat
  -> Int
patPos _reprs DefaultPat = -1
patPos _reprs (LitPat _) = 0
patPos reprs pat@(DataPat dataCon _ _) =
  -- We sort data patterns by their syntactical order
  let name = nameOcc $ dcName dataCon in
  case getConstrRepr name reprs of
    Nothing ->
      -- TODO: err
      error $ $(curLoc) ++ (show pat)
    Just (ConstrRepr' _name n _mask _value _anns) ->
      n


-- | Generate a list of Declarations for a let-binder where the RHS is a function application
mkFunApp
  :: HasCallStack
  => DeclarationType
  -> Identifier -- ^ LHS of the let-binder
  -> Id -- ^ Name of the applied function
  -> [Term] -- ^ Function arguments
  -> [Declaration] -- ^ Tick declarations
  -> NetlistMonad [Declaration]
mkFunApp declType dstId fun args tickDecls = do
  topAnns <- Lens.use topEntityAnns
  tcm     <- Lens.view tcCache
  case (isGlobalId fun, lookupVarEnv fun topAnns) of
    (True, Just topEntity)
      | let ty = coreTypeOf (topId topEntity)
      , let (fArgTys0,fResTy) = splitFunForallTy ty
      -- Take into account that clocks and stuff are split off from any product
      -- types containing them
      , let fArgTys1 = splitShouldSplit tcm $ rights fArgTys0
      , length fArgTys1 == length args
      -> do
        argHWTys <- mapM (unsafeCoreTypeToHWTypeM' $(curLoc)) fArgTys1
        (argExprs, concat -> argDecls) <- unzip <$>
          mapM (\(e,t) -> mkExpr False declType (NetlistId dstId t) e)
                                 (zip args fArgTys1)

        -- Filter void arguments, but make sure to render their declarations:
        let
          filteredTypeExprs = filter (not . isVoid . snd) (zip argExprs argHWTys)

        dstHWty  <- unsafeCoreTypeToHWTypeM' $(curLoc) fResTy

        -- TODO: The commented code fetches the function definition from the
        --       set of global bindings and uses it to replicate the port names
        --       of it. However, this does rely on the binding actually being
        --       present in the binding map. This isn't the case, as only
        --       the current top entity (and its dependencies, stopping at other
        --       top entities) are present. We can't add the non-normalized
        --       version, as this logic relies on 'splitArguments' having
        --       fired. Adding normalized versions would create a dependency
        --       between two top entities, defeating the ability to compile in
        --       parallel.
        --
        --       One option is to split the normalization process into two
        --       chunks: preprocessing (e.g., 'splitArguments') and actually
        --       normalizing. This would ensure only minimal work is being done
        --       serially.
        --
        --       The current workaround is to not rely on named arguments, using
        --       positional ones instead when instantiating a top entity.
        --
        -- funTerm <- fmap bindingTerm . lookupVarEnv fun <$> Lens.use bindings
        --
        -- expandedTopEntity <-
        --   case splitNormalized tcm <$> funTerm of
        --     Nothing -> error ("Internal error: could not find " <> show fun)
        --     Just (Left err) -> error ("Internal error: " <> show err)
        --     Just (Right (argIds, _binds, resId)) -> do
        --       argTys <- mapM (unsafeCoreTypeToHWTypeM $(curLoc)) (map coreTypeOf argIds)
        --       resTy <- unsafeCoreTypeToHWTypeM $(curLoc) (coreTypeOf resId)
        --       is <- Lens.use seenIds
        --       let topAnnM = topAnnotation topEntity
        --       pure (expandTopEntityOrErr is (zip argIds argTys) (resId, resTy) topAnnM)

        -- Generate ExpandedTopEntity, see TODO^
        argTys <- mapM (unsafeCoreTypeToHWTypeM $(curLoc) . inferCoreTypeOf tcm) args
        resTy <- unsafeCoreTypeToHWTypeM $(curLoc) fResTy
        let
          ettArgs = (Nothing,) <$> argTys
          ettRes = (Nothing, resTy)
        expandedTopEntity <-
            expandTopEntityOrErrM ettArgs ettRes (topAnnotation topEntity)

        instDecls <-
          mkTopUnWrapper
            fun expandedTopEntity (dstId, dstHWty)
            filteredTypeExprs tickDecls

        return (argDecls ++ instDecls)

      | otherwise -> error $ $(curLoc) ++ "under-applied TopEntity: " ++ showPpr fun
    (True, Nothing) -> do
      normalized <- Lens.use bindings
      case lookupVarEnv fun normalized of
        Nothing -> error [I.i|
          Internal error: unknown normalized binder:

            #{showPpr fun}
        |]
        Just (Binding{bindingTerm}) -> do
          (_, Component compName compInps co _) <- preserveVarEnv $ genComponent fun
          let argTys = map (inferCoreTypeOf tcm) args
          argHWTys <- mapM coreTypeToHWTypeM' argTys

          (argExprs, concat -> argDecls) <- unzip <$>
            mapM (\(e,t) -> mkExpr False declType (NetlistId dstId t) e)
                 (zip args argTys)

          -- Filter void arguments, but make sure to render their declarations:
          let
            argTypeExprs = zip argHWTys (zip argExprs argTys)
            filteredTypeExprs = fmap snd $ filter (not . isVoidMaybe True . fst) argTypeExprs

          let compOutp = (\(_,x,_) -> x) <$> listToMaybe co
          if length filteredTypeExprs == length compInps
            then do
              (argExprs',argDecls') <- (second concat . unzip) <$> mapM (toSimpleVar declType dstId) filteredTypeExprs
              let inpAssigns    = zipWith (\(i,t) e -> (Identifier i Nothing,In,t,e)) compInps argExprs'
                  outpAssign    = case compOutp of
                    Nothing -> []
                    Just (id_,hwtype) -> [(Identifier id_ Nothing,Out,hwtype,Identifier dstId Nothing)]
              let instLabel0 = StrictText.concat [Id.toText compName, "_", Id.toText dstId]
              instLabel1 <- fromMaybe instLabel0 <$> Lens.view setName
              instLabel2 <- affixName instLabel1
              instLabel3 <- Id.makeBasic instLabel2
              let portMap = NamedPortMap (outpAssign ++ inpAssigns)
                  instDecl = InstDecl Entity Nothing [] compName instLabel3 [] portMap
              declareInstUses outpAssign
              return (argDecls ++ argDecls' ++ tickDecls ++ [instDecl])
            else
              let
                argsFiltered :: [Expr]
                argsFiltered = map fst filteredTypeExprs
              in error [I.i|
              Under-applied normalized function at component #{compName}:

              #{showPpr fun}

              Core:

              #{showPpr bindingTerm}

              Applied to arguments:
              #{showPpr args}

              Applied to filtered arguments:
              #{argsFiltered}

              Component inputs:
              #{compInps}
            |]
    _ ->
      case args of
        [] -> do
          -- TODO: Figure out what to do with zero-width constructs
          assn <- contAssign dstId (Identifier (Id.unsafeFromCoreId fun) Nothing)
          pure [assn]
        _ -> error [I.i|
          Netlist generation encountered a local function. This should not
          happen. Function:

            #{showPpr fun}

          Arguments:

            #{showPpr args}

          Posssible user issues:

            * A top entity has an higher-order argument, e.g (Int -> Int) or
            Maybe (Int -> Int)

          Possible internal compiler issues:

            * 'bindOrLiftNonRep' failed to fire

            * 'caseCon' failed to eliminate something of a type such as
            "Maybe (Int -> Int)"
          |]

toSimpleVar :: DeclarationType
            -> Identifier
            -> (Expr,Type)
            -> NetlistMonad (Expr,[Declaration])
toSimpleVar _ _ (e@(Identifier _ Nothing),_) = return (e,[])
toSimpleVar declType dstId (e,ty) = do
  argNm <- Id.suffix dstId "fun_arg"
  hTy <- unsafeCoreTypeToHWTypeM' $(curLoc) ty
  let assignTy = declTypeUsage declType
  argDecl <- mkInit declType assignTy argNm hTy e
  return (Identifier argNm Nothing, argDecl)

-- | Generate an expression for a term occurring on the RHS of a let-binder
mkExpr :: HasCallStack
       => Bool -- ^ Treat BlackBox expression as declaration
       -> DeclarationType
       -- ^ Should the returned declarations be concurrent or sequential?
       -> NetlistId -- ^ Name hint for the id to (potentially) assign the result to
       -> Term -- ^ Term to convert to an expression
       -> NetlistMonad (Expr,[Declaration]) -- ^ Returned expression and a list of generate BlackBox declarations
mkExpr _ _ _ (stripTicks -> Core.Literal l) = do
  iw <- Lens.view intWidth
  return (mkLiteral iw l, [])

mkExpr bbEasD declType bndr app =
 let (appF,args,ticks) = collectArgsTicks app
     (tmArgs,tyArgs) = partitionEithers args
 in  withTicks ticks $ \tickDecls -> do
  hwTys  <- mapM (unsafeCoreTypeToHWTypeM' $(curLoc)) (netlistTypes bndr)
  (_,sp) <- Lens.use curCompNm
  let hwTyA = case hwTys of
        hwTy:_ -> hwTy
        _ -> error ("internal error: unable to extract sufficient hwTys from: " <> show bndr)
  let invalid kind = throw (ClashException sp ($(curLoc) ++ "Not in normal form: " ++ kind ++ "\n\n" ++ showPpr app) Nothing)
  case appF of
    Data dc -> mkDcApplication declType hwTys bndr dc tmArgs
    Prim pInfo -> mkPrimitive False bbEasD declType bndr pInfo args tickDecls
    Var f
      | null tmArgs ->
          if isVoid hwTyA then
            return (Noop, [])
          else do
            return (Identifier (Id.unsafeFromCoreId f) Nothing, [])
      | not (null tyArgs) -> invalid "Var-application with type arguments"
      | otherwise -> do
          argNm <- Id.suffix (netlistId1 id Id.unsafeFromCoreId bndr) "fun_arg"
          decls  <- mkFunApp declType argNm f tmArgs tickDecls
          if isVoid hwTyA then
            return (Noop, decls)
          else
            -- This net was already declared in the call to mkSelection.
            return ( Identifier argNm Nothing
                   , NetDecl Nothing argNm hwTyA : decls)
    Case scrut ty' [alt] -> mkProjection declType bbEasD bndr scrut ty' alt
    Case scrut tyA (alt:alts) -> do
      argNm <- Id.suffix (netlistId1 id Id.unsafeFromCoreId bndr) "sel_arg"
      decls  <- mkSelection declType (NetlistId argNm (netlistTypes1 bndr))
                            scrut tyA (alt :| alts) tickDecls
      if isVoid hwTyA then
        return (Noop, decls)
      else
        -- This net was already declared in the call to mkSelection
        return ( Identifier argNm Nothing
               , NetDecl' Nothing argNm hwTyA Nothing:decls)
    Case _ _ [] -> invalid "No case alternatives\n\n"
    Letrec binders body -> do
      netDecls <- concatMapM mkNetDecl binders
      decls    <- concatMapM (uncurry (mkDeclarations' declType)) binders
      (bodyE,bodyDecls) <- mkExpr bbEasD declType bndr (mkApps (mkTicks body ticks) args)
      return (bodyE,netDecls ++ decls ++ bodyDecls)
    Core.Literal _ -> invalid "application of literal"
    Let _ _ -> invalid "application of let"
    TyApp _ _ -> invalid "application of type application"
    Tick _ _ -> invalid "application of tick"
    Cast _ _ _ -> invalid "application of cast"
    Lam _ _ -> invalid "application of lambda"
    TyLam _ _ -> invalid "application of type lambda"
    App _ _ -> invalid "application of application"

-- | Generate an expression that projects a field out of a data-constructor.
--
-- Works for both product types, as sum-of-product types.
mkProjection
  :: DeclarationType
  -> Bool
  -- ^ Projection must bind to a simple variable
  -> NetlistId
  -- ^ Name hint for the signal to which the projection is (potentially) assigned
  -> Term
  -- ^ The subject/scrutinee of the projection
  -> Type
  -- ^ The type of the result
  -> Alt
  -- ^ The field to be projected
  -> NetlistMonad (Expr, [Declaration])
mkProjection declType mkDec bndr scrut altTy alt@(pat,v) = do
  tcm <- Lens.view tcCache
  let assignTy = declTypeUsage declType
  let scrutTy = inferCoreTypeOf tcm scrut
      e = Case scrut scrutTy [alt]
  (_,sp) <- Lens.use curCompNm
  varTm <- case v of
    (Var n) -> return n
    _ -> throw (ClashException sp ($(curLoc) ++
                "Not in normal form: RHS of case-projection is not a variable:\n\n"
                 ++ showPpr e) Nothing)
  sHwTy <- unsafeCoreTypeToHWTypeM' $(curLoc) scrutTy
  vHwTy <- unsafeCoreTypeToHWTypeM' $(curLoc) altTy
  scrutRendered <- do
    scrutNm <-
      netlistId1
        Id.next
        (\b -> Id.suffix (Id.unsafeFromCoreId b) "projection")
        bndr
    (scrutExpr,newDecls) <- mkExpr False declType (NetlistId scrutNm scrutTy) scrut
    case scrutExpr of
      Identifier newId modM ->
        pure (Right (newId, modM, newDecls))
      Noop ->
        -- Scrutinee was a zero-width / void construct. We need to render its
        -- declarations, but it's no use assigning it to a new variable.
        -- TODO: Figure out whether we need to render alternatives too.
        -- TODO: seems useless?
        pure (Left newDecls)
      _ -> do
        scrutDecl <- mkInit declType assignTy scrutNm sHwTy scrutExpr
        pure (Right (scrutNm, Nothing, newDecls ++ scrutDecl))

  case scrutRendered of
    Left newDecls -> pure (Noop, newDecls)
    Right (selId, modM, decls) -> do
      let altVarId = Id.unsafeFromCoreId varTm
      modifier <- case pat of
        DataPat dc exts tms -> do
          let
            tms' =
              if bindsExistentials exts tms then
                throw (ClashException sp ($(curLoc)
                  ++ "Not in normal form: Pattern binds existential variables:\n\n"
                  ++ showPpr e) Nothing)
              else
                tms
          argHWTys <- mapM coreTypeToHWTypeM' (map coreTypeOf tms)
          let tmsBundled   = zip argHWTys tms'
              tmsFiltered  = filter (maybe False (not . isVoid) . fst) tmsBundled
              tmsFiltered' = map snd tmsFiltered
          case elemIndex varTm {varType = altTy} tmsFiltered' of
            Nothing -> pure Nothing
            Just fI
              | sHwTy /= vHwTy ->
                pure $ nestModifier modM (Just (Indexed (sHwTy,dcTag dc - 1,fI)))
              -- When element and subject have the same HW-type,
              -- then the projections is just the identity
              | otherwise ->
                pure $ nestModifier modM (Just (DC (Void Nothing,0)))
        _ -> throw (ClashException sp ($(curLoc)
               ++ "Not in normal form: Unexpected pattern in case-projection:\n\n"
               ++ showPpr e) Nothing)
      let extractExpr = Identifier (maybe altVarId (const selId) modifier) modifier
      case bndr of
        NetlistId scrutNm _ | mkDec -> do
          scrutNm' <- Id.next scrutNm
          scrutDecl <- mkInit declType assignTy scrutNm' vHwTy extractExpr
          return (Identifier scrutNm' Nothing, scrutDecl ++ decls)
        MultiId {} -> error "mkProjection: MultiId"
        _ -> return (extractExpr,decls)
  where
    nestModifier Nothing  m          = m
    nestModifier m Nothing           = m
    nestModifier (Just m1) (Just m2) = Just (Nested m1 m2)

-- | Generate an expression for a DataCon application occurring on the RHS of a let-binder
mkDcApplication
    :: HasCallStack
    => DeclarationType
    -> [HWType]
    -- ^ HWType of the LHS of the let-binder, can multiple types when we're
    -- creating a "split" product type (e.g. a tuple of a Clock and Reset)
    -> NetlistId
    -- ^ Name hint for result id
    -> DataCon
    -- ^ Applied DataCon
    -> [Term]
    -- ^ DataCon Arguments
    -> NetlistMonad (Expr,[Declaration])
    -- ^ Returned expression and a list of generate BlackBox declarations
mkDcApplication declType [dstHType] bndr dc args = do
  let dcNm = nameOcc (dcName dc)
  tcm <- Lens.view tcCache
  let argTys = map (inferCoreTypeOf tcm) args
  argNm <- netlistId1 return (\b -> Id.suffix (Id.unsafeFromCoreId b) "dc_arg") bndr
  argHWTys <- mapM coreTypeToHWTypeM' argTys

  (argExprs, concat -> argDecls) <- unzip <$>
    mapM (\(e,t) -> mkExpr False declType (NetlistId argNm t) e) (zip args argTys)

  -- Filter void arguments, but make sure to render their declarations:
  let
    filteredTypeExprDecls =
      filter
        (not . isVoidMaybe True . fst)
        (zip argHWTys argExprs)

    (hWTysFiltered, argExprsFiltered) = unzip filteredTypeExprDecls

  fmap (,argDecls) $! case (hWTysFiltered,argExprsFiltered) of
    -- Is the DC just a newtype wrapper?
    ([Just argHwTy],[argExpr]) | argHwTy == dstHType ->
      return (HW.DataCon dstHType (DC (Void Nothing,-1)) [argExpr])
    _ -> case dstHType of
      SP _ dcArgPairs -> do
        let dcI      = dcTag dc - 1
            dcArgs   = snd $ indexNote ($(curLoc) ++ "No DC with tag: " ++ show dcI) dcArgPairs dcI
        case compare (length dcArgs) (length argExprsFiltered) of
          EQ -> return (HW.DataCon dstHType (DC (dstHType,dcI)) argExprsFiltered)
          LT -> error $ $(curLoc) ++ "Over-applied constructor: " ++ StrictText.unpack dcNm
          GT -> error $ $(curLoc) ++ "Under-applied constructor: " ++ StrictText.unpack dcNm
      Product _ _ dcArgs ->
        case compare (length dcArgs) (length argExprsFiltered) of
          EQ -> return (HW.DataCon dstHType (DC (dstHType,0)) argExprsFiltered)
          LT -> error $ $(curLoc) ++ "Over-applied constructor: " ++ StrictText.unpack dcNm
          GT -> error $ unlines [ $(curLoc) ++ "Under-applied constructor:" ++ StrictText.unpack dcNm
                                , "dcArgs=" ++ unlines [" - " ++ show x | x <- dcArgs]
                                , "argExprs=" ++ unlines [" - " ++ show x | x <- argExprs]
                                , "hWTysFilt=" ++ unlines [" - " ++ show x | x <- hWTysFiltered]
                                , "argExprsFilt=" ++ unlines [" - " ++ show x | x <- argExprsFiltered]
                                ]
      CustomProduct _ _ _ _ dcArgs ->
        case compare (length dcArgs) (length argExprsFiltered) of
          EQ -> return (HW.DataCon dstHType (DC (dstHType,0)) argExprsFiltered)
          LT -> error $ $(curLoc) ++ "Over-applied constructor: " ++ StrictText.unpack dcNm
          GT -> error $ $(curLoc) ++ "Under-applied constructor: " ++ StrictText.unpack dcNm
      Sum _ _ ->
        return (HW.DataCon dstHType (DC (dstHType,dcTag dc - 1)) [])
      CustomSP _ _ _ dcArgsTups -> do
        -- Safely get item from list, or err with note
        let dcI    = dcTag dc - 1
        let note   = $(curLoc) ++ "No DC with tag: " ++ show dcI
        let argTup = indexNote note dcArgsTups dcI
        let (_, _, dcArgs) = argTup

        case compare (length dcArgs) (length argExprsFiltered) of
          EQ -> return (HW.DataCon dstHType (DC (dstHType, dcI)) argExprsFiltered)
          LT -> error $ $(curLoc) ++ "Over-applied constructor: " ++ StrictText.unpack dcNm
          GT -> error $ $(curLoc) ++ "Under-applied constructor: " ++ StrictText.unpack dcNm

      CustomSum _ _ _ _ ->
        return (HW.DataCon dstHType (DC (dstHType, dcTag dc - 1)) [])
      Enable _ ->
        case argExprsFiltered of
          [x] -> return (HW.DataCon dstHType (DC (dstHType,dcTag dc - 1)) [x])
          _   -> error $ $(curLoc) ++ "unexpected arguments to Enable: " ++ show argExprsFiltered
      Bool ->
        let dc' = case dcTag dc of
                   1  -> HW.Literal Nothing (BoolLit False)
                   2  -> HW.Literal Nothing (BoolLit True)
                   tg -> error $ $(curLoc) ++ "unknown bool literal: " ++ showPpr dc ++ "(tag: " ++ show tg ++ ")"
        in  return dc'
      Vector 0 _ -> return (HW.DataCon dstHType VecAppend [])
      Vector 1 _ -> case argExprsFiltered of
                      [e] -> return (HW.DataCon dstHType VecAppend [e])
                      _ -> error $ $(curLoc) ++ "Unexpected number of arguments for `Cons`: " ++ showPpr args
      Vector _ _ -> case argExprsFiltered of
                      [e1,e2] -> return (HW.DataCon dstHType VecAppend [e1,e2])
                      _ -> error $ $(curLoc) ++ "Unexpected number of arguments for `Cons`: " ++ showPpr args
      MemBlob _ _ ->
        case compare 6 (length argExprsFiltered) of
          EQ -> return (HW.DataCon dstHType (DC (dstHType,0)) argExprsFiltered)
          LT -> error $ $(curLoc) ++ "Over-applied constructor"
          GT -> error $ $(curLoc) ++ "Under-applied constructor"
      RTree 0 _ -> case argExprsFiltered of
                      [e] -> return (HW.DataCon dstHType RTreeAppend [e])
                      _ -> error $ $(curLoc) ++ "Unexpected number of arguments for `LR`: " ++ showPpr args
      RTree _ _ -> case argExprsFiltered of
                      [e1,e2] -> return (HW.DataCon dstHType RTreeAppend [e1,e2])
                      _ -> error $ $(curLoc) ++ "Unexpected number of arguments for `BR`: " ++ showPpr args
      String ->
        let dc' = case dcTag dc of
                    1 -> HW.Literal Nothing (StringLit "")
                    _ -> error $ $(curLoc) ++ "mkDcApplication undefined for: " ++ show (dstHType,dc,dcTag dc,args,argHWTys)
        in  return dc'
      Void {} -> return Noop
      Signed _
#if MIN_VERSION_base(4,15,0)
        | dcNm == "GHC.Num.Integer.IS"
#else
        | dcNm == "GHC.Integer.Type.S#"
#endif
        , (a:_) <- argExprsFiltered
        -> pure a
        -- ByteArray# are non-translatable / void, except when they're literals
#if MIN_VERSION_base(4,15,0)
        | dcNm == "GHC.Num.Integer.IP"
#else
        | dcNm == "GHC.Integer.Type.Jp#"
#endif
        , (a@(HW.Literal Nothing (NumLit _)):_) <- argExprs
        -> pure a
#if MIN_VERSION_base(4,15,0)
        | dcNm == "GHC.Num.Integer.IN"
#else
        | dcNm == "GHC.Integer.Type.Jn#"
#endif
        -- ByteArray# are non-translatable / void, except when they're literals
        , (HW.Literal Nothing (NumLit i):_) <- argExprs
        -> pure (HW.Literal Nothing (NumLit (negate i)))
      Unsigned _
#if MIN_VERSION_base(4,15,0)
        | dcNm == "GHC.Num.Natural.NS"
#else
        | dcNm == "GHC.Natural.NatS#"
#endif
        , (a:_) <- argExprsFiltered
        -> pure a
#if MIN_VERSION_base(4,15,0)
        | dcNm == "GHC.Num.Natural.NB"
#else
        | dcNm == "GHC.Natural.NatJ#"
#endif
        -- ByteArray# are non-translatable / void, except when they're literals
        , (a@(HW.Literal Nothing (NumLit _)):_) <- argExprs
        -> pure a
      _ ->
        error $ $(curLoc) ++ "mkDcApplication undefined for: " ++ show (dstHType,dc,args,argHWTys)

-- Handle MultiId assignment
mkDcApplication declType dstHTypes (MultiId argNms) _ args = do
  tcm                 <- Lens.view tcCache
  let argTys          = map (inferCoreTypeOf tcm) args
  argHWTys            <- mapM coreTypeToHWTypeM' argTys
  -- Filter out the arguments of hwtype `Void` and only translate
  -- them to the intermediate HDL afterwards
  let argsBundled   = zip argHWTys (zipEqual (map CoreId argNms) args)
      (_hWTysFiltered,argsFiltered) = unzip
        (filter (maybe True (not . isVoid) . fst) argsBundled)
  (argExprs,argDecls) <- fmap (second concat . unzip) $!
                         mapM (uncurry (mkExpr False declType)) argsFiltered
  if length dstHTypes == length argExprs then do
    assns <- mapMaybeM
                  (\case (_,Noop) -> pure Nothing
                         (dstId,e) -> let nm = netlistId1 id Id.unsafeFromCoreId dstId
                                       in case e of
                                            Identifier nm0 Nothing
                                              | nm == nm0 -> pure Nothing
                                            _ -> Just <$> case declType of
                                                            Concurrent -> contAssign nm e
                                                            Sequential -> procAssign Blocking nm e)
                  (zipEqual (map CoreId argNms) argExprs)
    return (Noop,argDecls ++ assns)
  else
    error "internal error"

mkDcApplication _ _ _ _ _ = error "internal error"
