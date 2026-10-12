{-|
  Copyright  :  (C) 2026, QBayLogic B.V.
  License    :  BSD2 (see the file LICENSE)
  Maintainer :  QBayLogic B.V. <devops@qbaylogic.com>

  Create netlists out of normalized terms of scoped Clash Core.

  'genComponentT' opens the normalized term of a component into a single scope
  ('Clash.Netlist.Scoped.Util.splitNormalized'), and records the HDL
  identifiers of its variables in a t'CompCtx'
  ('Clash.Netlist.Scoped.Util.mkUniqueNormalized'). The handlers below get
  that context.

  'mkDeclarations'' and 'mkExpr' dispatch on the form of a term. The other
  handlers are adapters that run their counterparts in "Clash.Netlist" on
  Clash Core, see 'Clash.Netlist.Scoped.Types.toLegacy'. TODO(Phase 5,
  Netlist): port them. Their signatures are fixed in @Clash.Netlist.Scoped.hs-boot@.
-}

{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}

module Clash.Netlist.Scoped
  ( -- * Netlists
    genNetlist
  , genComponent
  , genComponentT
    -- * Handlers
  , mkDeclarations
  , mkDeclarations'
  , mkExpr
  , mkNetDecl
  , mkFunApp
  , mkSelection
  , mkProjection
  , mkDcApplication
  ) where

import           Control.Exception                (throw)
import           Control.Lens                     ((.=), (<~))
import qualified Control.Lens                     as Lens
import           Control.Monad                    (zipWithM)
import           Control.Monad.Extra              (concatMapM, mapMaybeM)
import           Control.Monad.IO.Class           (liftIO)
import           Control.Monad.State.Strict       (State)
import           Data.Bifunctor                   (first, second)
import           Data.Either                      (lefts, partitionEithers)
import           Data.Char                        (ord)
import           Data.List                        (elemIndex, partition)
import           Data.List.Extra                  (zipEqual)
import           Data.List.NonEmpty               (NonEmpty (..))
import qualified Data.List.NonEmpty.Extra         as NE
import           Data.Maybe                       (fromMaybe)
import qualified Data.Text                        as StrictText
import           Data.Text.Extra                  (showt)
import           GHC.Num.Integer                  (Integer (..))
import           GHC.Num.Natural                  (Natural (..))
import           GHC.Stack                        (HasCallStack)
import qualified GHC.Types.SrcLoc                 as GHC (SrcSpan)

import           Clash.Annotations.BitRepresentation.Internal
  (ConstrRepr' (..), CustomReprs, DataRepr' (..), Type' (..), getConstrRepr, getDataRepr)
import           Clash.Core.DataCon               (DataCon (..))
import           Clash.Core.HasType               (coreTypeOf)
import           Clash.Core.Literal               (Literal (..))
import           Clash.Core.Pretty                (showPpr)
import qualified Clash.Core.Name                  as C (nameOcc)
import qualified Clash.Core.Term                  as C
import qualified Clash.Core.Type                  as C
import           Clash.Core.TyCon                 (TyConMap)
import qualified Clash.Core.Var                   as C
import           Clash.Driver.Types
  (Binding (..), ClashEnv (..), ClashOpts (..), GlobalBindings)
import           Clash.Netlist                    (genNames, runNetlistMonad)
import qualified Clash.Netlist                    as Legacy
import qualified Clash.Netlist.Id                 as Id
import qualified Clash.Netlist.Types              as HW (Expr (Literal), Literal)
import           Clash.Netlist.Types              as HW
  hiding (Literal, NetlistId (..), netlistId1, netlistTypes, netlistTypes1)
import           Clash.Netlist.Util
  (condAssign, contAssign, coreTypeToHWTypeM', dcToLiteral, isVoid, isVoidMaybe, mkInit, mkLiteral,
   procAssign)
import           Clash.Util                       (ClashException (..), curLoc, indexNote, makeCachedO)
import qualified Clash.Util.Supply                as Supply
import           Clash.Core.Evaluator.Types       (Evaluator)

import           Clash.Core.Scoped.Alpha          (aeqTy)
import           Clash.Core.Scoped.Convert        (toClashType)
import           Clash.Core.Scoped.HasType        (inferCoreTypeOf)
import           Clash.Core.Scoped.Type           (coreView)
import           Clash.Core.Scoped.TysPrim        (integerPrimTy, naturalPrimTy)
import           Clash.Core.Scoped.NameMap        (GlobalEnv, lookupGlobalEnv)
import           Clash.Core.Scoped.FreeVars       (anyBoundOccursInType, teleNameBinders)
import           Clash.Core.Scoped.Subst
  (openAlt, openBind, patVars, teleIdBinderTypes, teleIdVars)
import           Clash.Core.Scoped.Syntax
import           Clash.Core.Scoped.Term
  (collectArgsTicks, collectTicks, mkApps, mkTicks, stripTicks)

import           Clash.Netlist.Scoped.BlackBox    (mkPrimitive)
import           Clash.Netlist.Scoped.Types
import           Clash.Netlist.Scoped.Util
  (UniqueNormalized (..), mkUniqueNormalized, splitNormalized,
   unsafeCoreTypeToHWTypeM', withTicks)

-- | Generate a hierarchical netlist out of a set of global binders with
-- @topEntity@ at the top.
genNetlist
  :: ClashEnv
  -> Evaluator
  -- ^ The whnf evaluator, to evaluate the attributes of 'Attributes' ticks
  -> Bool
  -- ^ Whether this we're compiling a testbench (suppresses certain warnings)
  -> GlobalBindings
  -- ^ Global binders
  -> GlobalEnv TopEntityT
  -- ^ TopEntity annotations
  -> GlobalEnv Identifier
  -- ^ Top entity names
  -> (CustomReprs -> TyConMap -> C.Type ->
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
  -> C.Id
  -- ^ Name of the @topEntity@
  -> IO (Component, ComponentMap, IdentifierSet)
genNetlist env eval isTb globals tops topNames typeTrans ite be seen0 dir prefixM topEntity = do
  let (componentNames_, seen1) =
        genNames (opt_newInlineStrat (envOpts env)) prefixM seen0 topNames globals
  ((_meta, topComponent), s) <-
    runNetlistMonad env eval isTb globals tops typeTrans ite be seen1 dir componentNames_
      $ genComponent (mkGlobalId topEntity)
  return (topComponent, _components s, seen1)

-- | Generate a component for a given function (caching)
genComponent
  :: HasCallStack
  => GlobalId
  -- ^ Name of the function
  -> NetlistMonad (ComponentMeta, Component)
genComponent compName = do
  compExprM <- lookupGlobalEnv compName <$> Lens.use bindings
  case compExprM of
    Nothing -> do
      (_,sp) <- Lens.use curCompNm
      throw (ClashException sp ($(curLoc) ++ "No normalized expression found for: " ++ show (globalIdVar compName)) Nothing)
    Just b -> do
      makeCachedO (globalIdVar compName) components $ genComponentT compName (bindingTerm b)

-- | Generate a component for a given function
genComponentT
  :: HasCallStack
  => GlobalId
  -- ^ Name of the function
  -> Term 'VoidS
  -- ^ Corresponding term
  -> NetlistMonad (ComponentMeta, Component)
genComponentT compName0 componentExpr = do
  tcm <- Lens.view tcCache
  tenv <- Lens.view typeEnv
  let compVar = globalIdVar compName0
  compName1 <-
    fromMaybe (error ($(curLoc) ++ "No component name for: " ++ show compVar))
      . lookupGlobalEnv compName0 <$> Lens.use componentNames
  sp <- maybe (error ($(curLoc) ++ "No normalized expression found for: " ++ show compVar)) bindingLoc
          . lookupGlobalEnv compName0 <$> Lens.use bindings
  curCompNm .= (compName1, sp)
  usages .= mempty

  topEntityTM <- lookupGlobalEnv compName0 <$> Lens.use topEntityAnns
  let topAnnMM = topAnnotation <$> topEntityTM
      topVarTypeM = snd . C.splitCoreFunForallTy tcm . coreTypeOf . topId <$> topEntityTM

  seenIds <~ Lens.use seenComps
  supply <- liftIO Supply.newSupply
  UniqueNormalized ctx wereVoids compInps argWrappers compOutps resUnwrappers binders resultM <-
    case splitNormalized tenv componentExpr of
      Right split ->
        -- HACK: Determine resulttype of this function by looking at its definition
        -- instead of looking at its last binder (which obscures any attributes
        -- [see: Clash.Annotations.SynthesisAttributes]).
        mkUniqueNormalized tenv supply topAnnMM topVarTypeM split
      Left err ->
        throw (ClashException sp ($curLoc ++ err) Nothing)

  netDecls <- concatMapM (mkNetDecl ctx) (filter (maybe (const True) (/=) resultM . fst) binders)
  decls    <- concat <$> mapM (uncurry (mkDeclarations ctx)) binders

  case resultM of
    Just result -> do
      [NetDecl' _ _ _ rIM] <- case filter ((==result) . fst) binders of
        b:_ -> mkNetDecl ctx b
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

-- | Generate the declaration of the signal of a let-binder: see
-- 'Clash.Netlist.mkNetDecl'
mkNetDecl :: Distinct n => CompCtx n -> (LocalId n, Term n) -> NetlistMonad [Declaration]
mkNetDecl ctx (b, e) = Legacy.mkNetDecl (toLegacyLocalId ctx b, toLegacy ctx e)

-- | Generate a list of concurrent Declarations for a let-binder, return an
-- empty list if the bound expression is represented by 0 bits
mkDeclarations
  :: (HasCallStack, Distinct n)
  => CompCtx n
  -> LocalId n
  -- ^ LHS of the let-binder
  -> Term n
  -- ^ RHS of the let-binder
  -> NetlistMonad [Declaration]
mkDeclarations ctx = mkDeclarations' ctx Concurrent

-- | Generate a list of Declarations for a let-binder, return an empty list if
-- the bound expression is represented by 0 bits
mkDeclarations'
  :: (HasCallStack, Distinct n)
  => CompCtx n
  -> DeclarationType
  -- ^ Concurrent of sequential declaration
  -> LocalId n
  -- ^ LHS of the let-binder
  -> Term n
  -- ^ RHS of the let-binder
  -> NetlistMonad [Declaration]
mkDeclarations' ctx declType bndr (collectTicks -> (Var o ty v,ticks)) =
  withTicks ctx ticks (mkFunApp ctx declType (localIdentifier ctx (localIdName bndr)) (Left (LocalId o ty v)) [])

mkDeclarations' ctx declType bndr (collectTicks -> (GlobalVar g,ticks)) =
  withTicks ctx ticks (mkFunApp ctx declType (localIdentifier ctx (localIdName bndr)) (Right g) [])

mkDeclarations' ctx _declType _bndr e@(collectTicks -> (Case _ _ [],_)) = do
  (_,sp) <- Lens.use curCompNm
  throw $ ClashException
          sp
          ( unwords [ $(curLoc)
                    , "Not in normal form: Case-decompositions with an"
                    , "empty list of alternatives not supported:\n\n"
                    , showPpr (toLegacy ctx e)
                    ])
          Nothing

mkDeclarations' ctx declType bndr (collectTicks -> (Case scrut altTy (alt:alts@(_:_)),ticks)) =
  withTicks ctx ticks (mkSelection ctx declType (CoreId bndr) scrut altTy (alt :| alts))

mkDeclarations' ctx declType bndr app = do
  let (appF,args0,ticks) = collectArgsTicks app
      (args,tyArgs) = partitionEithers args0
      dstId = localIdentifier ctx (localIdName bndr)
      funApp f
        | null tyArgs =
          withTicks ctx ticks (mkFunApp ctx declType dstId f args)
        | otherwise = do
          (_,sp) <- Lens.use curCompNm
          throw (ClashException sp ($(curLoc) ++ "Not in normal form: Var-application with Type arguments:\n\n" ++ showPpr (toLegacy ctx app)) Nothing)
  case appF of
    Var o ty f -> funApp (Left (LocalId o ty f))
    GlobalVar g -> funApp (Right g)
    _ -> do
      (exprApp,declsApp0) <- mkExpr ctx False declType (CoreId bndr) app
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
                   then withTicks ctx ticks return
                   else pure declsApp0
      return (declsApp1 ++ assn)

-- | Generate an expression for a term occurring on the RHS of a let-binder
mkExpr
  :: (HasCallStack, Distinct n)
  => CompCtx n
  -> Bool
  -- ^ Treat BlackBox expression as declaration
  -> DeclarationType
  -- ^ Should the returned declarations be concurrent or sequential?
  -> NetlistId n
  -- ^ Name hint for the id to (potentially) assign the result to
  -> Term n
  -- ^ Term to convert to an expression
  -> NetlistMonad (Expr,[Declaration])
  -- ^ Returned expression and a list of generate BlackBox declarations
mkExpr _ _ _ _ (stripTicks -> Literal l) = do
  iw <- Lens.view intWidth
  return (mkLiteral iw l, [])

mkExpr ctx bbEasD declType bndr app =
 let (appF,args,ticks) = collectArgsTicks app
     (tmArgs,tyArgs) = partitionEithers args
 in  withTicks ctx ticks $ \tickDecls -> do
  hwTys  <- mapM (hwTypeOf $(curLoc)) (netlistTypes bndr)
  (_,sp) <- Lens.use curCompNm
  let hwTyA = case hwTys of
        hwTy:_ -> hwTy
        _ -> error ("internal error: unable to extract sufficient hwTys from: " <> show bndr)
  let invalid kind = throw (ClashException sp ($(curLoc) ++ "Not in normal form: " ++ kind ++ "\n\n" ++ showPpr (toLegacy ctx app)) Nothing)
      bndrIdentifier = netlistId1 id (localIdentifier ctx . localIdName) bndr
      funApp f fIdentifier
        | null tmArgs =
            if isVoid hwTyA then
              return (Noop, [])
            else do
              return (Identifier fIdentifier Nothing, [])
        | not (null tyArgs) = invalid "Var-application with type arguments"
        | otherwise = do
            argNm <- Id.suffix bndrIdentifier "fun_arg"
            decls  <- mkFunApp ctx declType argNm f tmArgs tickDecls
            if isVoid hwTyA then
              return (Noop, decls)
            else
              -- This net was already declared in the call to mkSelection.
              return ( Identifier argNm Nothing
                     , NetDecl Nothing argNm hwTyA : decls)
  case appF of
    Data dc -> mkDcApplication ctx declType hwTys bndr dc tmArgs
    Prim pInfo -> mkPrimitive ctx False bbEasD declType bndr pInfo args tickDecls
    Var o ty f -> funApp (Left (LocalId o ty f)) (localIdentifier ctx f)
    GlobalVar g -> funApp (Right g) (Id.unsafeFromCoreId (globalIdVar g))
    Case scrut ty' [alt] -> mkProjection ctx declType bbEasD bndr scrut ty' alt
    Case scrut tyA (alt:alts) -> do
      argNm <- Id.suffix bndrIdentifier "sel_arg"
      decls  <- mkSelection ctx declType (NetlistId argNm (netlistTypes1 bndr))
                            scrut tyA (alt :| alts) tickDecls
      if isVoid hwTyA then
        return (Noop, decls)
      else
        -- This net was already declared in the call to mkSelection
        return ( Identifier argNm Nothing
               , NetDecl' Nothing argNm hwTyA Nothing:decls)
    Case _ _ [] -> invalid "No case alternatives\n\n"
    Let bind body ->
      -- The binders keep their human readable names as HDL identifiers, as
      -- 'mkUniqueNormalized' only names those of the let-expression of the
      -- component.
      openBind (compScope ctx) bind body $ \sc bind' body' -> do
        let binders = case bind' of
              NonRec b x -> [(binderLocalId b, sink x)]
              Rec bs xs -> zip [ LocalId o ty n | Var o ty n <- teleIdVars bs ] xs
            ctx' = extendCompCtx sc
                     [ (b, Id.unsafeMake (C.nameOcc (localIdOcc b))) | (b, _) <- binders ]
                     ctx
        netDecls <- concatMapM (mkNetDecl ctx') binders
        decls    <- concatMapM (uncurry (mkDeclarations' ctx' declType)) binders
        (bodyE,bodyDecls) <-
          mkExpr ctx' bbEasD declType (sink bndr) (mkApps (mkTicks body' (sink1 ticks)) (sink1Args args))
        return (bodyE,netDecls ++ decls ++ bodyDecls)
    Literal _ -> invalid "application of literal"
    TyApp _ _ -> invalid "application of type application"
    Tick _ _ -> invalid "application of tick"
    Cast _ _ _ -> invalid "application of cast"
    Lam _ _ -> invalid "application of lambda"
    TyLam _ _ -> invalid "application of type lambda"
    App _ _ -> invalid "application of application"
 where
  sink1Args :: DExt x y => [Either (Term x) (Type x)] -> [Either (Term y) (Type y)]
  sink1Args = map (either (Left . sink) (Right . sink))

-- * Adapters

-- | Generate a list of Declarations for a let-binder where the RHS is a
-- function application: see 'Clash.Netlist.mkFunApp'
mkFunApp
  :: (HasCallStack, Distinct n)
  => CompCtx n
  -> DeclarationType
  -> Identifier
  -- ^ LHS of the let-binder
  -> Either (LocalId n) GlobalId
  -- ^ Name of the applied function: a local or a global variable
  -> [Term n]
  -- ^ Function arguments
  -> [Declaration]
  -- ^ Tick declarations
  -> NetlistMonad [Declaration]
mkFunApp ctx declType dstId fun args =
  Legacy.mkFunApp declType dstId
    (either (toLegacyLocalId ctx) globalIdVar fun)
    (lefts (toLegacyArgs ctx (map Left args)))

-- | Generate a declaration that selects an alternative based on the value of
-- the scrutinee
mkSelection
  :: forall n
   . Distinct n
  => CompCtx n
  -> DeclarationType
  -> NetlistId n
  -> Term n
  -> Type n
  -> NonEmpty (Alt n)
  -> [Declaration]
  -> NetlistMonad [Declaration]
mkSelection ctx declType bndr scrut altTy alts0 tickDecls = do
  let dstId = netlistIdentifier ctx bndr
      scrutTy = inferCoreTypeOf (compTypeEnv ctx) (compScope ctx) scrut
  scrutHTy <- hwTypeOf $(curLoc) scrutTy
  scrutId  <- Id.suffix dstId "selection"
  (_,sp) <- Lens.use curCompNm
  ite <- Lens.use backEndITE
  altHTy <- hwTypeOf $(curLoc) altTy
  let e = Case scrut altTy (NE.toList alts0)
  case iteAlts scrutHTy alts0 of
    Just (altT,altF)
      | ite
      , Concurrent <- declType
      -> do
      (scrutExpr,scrutDecls) <- case scrutHTy of
        SP {} -> first (mkScrutExpr sp scrutHTy (NE.last alts0)) <$>
                   mkExpr ctx True declType (NetlistId scrutId scrutTy) scrut
        _ -> mkExpr ctx False declType (NetlistId scrutId scrutTy) scrut
      altTId <- Id.suffix dstId "sel_alt_t"
      altFId <- Id.suffix dstId "sel_alt_f"
      (altTExpr,altTDecls) <- mkAltExpr altTId altT
      (altFExpr,altFDecls) <- mkAltExpr altFId altF
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
    _ | aeqTy scrutTy integerPrimTy
      -> throw (ClashException sp ($(curLoc) ++ "Can't create netlist for:  case (_ :: Integer) of ...") (Just $ showPpr (toLegacy ctx e)))
    _ | aeqTy scrutTy naturalPrimTy
      -> throw (ClashException sp ($(curLoc) ++ "Can't create netlist for:  case (_ :: Natural) of ...") (Just $ showPpr (toLegacy ctx e)))
    _ -> do
      reprs <- Lens.view customReprs
      let alts1 = (reorderDefault . reorderCustom ctx reprs scrutTy) alts0
      (scrutExpr,scrutDecls) <- first (mkScrutExpr sp scrutHTy (NE.head alts1)) <$>
                                  mkExpr ctx True declType (NetlistId scrutId scrutTy) scrut
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
  -- The expression of the right-hand side of an alternative
  mkAltExpr :: Identifier -> Alt n -> NetlistMonad (Expr, [Declaration])
  mkAltExpr altId alt = withAlt ctx alt $ \ctx' _ e ->
    mkExpr ctx' False declType (NetlistId altId (sink altTy)) e

  mkCondExpr :: HWType -> Alt n -> NetlistMonad ((Maybe HW.Literal,Expr),[Declaration])
  mkCondExpr scrutHTy alt@(Alt pat _) = do
    altId <- Id.suffix (netlistIdentifier ctx bndr) "sel_alt"
    (altExpr,altDecls) <- mkAltExpr altId alt
    (,altDecls) <$> case pat of
      DefaultPat           -> return (Nothing,altExpr)
      DataPat dc _ _ -> return (Just (dcToLiteral scrutHTy (dcTag dc)),altExpr)
      LitPat  (IntegerLiteral i) -> return (Just (NumLit i),altExpr)
      LitPat  (IntLiteral i) -> return (Just (NumLit i), altExpr)
      LitPat  (WordLiteral w) -> return (Just (NumLit w), altExpr)
      LitPat  (CharLiteral c) -> return (Just (NumLit . toInteger $ ord c), altExpr)
      LitPat  (Int64Literal i) -> return (Just (NumLit i), altExpr)
      LitPat  (Word64Literal w) -> return (Just (NumLit w), altExpr)
      LitPat  (Int8Literal i) -> return (Just (NumLit i), altExpr)
      LitPat  (Int16Literal i) -> return (Just (NumLit i), altExpr)
      LitPat  (Int32Literal i) -> return (Just (NumLit i), altExpr)
      LitPat  (Word8Literal w) -> return (Just (NumLit w), altExpr)
      LitPat  (Word16Literal w) -> return (Just (NumLit w), altExpr)
      LitPat  (Word32Literal w) -> return (Just (NumLit w), altExpr)
      LitPat  (NaturalLiteral n) -> return (Just (NumLit n), altExpr)
      LitPat l -> do
        (_,sp) <- Lens.use curCompNm
        throw (ClashException sp ($(curLoc) ++ "Not an integer literal in LitPat:\n\n" ++ showPpr (C.LitPat l)) Nothing)

  mkScrutExpr :: GHC.SrcSpan -> HWType -> Alt n -> Expr -> Expr
  mkScrutExpr sp scrutHTy (Alt pat _) scrutE = case pat of
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

-- | Attempt to make an if-then-else construction out of this Case, see
-- 'Clash.Netlist.Util.iteAlts'. Gives the alternatives for 'True' and 'False'.
iteAlts :: HWType -> NonEmpty (Alt n) -> Maybe (Alt n, Alt n)
iteAlts sHTy (alt0@(Alt pat0 _) :| [alt1@(Alt pat1 _)]) | validIteSTy sHTy = case pat0 of
  DataPat dc _ _ -> case dcTag dc of
    2 -> Just (alt0,alt1)
    _ -> Just (alt1,alt0)
  LitPat (IntegerLiteral l) -> case l of
    1 -> Just (alt0,alt1)
    _ -> Just (alt1,alt0)
  DefaultPat -> case pat1 of
    DataPat dc _ _ -> case dcTag dc of
      2 -> Just (alt1,alt0)
      _ -> Just (alt0,alt1)
    LitPat (IntegerLiteral l) -> case l of
      1 -> Just (alt1,alt0)
      _ -> Just (alt0,alt1)
    _ -> Nothing
  _ -> Nothing
 where
  validIteSTy Bool          = True
  validIteSTy Bit           = True
  validIteSTy (Sum _ [_,_]) = True
  validIteSTy (SP _ [_,_])  = True
  validIteSTy (Unsigned 1)  = True
  validIteSTy (Index 2)     = True
  validIteSTy _             = False

iteAlts _ _ = Nothing

-- GHC puts default patterns in the first position, we want them in the
-- last position.
reorderDefault :: NonEmpty (Alt n) -> NonEmpty (Alt n)
reorderDefault (alt@(Alt DefaultPat _) :| alts') =
  case alts' of
    [] -> alt :| []
    x:xs -> x :| (xs <> [alt])
reorderDefault alts' = alts'

-- | Order the alternatives of a type with a custom bit representation by the
-- syntactical order of their constructors
reorderCustom
  :: Distinct n
  => CompCtx n
  -> CustomReprs
  -> Type n
  -> NonEmpty (Alt n)
  -> NonEmpty (Alt n)
reorderCustom ctx reprs ty alts =
  case typeName (coreView (compTypeEnv ctx) (compScope ctx) ty) of
    Right nm | Just DataRepr' {} <- getDataRepr nm reprs ->
      NE.sortOn (patPos reprs) alts
    _ -> alts
 where
  -- See 'Clash.Annotations.BitRepresentation.ClashLib.coreToType''
  typeName :: Type n -> Either String Type'
  typeName = \case
    AppTy t1 t2 -> AppTy' <$> typeName t1 <*> typeName t2
    ConstTy (C.TyCon nm) -> Right (ConstTy' (C.nameOcc nm))
    LitTy (C.NumTy n) -> Right (LitTy' n)
    LitTy (C.SymTy lit) -> Right (SymLitTy' (StrictText.pack lit))
    t -> Left ($(curLoc) ++ "Unexpected type: " ++ show t)

patPos :: CustomReprs -> Alt n -> Int
patPos reprs (Alt pat _) = case pat of
  DefaultPat -> -1
  LitPat _ -> 0
  DataPat dataCon _ _ ->
    -- We sort data patterns by their syntactical order
    let name = C.nameOcc $ dcName dataCon in
    case getConstrRepr name reprs of
      Nothing ->
        -- TODO: err
        error $ $(curLoc) ++ show pat
      Just (ConstrRepr' _name n _mask _value _anns) ->
        n

-- | Generate an expression that projects a field out of a data-constructor.
--
-- Works for both product types, as sum-of-product types.
mkProjection
  :: Distinct n
  => CompCtx n
  -> DeclarationType
  -> Bool
  -- ^ Projection must bind to a simple variable
  -> NetlistId n
  -- ^ Name hint for the signal to which the projection is (potentially) assigned
  -> Term n
  -- ^ The subject/scrutinee of the projection
  -> Type n
  -- ^ The type of the result
  -> Alt n
  -- ^ The field to be projected
  -> NetlistMonad (Expr, [Declaration])
mkProjection ctx declType mkDec bndr scrut altTy alt = do
  let assignTy = declTypeUsage declType
      scrutTy = inferCoreTypeOf (compTypeEnv ctx) (compScope ctx) scrut
      e = Case scrut scrutTy [alt]
  (_,sp) <- Lens.use curCompNm
  withAlt ctx alt $ \ctxAlt pat v -> do
    varTm <- case v of
      Var _ _ n -> return n
      _ -> throw (ClashException sp ($(curLoc) ++
                  "Not in normal form: RHS of case-projection is not a variable:\n\n"
                   ++ showPpr (toLegacy ctx e)) Nothing)
    sHwTy <- hwTypeOf $(curLoc) scrutTy
    vHwTy <- hwTypeOf $(curLoc) altTy
    scrutRendered <- do
      scrutNm <-
        netlistId1
          Id.next
          (\b -> Id.suffix (localIdentifier ctx (localIdName b)) "projection")
          bndr
      (scrutExpr,newDecls) <- mkExpr ctx False declType (NetlistId scrutNm scrutTy) scrut
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
      -- The projected field is zero-width / void, so it is filtered out of the
      -- subject's fields below and there is nothing to select. Rendering it
      -- anyway would reference the pattern's binder, which is never declared.
      Right (_, _, decls) | isVoid vHwTy -> pure (Noop, decls)
      Right (selId, modM, decls) -> do
        let altVarId = localIdentifier ctxAlt varTm
        modifier <- case pat of
          DataPat dc exts tms -> do
            let
              fields = [ (n, ty) | Var _ ty n <- snd (patVars pat) ]
              fields' =
                if bindsExistentials exts tms then
                  throw (ClashException sp ($(curLoc)
                    ++ "Not in normal form: Pattern binds existential variables:\n\n"
                    ++ showPpr (toLegacy ctx e)) Nothing)
                else
                  fields
            argHWTys <- mapM (hwTypeOfMaybe . snd) fields
            let tmsBundled   = zip argHWTys fields'
                tmsFiltered  = filter (maybe False (not . isVoid) . fst) tmsBundled
                tmsFiltered' = map (fst . snd) tmsFiltered
            case elemIndex varTm tmsFiltered' of
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
                 ++ showPpr (toLegacy ctx e)) Nothing)
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

-- | Whether the types of the fields of a pattern mention its existential type
-- variables, see 'Clash.Netlist.Util.bindsExistentials'
bindsExistentials :: Tele TyBinder n i -> Tele IdBinder i l -> Bool
bindsExistentials exts tms =
  any (anyBoundOccursInType (teleNameBinders exts)) (teleIdBinderTypes tms)

-- | Generate an expression for a DataCon application occurring on the RHS of a let-binder
mkDcApplication
    :: (HasCallStack, Distinct n)
    => CompCtx n
    -> DeclarationType
    -> [HWType]
    -- ^ HWType of the LHS of the let-binder, can multiple types when we're
    -- creating a "split" product type (e.g. a tuple of a Clock and Reset)
    -> NetlistId n
    -- ^ Name hint for result id
    -> DataCon
    -- ^ Applied DataCon
    -> [Term n]
    -- ^ DataCon Arguments
    -> NetlistMonad (Expr,[Declaration])
    -- ^ Returned expression and a list of generate BlackBox declarations
mkDcApplication ctx declType [dstHType] bndr dc args = do
  let dcNm = C.nameOcc (dcName dc)
      argTys = map (inferCoreTypeOf (compTypeEnv ctx) (compScope ctx)) args
  argNm <- netlistId1 return (\b -> Id.suffix (localIdentifier ctx (localIdName b)) "dc_arg") bndr
  argHWTys <- mapM hwTypeOfMaybe argTys

  (argExprs, concat -> argDecls) <- unzip <$>
    mapM (\(e,t) -> mkExpr ctx False declType (NetlistId argNm t) e) (zip args argTys)

  -- Filter void arguments, but make sure to render their declarations:
  let
    filteredTypeExprDecls =
      filter
        (not . isVoidMaybe True . fst)
        (zip argHWTys argExprs)

    (hWTysFiltered, argExprsFiltered) = unzip filteredTypeExprDecls

    showArgs = showPpr (map (toLegacy ctx) args)

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
                      _ -> error $ $(curLoc) ++ "Unexpected number of arguments for `Cons`: " ++ showArgs
      Vector _ _ -> case argExprsFiltered of
                      [e1,e2] -> return (HW.DataCon dstHType VecAppend [e1,e2])
                      _ -> error $ $(curLoc) ++ "Unexpected number of arguments for `Cons`: " ++ showArgs
      MemBlob _ _ ->
        case compare 6 (length argExprsFiltered) of
          EQ -> return (HW.DataCon dstHType (DC (dstHType,0)) argExprsFiltered)
          LT -> error $ $(curLoc) ++ "Over-applied constructor"
          GT -> error $ $(curLoc) ++ "Under-applied constructor"
      RTree 0 _ -> case argExprsFiltered of
                      [e] -> return (HW.DataCon dstHType RTreeAppend [e])
                      _ -> error $ $(curLoc) ++ "Unexpected number of arguments for `LR`: " ++ showArgs
      RTree _ _ -> case argExprsFiltered of
                      [e1,e2] -> return (HW.DataCon dstHType RTreeAppend [e1,e2])
                      _ -> error $ $(curLoc) ++ "Unexpected number of arguments for `BR`: " ++ showArgs
      String ->
        let dc' = case dcTag dc of
                    1 -> HW.Literal Nothing (StringLit "")
                    _ -> error $ $(curLoc) ++ "mkDcApplication undefined for: " ++ show (dstHType,dc,dcTag dc,showArgs,argHWTys)
        in  return dc'
      Void {} -> return Noop
      Signed _
        | dcNm == showt 'IS
        , (a:_) <- argExprsFiltered
        -> pure a
        -- ByteArray# are non-translatable / void, except when they're literals
        | dcNm == showt 'IP
        , (a@(HW.Literal Nothing (NumLit _)):_) <- argExprs
        -> pure a
        | dcNm == showt 'IN
        -- ByteArray# are non-translatable / void, except when they're literals
        , (HW.Literal Nothing (NumLit i):_) <- argExprs
        -> pure (HW.Literal Nothing (NumLit (negate i)))
      Unsigned _
        | dcNm == showt 'NS
        , (a:_) <- argExprsFiltered
        -> pure a
        | dcNm == showt 'NB
        -- ByteArray# are non-translatable / void, except when they're literals
        , (a@(HW.Literal Nothing (NumLit _)):_) <- argExprs
        -> pure a
      _ ->
        error $ $(curLoc) ++ "mkDcApplication undefined for: " ++ show (dstHType,dc,showArgs,argHWTys)

-- Handle MultiId assignment
mkDcApplication ctx declType dstHTypes (MultiId argNms) _ args = do
  let argTys = map (inferCoreTypeOf (compTypeEnv ctx) (compScope ctx)) args
  argHWTys            <- mapM hwTypeOfMaybe argTys
  -- Filter out the arguments of hwtype `Void` and only translate
  -- them to the intermediate HDL afterwards
  let argsBundled   = zip argHWTys (zipEqual (map CoreId argNms) args)
      (_hWTysFiltered,argsFiltered) = unzip
        (filter (maybe True (not . isVoid) . fst) argsBundled)
  (argExprs,argDecls) <- fmap (second concat . unzip) $!
                         mapM (uncurry (mkExpr ctx False declType)) argsFiltered
  if length dstHTypes == length argExprs then do
    assns <- mapMaybeM
                  (\case (_,Noop) -> pure Nothing
                         (dstId,e) -> let nm = netlistIdentifier ctx dstId
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

mkDcApplication _ _ _ _ _ _ = error "internal error"

-- * Utilities

-- | Open a case alternative, and give the variables its pattern binds their
-- human readable names as HDL identifiers: those of Clash Core, see
-- 'Clash.Netlist.Id.unsafeFromCoreId'. Netlist only refers to the field
-- binders of projections ('mkProjection').
withAlt
  :: Distinct n
  => CompCtx n
  -> Alt n
  -> (forall l. DExt n l => CompCtx l -> Pat n l -> Term l -> r)
  -> r
withAlt ctx alt k = openAlt (compScope ctx) alt $ \sc pat e ->
  let (tvs, ids) = patVars pat
      ctxTy = extendCompCtxTy sc [ (o, kd, nm) | VarTy o kd nm <- tvs ] ctx
      ctx' = extendCompCtx sc
               [ (LocalId o ty nm, Id.unsafeMake (C.nameOcc o)) | Var o ty nm <- ids ]
               ctxTy
  in k ctx' pat e

-- | The HDL identifier of a 'NetlistId', fails on 'MultiId'
netlistIdentifier :: HasCallStack => CompCtx n -> NetlistId n -> Identifier
netlistIdentifier ctx = netlistId1 id (localIdentifier ctx . localIdName)

-- | The HWType of a type. TODO(Phase 5, Util): translate types of scoped Core.
hwTypeOf :: String -> Type n -> NetlistMonad HWType
hwTypeOf loc = unsafeCoreTypeToHWTypeM' loc . toClashType

-- | The HWType of a type, if it has one, see 'hwTypeOf'
hwTypeOfMaybe :: Type n -> NetlistMonad (Maybe HWType)
hwTypeOfMaybe = coreTypeToHWTypeM' . toClashType
