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
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
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
import           Control.Monad.Extra              (concatMapM)
import           Control.Monad.IO.Class           (liftIO)
import           Control.Monad.State.Strict       (State)
import           Data.Either                      (lefts, partitionEithers)
import           Data.List.NonEmpty               (NonEmpty (..))
import qualified Data.List.NonEmpty               as NE
import           Data.Maybe                       (fromMaybe)
import qualified Data.Text                        as StrictText
import           GHC.Stack                        (HasCallStack)

import           Clash.Annotations.BitRepresentation.Internal (CustomReprs)
import           Clash.Core.DataCon               (DataCon)
import           Clash.Core.HasType               (coreTypeOf)
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
import           Clash.Netlist.Types              as HW
  hiding (Literal, NetlistId (..), netlistId1, netlistTypes, netlistTypes1)
import           Clash.Netlist.Util               (contAssign, isVoid, mkLiteral, procAssign)
import           Clash.Util                       (ClashException (..), curLoc, makeCachedO)
import qualified Clash.Util.Supply                as Supply
import           Clash.Core.Evaluator.Types       (Evaluator)

import           Clash.Core.Scoped.NameMap        (GlobalEnv, lookupGlobalEnv)
import           Clash.Core.Scoped.Subst          (openBind, teleIdVars)
import           Clash.Core.Scoped.Syntax
import           Clash.Core.Scoped.Term
  (collectArgsTicks, collectTicks, mkApps, mkTicks, stripTicks)

import           Clash.Netlist.Scoped.BlackBox    (mkPrimitive)
import           Clash.Netlist.Scoped.Legacy      (toLegacyNetlistId)
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
mkNetDecl :: CompCtx n -> (LocalId n, Term n) -> NetlistMonad [Declaration]
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
  hwTys  <- mapM (unsafeCoreTypeToHWTypeM' $(curLoc) . toLegacyType ctx) (netlistTypes bndr)
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
-- the scrutinee: see 'Clash.Netlist.mkSelection'
mkSelection
  :: Distinct n
  => CompCtx n
  -> DeclarationType
  -> NetlistId n
  -> Term n
  -> Type n
  -> NonEmpty (Alt n)
  -> [Declaration]
  -> NetlistMonad [Declaration]
mkSelection ctx declType bndr scrut altTy alts tickDecls =
  case toLegacy ctx (Case scrut altTy (NE.toList alts)) of
    C.Case scrut' altTy' (alt':alts') ->
      Legacy.mkSelection declType (toLegacyNetlistId ctx bndr) scrut' altTy'
        (alt' :| alts') tickDecls
    _ -> error ($(curLoc) ++ "impossible")

-- | Generate an expression that projects a field out of a data-constructor:
-- see 'Clash.Netlist.mkProjection'
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
mkProjection ctx declType mkDec bndr scrut altTy alt =
  case toLegacy ctx (Case scrut altTy [alt]) of
    C.Case scrut' altTy' [alt'] ->
      Legacy.mkProjection declType mkDec (toLegacyNetlistId ctx bndr) scrut' altTy' alt'
    _ -> error ($(curLoc) ++ "impossible")

-- | Generate an expression for a DataCon application occurring on the RHS of a
-- let-binder: see 'Clash.Netlist.mkDcApplication'
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
mkDcApplication ctx declType hwTys bndr dc args =
  Legacy.mkDcApplication declType hwTys (toLegacyNetlistId ctx bndr) dc
    (lefts (toLegacyArgs ctx (map Left args)))
