{-|
  Copyright  :  (C) 2026, QBayLogic B.V.
  License    :  BSD2 (see the file LICENSE)
  Maintainer :  QBayLogic B.V. <devops@qbaylogic.com>

  Shape-based transformations: transformations declare the 'Term' constructors
  they can fire on and receive the constructor's fields directly, so traversals
  can match a node's constructor once and dispatch only the transformations
  registered for that shape. See Note [shaped transformations].
-}

module Clash.Rewrite.Shape
  ( -- * Shaped transformations
    ShapeHandlers (..)
  , ShapedTransformation (..)
    -- * Declaring a transformation's entry shapes
    -- ** Single-shape transformations
  , applyVar
  , applyPrim
  , applyLam
  , applyApp
  , applyTyApp
  , applyLet
  , applyCase
  , applyCast
  , applyTick
  , applyAnyShape
    -- ** Multi-shape transformations
  , applyShapes
  , onVar
  , onData
  , onLiteral
  , onPrim
  , onLam
  , onTyLam
  , onApp
  , onTyApp
  , onLet
  , onCase
  , onCast
  , onTick
    -- ** Node-receiving handlers, for transformations that operate on the
    -- node itself (application spines, self-recursion on rebuilt terms)
  , onVarNode
  , onPrimNode
  , onAppNode
  , onTyAppNode
  , onLetNode
  , onTickNode
    -- ** Renaming
  , withTransformationName
    -- * Bundles
  , TransformationBundle
  , compileBundle
  , compileBundleQuiet
    -- * Running
  , dispatchBundle
  , runShapedTransformation
  , runShapedTransformationQuiet
    -- * Fused traversals
  , topdownBundle
  , bottomupBundle
  , topdownFixBundle
  , topdownSucBundle
  , innerMostBundle
  ) where

import qualified Control.Monad.Writer as Writer
import qualified Data.Monoid as Monoid

import Clash.Core.DataCon (DataCon)
import Clash.Core.Literal (Literal)
import Clash.Core.Term
  (Alt, Bind (..), CoreContext (..), PrimInfo, Term (..), TickInfo,
   bindToList, patIds, primArg)
import Clash.Core.Type (Type)
import Clash.Core.Var (Id, TyVar)
import Clash.Core.VarEnv (extendInScopeSet, extendInScopeSetList)
import Clash.Rewrite.Types
  (Rewrite, RewriteMonad, TransformContext (..))
import Clash.Rewrite.Util (applyWith)

{-
Note [shaped transformations]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
A 'ShapedTransformation' declares, through its 'ShapeHandlers' record, exactly
which 'Term' constructors it can fire on, and each handler receives the
constructor's fields directly. The handler record /is/ the transformation:
there is no second registration site whose shape list could drift from the
transformation's own pattern matches — the failure mode of hand-written
dispatch tables.

Traversals over a bundle of shaped transformations ('topdownBundle' and
friends) match a node's constructor once and run only that constructor's
bucket, in the order of the flat bundle list. This eliminates the per-attempt
'applyWith' overhead for transformations that cannot match the node, which
dominates normalization time for large bundles.

Two contracts make this sound:

1. /Unchanged means untouched./ A handler that does not signal change (through
   'Clash.Rewrite.Util.setChanged' or 'Clash.Rewrite.Util.changed') must
   return the node it was given, unmodified. The next handler in the bucket is
   then run on the same fields without re-matching the constructor. This is a
   pre-existing engine invariant; violations are caught by the "Expression
   changed without notice" check in 'Clash.Rewrite.Util.applyDebug'.

2. /Change re-guards the rest of the bucket./ When a handler signals change,
   the result may have any constructor. The remaining members of the entered
   bucket still run — each through its own full 'ShapeHandlers' record against
   the current term (the "guarded suffix"). This replicates the semantics of
   the sequential @'Clash.Rewrite.Combinators.>->'@ chain the bucket replaces:
   there, every remaining transformation was offered the changed term and its
   own pattern matches decided whether it fired. Transformations registered
   for other constructors only run once the surrounding traversal or fixpoint
   combinator re-dispatches on the new shape.

Constructor matches per node therefore come down to one plus the number of
rewrites that actually fired — matching is only repeated when the term it
matched on no longer exists.

Whole-term passes that genuinely inspect every constructor (such as
'Clash.Normalize.Transformations.ANF.makeANF') do not fit this model; they
stay plain 'Rewrite' values behind 'Clash.Rewrite.Util.apply'. The single
deliberately shape-agnostic per-node transformation, @topLet@, uses
'applyAnyShape', which is the greppable marker for "no entry shape declared".
-}

-- | The per-constructor entry points of one transformation. A 'Nothing' field
-- means the transformation cannot fire on that constructor and is never
-- offered such nodes. Handlers receive the matched node itself (for cheap
-- unchanged returns and for spine collection) followed by the constructor's
-- fields; use the @on*@ builders rather than constructing this record
-- directly.
data ShapeHandlers extra = ShapeHandlers
  { handleVar      :: !(Maybe (TransformContext -> Term -> Id -> RewriteMonad extra Term))
  , handleData     :: !(Maybe (TransformContext -> Term -> DataCon -> RewriteMonad extra Term))
  , handleLiteral  :: !(Maybe (TransformContext -> Term -> Literal -> RewriteMonad extra Term))
  , handlePrim     :: !(Maybe (TransformContext -> Term -> PrimInfo -> RewriteMonad extra Term))
  , handleLam      :: !(Maybe (TransformContext -> Term -> Id -> Term -> RewriteMonad extra Term))
  , handleTyLam    :: !(Maybe (TransformContext -> Term -> TyVar -> Term -> RewriteMonad extra Term))
  , handleApp      :: !(Maybe (TransformContext -> Term -> Term -> Term -> RewriteMonad extra Term))
  , handleTyApp    :: !(Maybe (TransformContext -> Term -> Term -> Type -> RewriteMonad extra Term))
  , handleLet      :: !(Maybe (TransformContext -> Term -> Bind Term -> Term -> RewriteMonad extra Term))
  , handleCase     :: !(Maybe (TransformContext -> Term -> Term -> Type -> [Alt] -> RewriteMonad extra Term))
  , handleCast     :: !(Maybe (TransformContext -> Term -> Term -> Type -> Type -> RewriteMonad extra Term))
  , handleTick     :: !(Maybe (TransformContext -> Term -> TickInfo -> Term -> RewriteMonad extra Term))
  , handleAnyShape :: !(Maybe (Rewrite extra))
    -- ^ Escape hatch for a per-node transformation that is genuinely
    -- shape-agnostic (its firing condition lives in the 'TransformContext',
    -- not the constructor). Mutually exclusive with all other fields.
  }

-- | '<>' combines the entry shapes of two handler records. Both records
-- providing a handler for the same constructor is a programmer error, as is
-- combining 'handleAnyShape' with per-constructor handlers.
instance Semigroup (ShapeHandlers extra) where
  a <> b = checkAnyShapeExclusive ShapeHandlers
    { handleVar      = combine "Var" (handleVar a) (handleVar b)
    , handleData     = combine "Data" (handleData a) (handleData b)
    , handleLiteral  = combine "Literal" (handleLiteral a) (handleLiteral b)
    , handlePrim     = combine "Prim" (handlePrim a) (handlePrim b)
    , handleLam      = combine "Lam" (handleLam a) (handleLam b)
    , handleTyLam    = combine "TyLam" (handleTyLam a) (handleTyLam b)
    , handleApp      = combine "App" (handleApp a) (handleApp b)
    , handleTyApp    = combine "TyApp" (handleTyApp a) (handleTyApp b)
    , handleLet      = combine "Let" (handleLet a) (handleLet b)
    , handleCase     = combine "Case" (handleCase a) (handleCase b)
    , handleCast     = combine "Cast" (handleCast a) (handleCast b)
    , handleTick     = combine "Tick" (handleTick a) (handleTick b)
    , handleAnyShape = combine "AnyShape" (handleAnyShape a) (handleAnyShape b)
    }
   where
    combine :: String -> Maybe h -> Maybe h -> Maybe h
    combine _ Nothing handler = handler
    combine _ handler Nothing = handler
    combine constructorName _ _ =
      error ("ShapeHandlers: duplicate handler for " <> constructorName)

instance Monoid (ShapeHandlers extra) where
  mempty = ShapeHandlers
    { handleVar = Nothing
    , handleData = Nothing
    , handleLiteral = Nothing
    , handlePrim = Nothing
    , handleLam = Nothing
    , handleTyLam = Nothing
    , handleApp = Nothing
    , handleTyApp = Nothing
    , handleLet = Nothing
    , handleCase = Nothing
    , handleCast = Nothing
    , handleTick = Nothing
    , handleAnyShape = Nothing
    }

-- | Error out when 'handleAnyShape' is combined with per-constructor
-- handlers: a transformation is either shape-agnostic or shaped, never both.
checkAnyShapeExclusive :: ShapeHandlers extra -> ShapeHandlers extra
checkAnyShapeExclusive handlers
  | Just _ <- handleAnyShape handlers
  , hasShapedHandler handlers
  = error "ShapeHandlers: handleAnyShape is mutually exclusive with per-constructor handlers"
  | otherwise
  = handlers

hasShapedHandler :: ShapeHandlers extra -> Bool
hasShapedHandler handlers = or
  [ present (handleVar handlers)
  , present (handleData handlers)
  , present (handleLiteral handlers)
  , present (handlePrim handlers)
  , present (handleLam handlers)
  , present (handleTyLam handlers)
  , present (handleApp handlers)
  , present (handleTyApp handlers)
  , present (handleLet handlers)
  , present (handleCase handlers)
  , present (handleCast handlers)
  , present (handleTick handlers)
  ]
 where
  present :: Maybe h -> Bool
  present = maybe False (const True)

-- | A transformation with a canonical name (used for debug tracing, counters,
-- and the rewrite history) and its declared entry shapes.
data ShapedTransformation extra = ShapedTransformation
  { transformationName     :: !String
  , transformationHandlers :: !(ShapeHandlers extra)
  }

-- | Give a shaped transformation a different name; used for deliberate debug
-- aliases of a transformation that runs at several strategy positions.
withTransformationName
  :: String -> ShapedTransformation extra -> ShapedTransformation extra
withTransformationName name shaped = shaped { transformationName = name }

-- | Declare a transformation with handlers for several constructors. The
-- handler record must have at least one handler.
applyShapes :: String -> ShapeHandlers extra -> ShapedTransformation extra
applyShapes name handlers
  | not (hasShapedHandler handlers)
  , Nothing <- handleAnyShape handlers
  = error ("applyShapes: transformation " <> name <> " declares no entry shapes")
  | otherwise
  = ShapedTransformation name handlers

-- Handler builders. Field-receiving handlers get the matched node itself —
-- to return on the unchanged path, with sharing intact — followed by the
-- constructor's fields, so no handler ever needs to match a 'Term'
-- constructor. The @on*Node@ builders register a plain 'Rewrite' at a single
-- constructor, for transformations that operate on collected application
-- spines and dispatch on the spine's head themselves.

onVar :: (TransformContext -> Term -> Id -> RewriteMonad extra Term) -> ShapeHandlers extra
onVar transform = mempty { handleVar = Just transform }

onData :: (TransformContext -> Term -> DataCon -> RewriteMonad extra Term) -> ShapeHandlers extra
onData transform = mempty { handleData = Just transform }

onLiteral :: (TransformContext -> Term -> Literal -> RewriteMonad extra Term) -> ShapeHandlers extra
onLiteral transform = mempty { handleLiteral = Just transform }

onPrim :: (TransformContext -> Term -> PrimInfo -> RewriteMonad extra Term) -> ShapeHandlers extra
onPrim transform = mempty { handlePrim = Just transform }

onLam :: (TransformContext -> Term -> Id -> Term -> RewriteMonad extra Term) -> ShapeHandlers extra
onLam transform = mempty { handleLam = Just transform }

onTyLam :: (TransformContext -> Term -> TyVar -> Term -> RewriteMonad extra Term) -> ShapeHandlers extra
onTyLam transform = mempty { handleTyLam = Just transform }

onApp :: (TransformContext -> Term -> Term -> Term -> RewriteMonad extra Term) -> ShapeHandlers extra
onApp transform = mempty { handleApp = Just transform }

onTyApp :: (TransformContext -> Term -> Term -> Type -> RewriteMonad extra Term) -> ShapeHandlers extra
onTyApp transform = mempty { handleTyApp = Just transform }

onLet :: (TransformContext -> Term -> Bind Term -> Term -> RewriteMonad extra Term) -> ShapeHandlers extra
onLet transform = mempty { handleLet = Just transform }

onCase :: (TransformContext -> Term -> Term -> Type -> [Alt] -> RewriteMonad extra Term) -> ShapeHandlers extra
onCase transform = mempty { handleCase = Just transform }

onCast :: (TransformContext -> Term -> Term -> Type -> Type -> RewriteMonad extra Term) -> ShapeHandlers extra
onCast transform = mempty { handleCast = Just transform }

onTick :: (TransformContext -> Term -> TickInfo -> Term -> RewriteMonad extra Term) -> ShapeHandlers extra
onTick transform = mempty { handleTick = Just transform }

onVarNode :: Rewrite extra -> ShapeHandlers extra
onVarNode transform = mempty { handleVar = Just (\ctx node _ -> transform ctx node) }

onPrimNode :: Rewrite extra -> ShapeHandlers extra
onPrimNode transform = mempty { handlePrim = Just (\ctx node _ -> transform ctx node) }

onAppNode :: Rewrite extra -> ShapeHandlers extra
onAppNode transform = mempty { handleApp = Just (\ctx node _ _ -> transform ctx node) }

onTyAppNode :: Rewrite extra -> ShapeHandlers extra
onTyAppNode transform = mempty { handleTyApp = Just (\ctx node _ _ -> transform ctx node) }

onLetNode :: Rewrite extra -> ShapeHandlers extra
onLetNode transform = mempty { handleLet = Just (\ctx node _ _ -> transform ctx node) }

onTickNode :: Rewrite extra -> ShapeHandlers extra
onTickNode transform = mempty { handleTick = Just (\ctx node _ _ -> transform ctx node) }

onAnyShape :: Rewrite extra -> ShapeHandlers extra
onAnyShape transform = mempty { handleAnyShape = Just transform }

-- Single-shape declarations.

applyVar :: String -> (TransformContext -> Term -> Id -> RewriteMonad extra Term) -> ShapedTransformation extra
applyVar name = applyShapes name . onVar

applyPrim :: String -> (TransformContext -> Term -> PrimInfo -> RewriteMonad extra Term) -> ShapedTransformation extra
applyPrim name = applyShapes name . onPrim

applyLam :: String -> (TransformContext -> Term -> Id -> Term -> RewriteMonad extra Term) -> ShapedTransformation extra
applyLam name = applyShapes name . onLam

applyApp :: String -> (TransformContext -> Term -> Term -> Term -> RewriteMonad extra Term) -> ShapedTransformation extra
applyApp name = applyShapes name . onApp

applyTyApp :: String -> (TransformContext -> Term -> Term -> Type -> RewriteMonad extra Term) -> ShapedTransformation extra
applyTyApp name = applyShapes name . onTyApp

applyLet :: String -> (TransformContext -> Term -> Bind Term -> Term -> RewriteMonad extra Term) -> ShapedTransformation extra
applyLet name = applyShapes name . onLet

applyCase :: String -> (TransformContext -> Term -> Term -> Type -> [Alt] -> RewriteMonad extra Term) -> ShapedTransformation extra
applyCase name = applyShapes name . onCase

applyCast :: String -> (TransformContext -> Term -> Term -> Type -> Type -> RewriteMonad extra Term) -> ShapedTransformation extra
applyCast name = applyShapes name . onCast

applyTick :: String -> (TransformContext -> Term -> TickInfo -> Term -> RewriteMonad extra Term) -> ShapedTransformation extra
applyTick name = applyShapes name . onTick

applyAnyShape :: String -> Rewrite extra -> ShapedTransformation extra
applyAnyShape name = applyShapes name . onAnyShape

-- | How a bundle wraps each handler invocation: the transformation's name,
-- the node it is offered, the handler action, and back the result together
-- with whether the handler signaled change.
type Instrumentation extra
  =  String
  -> TransformContext
  -> Term
  -> RewriteMonad extra Term
  -> RewriteMonad extra (Term, Bool)

quietInstrumentation :: Instrumentation extra
quietInstrumentation _name _ctx _node action = do
  (term, anyChanged) <- Writer.listen action
  pure (term, Monoid.getAny anyChanged)
{-# INLINE quietInstrumentation #-}

-- | One member of a compiled per-constructor bucket: its name, the handler
-- extracted for this constructor, and the member's full record for the
-- guarded suffix after an earlier member changed the term.
data BucketEntry extra handler =
  BucketEntry !String handler !(ShapedTransformation extra)

bucketEntryMember :: BucketEntry extra handler -> ShapedTransformation extra
bucketEntryMember (BucketEntry _ _ member) = member

-- | An ordered list of shaped transformations, compiled into per-constructor
-- buckets. The bucket order is the flat list order restricted to the members
-- that handle the constructor, so the grouping can never fall out of sync
-- with the list.
data TransformationBundle extra = TransformationBundle
  { bundleInstrumentation :: !(Instrumentation extra)
  , bundleVar     :: ![BucketEntry extra (TransformContext -> Term -> Id -> RewriteMonad extra Term)]
  , bundleData    :: ![BucketEntry extra (TransformContext -> Term -> DataCon -> RewriteMonad extra Term)]
  , bundleLiteral :: ![BucketEntry extra (TransformContext -> Term -> Literal -> RewriteMonad extra Term)]
  , bundlePrim    :: ![BucketEntry extra (TransformContext -> Term -> PrimInfo -> RewriteMonad extra Term)]
  , bundleLam     :: ![BucketEntry extra (TransformContext -> Term -> Id -> Term -> RewriteMonad extra Term)]
  , bundleTyLam   :: ![BucketEntry extra (TransformContext -> Term -> TyVar -> Term -> RewriteMonad extra Term)]
  , bundleApp     :: ![BucketEntry extra (TransformContext -> Term -> Term -> Term -> RewriteMonad extra Term)]
  , bundleTyApp   :: ![BucketEntry extra (TransformContext -> Term -> Term -> Type -> RewriteMonad extra Term)]
  , bundleLet     :: ![BucketEntry extra (TransformContext -> Term -> Bind Term -> Term -> RewriteMonad extra Term)]
  , bundleCase    :: ![BucketEntry extra (TransformContext -> Term -> Term -> Type -> [Alt] -> RewriteMonad extra Term)]
  , bundleCast    :: ![BucketEntry extra (TransformContext -> Term -> Term -> Type -> Type -> RewriteMonad extra Term)]
  , bundleTick    :: ![BucketEntry extra (TransformContext -> Term -> TickInfo -> Term -> RewriteMonad extra Term)]
  }

-- | Compile a bundle whose handler invocations run under the full
-- 'applyWith' instrumentation (debug tracing, transformation counters,
-- rewrite history, invariant checks).
compileBundle :: [ShapedTransformation extra] -> TransformationBundle extra
compileBundle = compileBundleWith applyWith

-- | Compile a bundle without instrumentation, for traversals inside other
-- transformations that today invoke a transformation function directly.
compileBundleQuiet :: [ShapedTransformation extra] -> TransformationBundle extra
compileBundleQuiet = compileBundleWith quietInstrumentation

compileBundleWith
  :: forall extra
   . Instrumentation extra
  -> [ShapedTransformation extra]
  -> TransformationBundle extra
compileBundleWith instrumentation members = TransformationBundle
  { bundleInstrumentation = instrumentation
  , bundleVar     = bucket handleVar (\transform ctx node _ -> transform ctx node)
  , bundleData    = bucket handleData (\transform ctx node _ -> transform ctx node)
  , bundleLiteral = bucket handleLiteral (\transform ctx node _ -> transform ctx node)
  , bundlePrim    = bucket handlePrim (\transform ctx node _ -> transform ctx node)
  , bundleLam     = bucket handleLam (\transform ctx node _ _ -> transform ctx node)
  , bundleTyLam   = bucket handleTyLam (\transform ctx node _ _ -> transform ctx node)
  , bundleApp     = bucket handleApp (\transform ctx node _ _ -> transform ctx node)
  , bundleTyApp   = bucket handleTyApp (\transform ctx node _ _ -> transform ctx node)
  , bundleLet     = bucket handleLet (\transform ctx node _ _ -> transform ctx node)
  , bundleCase    = bucket handleCase (\transform ctx node _ _ _ -> transform ctx node)
  , bundleCast    = bucket handleCast (\transform ctx node _ _ _ -> transform ctx node)
  , bundleTick    = bucket handleTick (\transform ctx node _ _ -> transform ctx node)
  }
 where
  -- A member lands in a constructor's bucket when it has a handler for that
  -- constructor, or — for shape-agnostic members — in every bucket, with its
  -- 'handleAnyShape' handler adapted to ignore the fields.
  bucket
    :: (ShapeHandlers extra -> Maybe handler)
    -> (Rewrite extra -> handler)
    -> [BucketEntry extra handler]
  bucket field adaptAnyShape =
    [ BucketEntry (transformationName member) handler member
    | member <- members
    , Just handler <- [entryFor member]
    ]
   where
    entryFor shaped = case field (transformationHandlers shaped) of
      Just handler -> Just handler
      Nothing -> adaptAnyShape <$> handleAnyShape (transformationHandlers shaped)

-- | Run a constructor's bucket: members in bundle order, each offered the
-- fields directly. After a member signals change the remaining members run as
-- the guarded suffix. See Note [shaped transformations].
runBucket
  :: Instrumentation extra
  -> (handler -> TransformContext -> Term -> RewriteMonad extra Term)
  -- ^ Apply a member's handler to this node's fields
  -> [BucketEntry extra handler]
  -> TransformContext
  -> Term
  -> RewriteMonad extra Term
runBucket instrumentation applyFields entries0 ctx = go entries0
 where
  go [] node = pure node
  go (BucketEntry name handler _ : rest) node = do
    (node1, changed) <-
      instrumentation name ctx node (applyFields handler ctx node)
    if changed
      then runGuardedSuffix instrumentation (map bucketEntryMember rest) ctx node1
      else go rest node1
{-# INLINE runBucket #-}

-- | Offer the term to each member in turn, going through the member's own
-- handler record: the constructor changed mid-bucket, so every remaining
-- member re-guards on the current term, exactly as the sequential chain the
-- bucket replaces did through the members' own pattern matches.
runGuardedSuffix
  :: Instrumentation extra
  -> [ShapedTransformation extra]
  -> TransformContext
  -> Term
  -> RewriteMonad extra Term
runGuardedSuffix _ [] _ node = pure node
runGuardedSuffix instrumentation (member : rest) ctx node = do
  node1 <- runMemberGuarded instrumentation member ctx node
  runGuardedSuffix instrumentation rest ctx node1

-- | Run one member on a term of unknown constructor: consult the member's
-- handler record; return the term untouched (without instrumentation) when no
-- handler is registered for its constructor.
runMemberGuarded
  :: Instrumentation extra
  -> ShapedTransformation extra
  -> Rewrite extra
runMemberGuarded instrumentation (ShapedTransformation name handlers) ctx node =
  case node of
    Var i | Just h <- handleVar handlers -> run (h ctx node i)
    Data dataCon | Just h <- handleData handlers -> run (h ctx node dataCon)
    Literal literal | Just h <- handleLiteral handlers -> run (h ctx node literal)
    Prim primInfo | Just h <- handlePrim handlers -> run (h ctx node primInfo)
    Lam i body | Just h <- handleLam handlers -> run (h ctx node i body)
    TyLam tyVar body | Just h <- handleTyLam handlers -> run (h ctx node tyVar body)
    App function argument | Just h <- handleApp handlers -> run (h ctx node function argument)
    TyApp function argumentType | Just h <- handleTyApp handlers -> run (h ctx node function argumentType)
    Let bind body | Just h <- handleLet handlers -> run (h ctx node bind body)
    Case subject type_ alternatives | Just h <- handleCase handlers -> run (h ctx node subject type_ alternatives)
    Cast body fromType toType | Just h <- handleCast handlers -> run (h ctx node body fromType toType)
    Tick tickInfo body | Just h <- handleTick handlers -> run (h ctx node tickInfo body)
    _ | Just h <- handleAnyShape handlers -> run (h ctx node)
    _ -> pure node
 where
  run action = fst <$> instrumentation name ctx node action

-- | A node together with its bucket chain and its one-level descent, produced
-- by a single constructor match. 'nodeStepDescend' returns the rebuilt node's
-- 'NodeStep' without re-matching: descent preserves the constructor.
data NodeStep extra = NodeStep
  { nodeStepTerm :: Term
  , nodeStepChain :: TransformContext -> RewriteMonad extra Term
  , nodeStepDescend :: Rewrite extra -> TransformContext -> RewriteMonad extra (NodeStep extra)
  }

-- | Match a node's constructor — the only place fused traversals do so — and
-- expose everything the traversals need. The descent arms replicate
-- 'Clash.Rewrite.Combinators.allR' exactly: same 'CoreContext' pushes, same
-- 'InScopeSet' extensions, and the same rebuild behavior. See
-- Note [NonRec erasure during descent] and the argument-context comment in
-- the 'App' arm.
nodeStep :: forall extra. TransformationBundle extra -> Term -> NodeStep extra
nodeStep bundle = step
 where
  instrumentation = bundleInstrumentation bundle

  step :: Term -> NodeStep extra
  step node = case node of
    Var i -> leafStep (\h ctx' node' -> h ctx' node' i) (bundleVar bundle) node
    Data dataCon -> leafStep (\h ctx' node' -> h ctx' node' dataCon) (bundleData bundle) node
    Literal literal -> leafStep (\h ctx' node' -> h ctx' node' literal) (bundleLiteral bundle) node
    Prim primInfo -> leafStep (\h ctx' node' -> h ctx' node' primInfo) (bundlePrim bundle) node
    Lam i body -> lamStep node i body
    TyLam tyVar body -> tyLamStep node tyVar body
    App function argument -> appStep node function argument
    TyApp function argumentType -> tyAppStep node function argumentType
    Let bind body -> letStep node bind body
    Case subject type_ alternatives -> caseStep node subject type_ alternatives
    Cast body fromType toType -> castStep node body fromType toType
    Tick tickInfo body -> tickStep node tickInfo body

  -- Leaves have no children to descend into.
  leafStep applyFields entries node = NodeStep
    { nodeStepTerm = node
    , nodeStepChain = \ctx -> runBucket instrumentation applyFields entries ctx node
    , nodeStepDescend = \_ _ -> pure (leafStep applyFields entries node)
    }

  lamStep node i body = NodeStep
    { nodeStepTerm = node
    , nodeStepChain = \ctx ->
        runBucket instrumentation (\h ctx' node' -> h ctx' node' i body) (bundleLam bundle) ctx node
    , nodeStepDescend = \go (TransformContext is c) -> do
        body' <- go (TransformContext (extendInScopeSet is i) (LamBody i : c)) body
        pure (lamStep (Lam i body') i body')
    }

  tyLamStep node tyVar body = NodeStep
    { nodeStepTerm = node
    , nodeStepChain = \ctx ->
        runBucket instrumentation (\h ctx' node' -> h ctx' node' tyVar body) (bundleTyLam bundle) ctx node
    , nodeStepDescend = \go (TransformContext is c) -> do
        body' <- go (TransformContext (extendInScopeSet is tyVar) (TyLamBody tyVar : c)) body
        pure (tyLamStep (TyLam tyVar body') tyVar body')
    }

  appStep node function argument = NodeStep
    { nodeStepTerm = node
    , nodeStepChain = \ctx ->
        runBucket instrumentation (\h ctx' node' -> h ctx' node' function argument) (bundleApp bundle) ctx node
    , nodeStepDescend = \go (TransformContext is c) -> do
        function' <- go (TransformContext is (AppFun : c)) function
        -- The argument's context is computed from the already rewritten
        -- function; the order is load-bearing.
        argument' <- go (TransformContext is (AppArg (primArg function') : c)) argument
        pure (appStep (App function' argument') function' argument')
    }

  tyAppStep node function argumentType = NodeStep
    { nodeStepTerm = node
    , nodeStepChain = \ctx ->
        runBucket instrumentation (\h ctx' node' -> h ctx' node' function argumentType) (bundleTyApp bundle) ctx node
    , nodeStepDescend = \go (TransformContext is c) -> do
        function' <- go (TransformContext is (TyAppC : c)) function
        pure (tyAppStep (TyApp function' argumentType) function' argumentType)
    }

  letStep node bind body = NodeStep
    { nodeStepTerm = node
    , nodeStepChain = \ctx ->
        runBucket instrumentation (\h ctx' node' -> h ctx' node' bind body) (bundleLet bundle) ctx node
    , nodeStepDescend = \go (TransformContext is c) -> do
        -- See Note [NonRec erasure during descent]
        let bindings = bindToList bind
            binders = map fst bindings
            is' = extendInScopeSetList is binders
            rewriteBinding (b, rhs) =
              (b,) <$> go (TransformContext is' (LetBinding b binders : c)) rhs
        bindings' <- traverse rewriteBinding bindings
        body' <- go (TransformContext is' (LetBody bindings : c)) body
        pure (letStep (Let (Rec bindings') body') (Rec bindings') body')
    }

  caseStep node subject type_ alternatives = NodeStep
    { nodeStepTerm = node
    , nodeStepChain = \ctx ->
        runBucket instrumentation (\h ctx' node' -> h ctx' node' subject type_ alternatives) (bundleCase bundle) ctx node
    , nodeStepDescend = \go (TransformContext is c) -> do
        subject' <- go (TransformContext is (CaseScrut : c)) subject
        let rewriteAlternative (pat, expression) =
              let (tyVars, ids) = patIds pat
                  is' = extendInScopeSetList (extendInScopeSetList is tyVars) ids
              in (pat,) <$> go (TransformContext is' (CaseAlt pat : c)) expression
        alternatives' <- traverse rewriteAlternative alternatives
        pure (caseStep (Case subject' type_ alternatives') subject' type_ alternatives')
    }

  castStep node body fromType toType = NodeStep
    { nodeStepTerm = node
    , nodeStepChain = \ctx ->
        runBucket instrumentation (\h ctx' node' -> h ctx' node' body fromType toType) (bundleCast bundle) ctx node
    , nodeStepDescend = \go (TransformContext is c) -> do
        body' <- go (TransformContext is (CastBody : c)) body
        pure (castStep (Cast body' fromType toType) body' fromType toType)
    }

  tickStep node tickInfo body = NodeStep
    { nodeStepTerm = node
    , nodeStepChain = \ctx ->
        runBucket instrumentation (\h ctx' node' -> h ctx' node' tickInfo body) (bundleTick bundle) ctx node
    , nodeStepDescend = \go (TransformContext is c) -> do
        body' <- go (TransformContext is (TickC tickInfo : c)) body
        pure (tickStep (Tick tickInfo body') tickInfo body')
    }

{-
Note [NonRec erasure during descent]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
'Clash.Rewrite.Combinators.allR' matches let-expressions through the 'Letrec'
pattern synonym and rebuilds them with 'Rec' bindings, so any 'NonRec' let a
traversal descends into comes back as a 'Rec' let. The fused descent in
'nodeStep' replicates that behavior deliberately: preserving 'NonRec' is a
real behavior change (downstream code branches on 'Rec'/'NonRec', and netlist
rendering order can shift) that must be validated on its own, separately from
this dispatch machinery.
-}

listenChanged :: RewriteMonad extra a -> RewriteMonad extra (a, Bool)
listenChanged action = do
  (result, anyChanged) <- Writer.listen action
  pure (result, Monoid.getAny anyChanged)
{-# INLINE listenChanged #-}

-- | Match the constructor once and run that constructor's bucket, without
-- traversing. The bundle equivalent of a single 'Clash.Rewrite.Util.apply'
-- chain, for composing with plain combinators.
dispatchBundle :: TransformationBundle extra -> Rewrite extra
dispatchBundle bundle ctx term = nodeStepChain (nodeStep bundle term) ctx

-- | Run a single shaped transformation as a plain 'Rewrite', instrumented
-- like 'Clash.Rewrite.Util.apply'.
runShapedTransformation :: ShapedTransformation extra -> Rewrite extra
runShapedTransformation shaped = runMemberGuarded applyWith shaped

-- | Like 'runShapedTransformation' but without instrumentation, for use
-- inside other transformations.
runShapedTransformationQuiet :: ShapedTransformation extra -> Rewrite extra
runShapedTransformationQuiet shaped = runMemberGuarded quietInstrumentation shaped

-- | Fused equivalent of @'Clash.Rewrite.Combinators.topdownR'
-- ('dispatchBundle' bundle)@: at each node, run the bucket chain to a
-- fixpoint, then descend into the children of the settled node — matching the
-- constructor only when a rewrite fired.
topdownBundle :: TransformationBundle extra -> Rewrite extra
topdownBundle bundle = go
 where
  go ctx term = goStep ctx (nodeStep bundle term)
  -- See Note [topdown repeatR] in Clash.Rewrite.Combinators: the chain is
  -- repeated at each node before descending.
  goStep ctx step = do
    (term1, changed) <- listenChanged (nodeStepChain step ctx)
    if changed
      then go ctx term1
      else nodeStepTerm <$> nodeStepDescend step go ctx

-- | Fused equivalent of @'Clash.Rewrite.Combinators.bottomupR'
-- ('dispatchBundle' bundle)@: descend first, then run the bucket chain once
-- on the rebuilt node — whose constructor is already known, so no re-match.
bottomupBundle :: TransformationBundle extra -> Rewrite extra
bottomupBundle bundle = go
 where
  go ctx term = do
    step' <- nodeStepDescend (nodeStep bundle term) go ctx
    nodeStepChain step' ctx

-- | Fused equivalent of @'Clash.Rewrite.Combinators.topdownFixR'
-- ('dispatchBundle' bundle)@; the control flow ports Note [topdownFixR]
-- verbatim, but the parent recheck after a child change reuses the arm the
-- descent returned instead of re-matching the constructor.
topdownFixBundle :: TransformationBundle extra -> Rewrite extra
topdownFixBundle bundle = \ctx term -> goStep True ctx (nodeStep bundle term)
 where
  goStep tryParent ctx step0 = do
    settled0 <-
      if tryParent
        then settle ctx step0
        else pure step0
    (step1, childChanged) <- listenChanged (nodeStepDescend settled0 goChild ctx)
    if childChanged
      then do
        (step2, parentChanged) <- listenChanged (settle ctx step1)
        if parentChanged
          then goStep False ctx step2
          else pure (nodeStepTerm step2)
      else pure (nodeStepTerm step1)

  goChild ctx term = goStep True ctx (nodeStep bundle term)

  -- Repeat the chain at this node until it no longer fires; matching happens
  -- only after a change. Returns the settled node's step.
  settle ctx step = do
    (term1, changed) <- listenChanged (nodeStepChain step ctx)
    if changed
      then settle ctx (nodeStep bundle term1)
      else pure step

-- | Fused equivalent of @topdownSucR ('dispatchBundle' bundle)@ from
-- "Clash.Normalize.Strategy": run the chain once; when it fired, stop —
-- neither repeating nor descending — otherwise descend.
topdownSucBundle :: TransformationBundle extra -> Rewrite extra
topdownSucBundle bundle = go
 where
  go ctx term = do
    let step = nodeStep bundle term
    (term1, changed) <- listenChanged (nodeStepChain step ctx)
    if changed
      then pure term1
      else nodeStepTerm <$> nodeStepDescend step go ctx

-- | Fused equivalent of @innerMost ('dispatchBundle' bundle)@ from
-- "Clash.Normalize.Strategy": bottom-up; when the chain fires, re-traverse
-- the result until the innermost fixpoint is reached.
innerMostBundle :: TransformationBundle extra -> Rewrite extra
innerMostBundle bundle = go
 where
  go ctx term = do
    step' <- nodeStepDescend (nodeStep bundle term) go ctx
    (term1, changed) <- listenChanged (nodeStepChain step' ctx)
    if changed
      then go ctx term1
      else pure term1
