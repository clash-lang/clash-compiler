{-|
  Copyright  :  (C) 2012-2016, University of Twente,
                    2021-2026, QBayLogic B.V.
  License    :  BSD2 (see the file LICENSE)
  Maintainer :  QBayLogic B.V. <devops@qbaylogic.com>

  Utility functions used by the normalisation transformations.

  Utility functions used by the normalization transformations on scoped Clash
  Core ("Clash.Core.Syntax").

  = Globals

  Tables of the normalizer that are keyed by global binders (the recursive
  components, the inline history, the normalized binders) are t'GlobalEnv's,
  so that a local variable can never hit an entry of a global with the same
  unique. The global bindings ('Clash.Rewrite.Types.bindings') and the
  normalized ones ('normalized') are closed terms.
-}

{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskellQuotes #-}

module Clash.Normalize.Util
 ( -- * Constant arguments of primitives
    isConstantArg
  , shouldReduce
    -- * Inlining
  , alreadyInlined
  , addNewInline
  , mkInlineTick
    -- * Recursive globals
  , isRecursiveBndr
  , isNonRecursiveGlobalVar
    -- * Constant specialization
  , ConstantSpecInfo (..)
  , csrFoundConstant
  , constantSpecInfo
    -- * Call graphs
  , CallGraph
  , callGraph
  , callGraphGlobals
    -- * Classifying functions
  , classifyFunction
  , isCheapFunction
    -- * Normalizing global binders
  , normalizeTopLvlBndr
  , normalizeWorkFreeApp
  , rewriteExpr
  , getNormalization
    -- * Type equality constraints
  , substWithTyEq
  , tvSubstWithTyEq
  )
 where

import           Control.Lens                ((&), (+~), (.=), (%=))
import qualified Control.Lens                as Lens
import           Control.Monad.Extra         (anyM)
import           Data.Bifunctor              (bimap)
import qualified Data.HashMap.Strict         as HashMapS
import qualified Data.IntMap.Strict          as IntMap
import qualified Data.List                   as List
import qualified Data.Map                    as MapLazy
import qualified Data.Map.Strict             as Map
import           Data.Maybe                  (fromMaybe)
import           Data.Text                   (Text)
import qualified Data.Text                   as Text
import           GHC.Builtin.Names           (eqTyConKey)
import           GHC.Stack                   (HasCallStack)
import           GHC.Types.SrcLoc            (SrcSpan)

import           Clash.Annotations.Primitive (extractPrim)
import           Clash.Core.Basic            (LitTy (..), NameMod (..))
import qualified Clash.Core.Name             as C
import           Clash.Core.Pretty           (showPpr)
import qualified Clash.Core.TyCon            as C (TyConName)
import           Clash.Debug                 (traceIf)
import           Clash.Driver.Types
  (Binding (..), TransformationInfo (FinalTerm), hasTransformationInfo)
import           Clash.Normalize.Primitives  (removedArg)
import           Clash.Normalize.Types
import           Clash.Primitives.Util       (constantArgs)
import           Clash.Rewrite.Types
  ( RewriteMonad, curFun, debugOpts, extra, bindings, primitives, typeEnv
  , FrameKind (..), Rewrite, TransformContext, ctxFrames, mkDerivedName, tfScope )
import           Clash.Unique                (fromGhcUnique)

import           Clash.Core.Alpha     (AlphaTerm (..), AlphaType (..))
import           Clash.Core.FreeVars  (teleNameBinders, termGlobalIds)
import           Clash.Core.HasType   (inferCoreTypeOf)
import           Clash.Core.NameMap
import           Clash.Core.Subst
import           Clash.Core.Syntax
import           Clash.Core.Term
import           Clash.Core.TermInfo  (isClosed)
import           Clash.Core.Type
  (PiTele (..), TypeView (..), isClockOrReset, mkFunTy, piTeleNameBinders, tyView)
import           Clash.Rewrite.Apply  (runRewrite)
import           Clash.Rewrite.Globals (freshGlobalId)

-- | Determine if argument should reduce to a constant given a primitive and
-- an argument number. Caches results.
isConstantArg
  :: Text
  -- ^ Primitive name
  -> Int
  -- ^ Argument number
  -> RewriteMonad NormalizeState Bool
  -- ^ Yields @DontCare@ for if given primitive name is not found, if the
  -- argument does not exist, or if the argument was not mentioned by the
  -- blackbox.
isConstantArg "Clash.Explicit.SimIO.mealyIO" i = pure (i == 2 || i == 3)
isConstantArg nm i = do
  argMap <- Lens.use (extra.primitiveArgs)
  case Map.lookup nm argMap of
    Nothing -> do
      -- Constant args not yet calculated, or primitive does not exist
      prims <- Lens.view primitives
      case extractPrim =<< HashMapS.lookup nm prims of
        Nothing ->
          -- Primitive does not exist:
          pure False
        Just p -> do
          -- Calculate constant arguments:
          let m = constantArgs nm p
          (extra.primitiveArgs) Lens.%= MapLazy.insert nm m
          pure (i `elem` m)
    Just m ->
      -- Cached version found
      pure (i `elem` m)

-- * Constant arguments of primitives

-- | Whether the context of a subterm indicates that it is to be reduced to a
-- constant / literal: whether it is an argument of a primitive that wants that
-- argument to be constant (see 'isConstantArg').
shouldReduce :: TransformContext n -> NormalizeSession Bool
shouldReduce = anyM isConstantArg' . ctxFrames
 where
  isConstantArg' (KAppArg (Just (nm, _, i))) = isConstantArg nm i
  isConstantArg' _ = pure False

-- * Inlining

-- | How often a global was already inlined into another one, according to
-- the 'inlineHistory'
alreadyInlined
  :: GlobalId
  -- ^ Function we want to inline
  -> GlobalId
  -- ^ Function in which we want to perform the inlining
  -> NormalizeMonad (Maybe Int)
alreadyInlined f cf = do
  inlinedHM <- Lens.use inlineHistory
  pure (lookupGlobalEnv f =<< lookupGlobalEnv cf inlinedHM)

-- | Record a new inlining in the 'inlineHistory'
addNewInline
  :: GlobalId
  -- ^ Function we're inlining
  -> GlobalId
  -- ^ Function in which we're inlining it
  -> NormalizeMonad ()
addNewInline f cf =
  inlineHistory %= insertWithGlobalEnv
                           (\_ hm -> insertWithGlobalEnv (+) f 1 hm)
                           cf
                           (singletonGlobalEnv f 1)

-- | A tick to prefix an inlined expression with the name of the global it
-- came from. For example, given
--
-- > foo = bar  -- ...
-- > bar = baz  -- ...
-- > baz = quuz -- ...
--
-- if @bar@ is inlined into @foo@, then the name of the component should
-- contain the name of the inlined component. This tick ensures that the
-- component in @foo@ is called @bar_baz@ instead of just @baz@.
mkInlineTick :: GlobalId -> TickInfo n
mkInlineTick n = NameMod PrefixName (LitTy (SymTy (toStr n)))
 where
  toStr = Text.unpack . snd . Text.breakOnEnd "." . C.nameOcc . globalIdName

-- * Recursive globals

-- | Whether a term is an application of a non-recursive global. A local
-- variable is not a global: the two can't be confused.
isNonRecursiveGlobalVar :: Term n -> NormalizeSession Bool
isNonRecursiveGlobalVar (collectArgs -> (GlobalVar g, _)) = not <$> isRecursiveBndr g
isNonRecursiveGlobalVar _ = pure False

-- | Whether a global binder is recursive. Caches its answer in
-- 'recursiveComponents'.
isRecursiveBndr :: GlobalId -> NormalizeSession Bool
isRecursiveBndr g = do
  cg <- Lens.use (extra.recursiveComponents)
  case lookupGlobalEnv g cg of
    Just isR -> pure isR
    Nothing -> do
      fBodyM <- lookupGlobalEnv g <$> Lens.use bindings
      case fBodyM of
        Nothing -> pure False
        Just b -> do
          -- There are no global mutually-recursive functions, only
          -- self-recursive ones, so checking whether 'f' occurs in its body is
          -- sufficient.
          let isR = g `memberGlobalSet` termGlobalIds (bindingTerm b)
          (extra.recursiveComponents) %= insertGlobalEnv g isR
          pure isR

-- * Constant specialization

-- | The constant parts of a term in scope @n@, see 'constantSpecInfo'
data ConstantSpecInfo n where
  ConstantSpecInfo
    :: DExt n l
    => !(Scope l)
    -- ^ The scope of the new let-bindings
    -> !(Tele Bind n l)
    -- ^ New non-recursive let-bindings, binding the non-constant parts.
    -- Their right-hand sides don't refer to one another.
    -> !(Term l)
    -- ^ The term, with its non-constant parts replaced by the variables of
    -- the new let-bindings
    -> !Bool
    -- ^ Whether any constant part was found. If not, the new let-bindings are
    -- of no use.
    -> ConstantSpecInfo n

-- | Whether 'constantSpecInfo' found a constant part
csrFoundConstant :: ConstantSpecInfo n -> Bool
csrFoundConstant (ConstantSpecInfo _ _ _ found) = found

-- | Calculate constant spec info: split a term in its constant parts, and
-- let-bindings for its non-constant parts. The goal of this function is to
-- analyze a given term and yield a new term that:
--
--  * Leaves all the constant parts as they were.
--  * Has all /variable/ parts replaced by a newly generated identifier.
--
-- The result structure will additionally contain:
--
--  * Whether the function found any constant parts at all
--  * The let-bindings binding the aforementioned identifiers with the term
--    they replaced.
--
-- This can be used in functions wanting to constant specialize over
-- partially constant data structures.
constantSpecInfo
  :: (HasCallStack, Distinct n)
  => TransformContext n
  -> Term n
  -> NormalizeSession (ConstantSpecInfo n)
constantSpecInfo ctx = constantSpecInfoIn (mkDerivedName ctx) (tfScope ctx)

-- | 'constantSpecInfo' in a scope that may extend that of the context,
-- with the context's way of naming new binders
constantSpecInfoIn
  :: (HasCallStack, Distinct n)
  => (C.OccName -> C.Name Term)
  -> Scope n
  -> Term n
  -> NormalizeSession (ConstantSpecInfo n)
constantSpecInfoIn mkNm sc e = do
  te <- Lens.view typeEnv
  -- Don't constant spec clocks or resets, they're either:
  --
  --  * A simple wire (Var), therefore not interesting to spec
  --  * A clock/reset generator, and speccing a generator weirds out HDL simulators.
  if isClockOrReset te sc (inferCoreTypeOf te sc e) then
    case collectArgs e of
      (Prim p, _)
        | primName p == primName removedArg ->
          pure (constantCsr sc e)
      _ -> bindCsr mkNm sc e
  else
    case collectArgsTicks e of
      (dc@(Data _), args, ticks) ->
        mergeCsrs mkNm sc ticks e dc args

      (prim@(Prim _), args, ticks) ->
        mergeCsrs mkNm sc ticks e prim args >>= \case
          csr@(ConstantSpecInfo _ TNil _ _) -> pure csr
          _ -> bindCsr mkNm sc e

      (Lam _ _, _, _) ->
        if not (isClosed e) then
          bindCsr mkNm sc e
        else
          pure (constantCsr sc e)

      (var@(GlobalVar f), args, ticks) -> do
        (curF, _) <- Lens.use curFun
        isNonRecGlobVar <- isNonRecursiveGlobalVar e
        if isNonRecGlobVar && f /= curF then
          mergeCsrs mkNm sc ticks e var args >>= \case
            csr@(ConstantSpecInfo _ TNil _ _) -> pure csr
            _ -> bindCsr mkNm sc e
        else
          bindCsr mkNm sc e

      (Literal _, _, _) ->
        pure (constantCsr sc e)

      _ ->
        bindCsr mkNm sc e

-- | A fully constant term: binds nothing
constantCsr :: Distinct n => Scope n -> Term n -> ConstantSpecInfo n
constantCsr sc e = ConstantSpecInfo sc TNil e True

-- | Bind a term to a new variable: it is fully non-constant
bindCsr
  :: Distinct n
  => (C.OccName -> C.Name Term)
  -> Scope n
  -> Term n
  -> NormalizeSession (ConstantSpecInfo n)
bindCsr mkNm sc oldTerm = do
  te <- Lens.view typeEnv
  pure $ withFreshIdBinder sc (mkNm "bindCsr") (inferCoreTypeOf te sc oldTerm) $ \sc' b ->
    ConstantSpecInfo sc' (TCons (NonRec b oldTerm) TNil) (idBinderVar b) False

-- | Combine the constant parts of the arguments of a data constructor,
-- primitive or global. The new let-bindings of an argument are in scope of
-- those of the next one, so that their binders are fresh with respect to one
-- another.
mergeCsrs
  :: forall n
   . (HasCallStack, Distinct n)
  => (C.OccName -> C.Name Term)
  -> Scope n
  -> [TickInfo n]
  -- ^ Ticks to wrap around proposed new term
  -> Term n
  -- ^ \"Old\" term
  -> Term n
  -- ^ The head of the application
  -> [Either (Term n) (Type n)]
  -- ^ Arguments
  -> NormalizeSession (ConstantSpecInfo n)
mergeCsrs mkNm sc ticks oldTerm hd = go sc TNil [] []
 where
  go
    :: DExt n x
    => Scope x
    -> Tele Bind n x
    -> [Either (Term x) (Type x)]
    -- ^ The arguments so far, in reverse order
    -> [Bool]
    -- ^ Whether constant parts were found in the term arguments so far
    -> [Either (Term n) (Type n)]
    -> NormalizeSession (ConstantSpecInfo n)
  go scx binds accRev founds = \case
    Right ty : rest -> go scx binds (Right (sink ty) : accRev) founds rest
    Left t : rest -> constantSpecInfoIn mkNm scx (sink t) >>= \case
      ConstantSpecInfo scy binds' t' found ->
        go scy (appendTele binds binds') (Left t' : map (bimap sink sink) accRev)
           (found : founds) rest
    []
      -- If any arguments are constant (and hence can be constant specced), a
      -- new term is created with these constants left in, but variable parts
      -- let-bound. There's one edge case: whenever a term has _no_ arguments.
      -- This happens for constructors without fields, or -depending on their
      -- WorkInfo- primitives without args. We still set 'csrFoundConstant',
      -- because we know the newly proposed term will be fully constant.
      | null founds || or founds ->
          let newTerm = mkApps (sink hd) (reverse accRev)
          in  pure (ConstantSpecInfo scx binds (mkTicks newTerm (sink1 ticks)) True)
      | otherwise ->
          -- No constructs were found to be constant, so we might as well refer
          -- to the whole thing with a new let-binding (instead of creating a
          -- number of "smaller" let-bindings)
          bindCsr mkNm sc oldTerm

-- * Call graphs

-- | A call graph counts the number of occurrences of every global @g@ in the
-- body of a global @f@.
type CallGraph = GlobalEnv (GlobalEnv Word)

-- | All globals mentioned in a call graph
callGraphGlobals :: CallGraph -> GlobalSet
callGraphGlobals cg =
  unionsGlobalSet (globalEnvKeysSet cg : map globalEnvKeysSet (globalEnvElems cg))

-- | Create a call graph for a set of global binders, given a root: the call
-- graph of the globals reachable from the root.
callGraph :: GlobalEnv (Binding (Term 'VoidS)) -> GlobalId -> CallGraph
callGraph bndrs = go emptyGlobalEnv
 where
  go cg root
    | not (memberGlobalEnv root cg)
    , Just rootTm <- lookupGlobalEnv root bndrs =
    let used = globalOccurrences (bindingTerm rootTm)
        cg' = insertGlobalEnv root used cg
    in  List.foldl' go cg' (globalEnvKeys used)
  go cg _ = cg

-- | The number of occurrences of every global in a term, including in the
-- terms of ticks
globalOccurrences :: Term n -> GlobalEnv Word
globalOccurrences = go emptyGlobalEnv
 where
  go :: GlobalEnv Word -> Term x -> GlobalEnv Word
  go !acc = \case
    Var {} -> acc
    GlobalVar g -> insertWithGlobalEnv (+) g 1 acc
    Data {} -> acc
    Literal {} -> acc
    Prim {} -> acc
    Lam _ e -> go acc e
    TyLam _ e -> go acc e
    App e1 e2 -> go (go acc e1) e2
    TyApp e _ -> go acc e
    Let (NonRec _ x) e -> go (go acc x) e
    Let (Rec _ xs) e -> List.foldl' go (go acc e) xs
    Case e _ alts -> List.foldl' (\a (Alt _ e') -> go a e') (go acc e) alts
    Cast e _ _ -> go acc e
    Tick (Attributes _ e') e -> go (go acc e') e
    Tick _ e -> go acc e

-- * Classifying functions

-- | Give a "performance/size" classification of a function in normal form.
classifyFunction :: Term n -> TermClassification
classifyFunction = go (TermClassification 0 0 0)
 where
  go :: TermClassification -> Term x -> TermClassification
  go !c = \case
    Lam _ e -> go c e
    TyLam _ e -> go c e
    -- Only the right-hand sides of let-bindings are classified, not the body
    Let (NonRec _ x) _ -> go c x
    Let (Rec _ xs) _ -> List.foldl' go c xs
    e@App {} -> case fst (collectArgs e) of
      Prim {} -> c & primitive +~ 1
      Var {} -> c & function +~ 1
      GlobalVar {} -> c & function +~ 1
      _ -> c
    Case _ _ alts -> case alts of
      (_:_:_) -> c & selection +~ 1
      _ -> c
    Tick _ e -> go c e
    _ -> c

-- | Whether a function adds little hardware: it has at most one function, at
-- most one primitive or at most one selection (multiplexer), and none of the
-- other two.
--
-- It is considered expensive when it has 2 or more of the following
-- components:
--
-- * functions
-- * primitives
-- * selections (multiplexers)
isCheapFunction :: Term n -> Bool
isCheapFunction tm = case classifyFunction tm of
  TermClassification {..}
    | _function  <= 1 -> _primitive <= 0 && _selection <= 0
    | _primitive <= 1 -> _function  <= 0 && _selection <= 0
    | _selection <= 1 -> _function  <= 0 && _primitive <= 0
    | otherwise       -> False

-- * Normalizing global binders

-- | The normalization strategy ('normalizationStrategy')
getNormalization :: NormalizeSession (Rewrite NormalizeState)
getNormalization = Lens.use (extra.normalizationStrategy)

-- | Normalize a global binder, caching the result in 'normalized'. If
-- the binder is a top entity, its type equality constraints are substituted
-- first.
--
-- The body doesn't need to be deshadowed: the body of a global is closed, and
-- binders are fresh once they are opened.
normalizeTopLvlBndr
  :: HasCallStack
  => Bool
  -- ^ Whether the binder is a top entity, whose type equality constraints are
  -- substituted (see 'substWithTyEq')
  -> GlobalId
  -> Binding (Term 'VoidS)
  -> NormalizeSession (Binding (Term 'VoidS))
normalizeTopLvlBndr isTop nm (Binding nm' sp inl pr tm _) = do
  cached <- lookupGlobalEnv nm <$> Lens.use (extra.normalized)
  case cached of
    Just b -> pure b
    Nothing -> do
      te <- Lens.view typeEnv
      let nmS = showPpr (globalIdName nm)
          tm1 = if isTop then substWithTyEq emptyScope tm else tm
      old <- Lens.use curFun
      normalization <- getNormalization
      tm2 <- rewriteExpr ("normalization", normalization) (nmS, tm1) (nm', sp)
      curFun .= old
      let ty' = inferCoreTypeOf te emptyScope tm2
          r' = nm' `memberGlobalSet` termGlobalIds tm2
          b = Binding (mkGlobalId (globalIdName nm') ty') sp inl pr tm2 r'
      (extra.normalized) %= insertGlobalEnv nm b
      pure b

-- | Normalize a global function applied to closed arguments, i.e. a work-free
-- application such as @f \@7 dict@.
-- 'Clash.Normalize.Transformations.Inline.inlineWorkFree' inlines the
-- result at every occurrence of the application. Results are cached in
-- 'workFreeAppCache'.
--
-- Unlike unapplied work-free binders (see
-- 'Clash.Rewrite.Types.inlineWFCacheLimit'), applications are cached
-- regardless of the size of @f@: most of the work is in the arguments and in
-- the functions @f@ calls, so even tiny functions such as dictionary
-- selectors benefit.
--
-- Note that a normalized constant can look different from its unnormalized
-- form, see Note [bignum literals and specialization] in
-- "Clash.Normalize.Transformations.Specialize".
--
-- The arguments must be closed. The application doesn't need to be
-- deshadowed: its parts are closed.
normalizeWorkFreeApp
  :: (HasCallStack, Distinct n)
  => GlobalId
  -- ^ Global function
  -> Binding (Term 'VoidS)
  -- ^ Binding of the global function. Only used if the application isn't
  -- cached yet.
  -> [Either (Term n) (Type n)]
  -- ^ Closed arguments
  -> NormalizeSession (Term n)
normalizeWorkFreeApp f b args = do
  -- Caching is sound because the application is closed: it means the same in
  -- any context, so its normalized form is a valid replacement anywhere. The
  -- cache is keyed on the application modulo alpha-equivalence, which suffices
  -- because the binding of @f@ never changes during normalization. Every
  -- occurrence still gets its own copy, so the circuit doesn't change. Only
  -- names in the HDL can: binders lifted out of the application are named
  -- after @f@, not after the function the application occurs in.
  cache <- Lens.use (extra.workFreeAppCache)
  case Map.lookup key cache of
    Just tm -> pure (sinkClosed tm)
    Nothing -> do
      te <- Lens.view typeEnv
      -- The application is normalized as if it were the body of a binder of
      -- its own. That binder is never referenced, so it isn't added to the
      -- global bindings. It is named after 'f', so binders lifted out of the
      -- application are named after 'f' too.
      g <- freshGlobalId (globalIdName f)
             (inferCoreTypeOf te emptyScope (mkApps (GlobalVar f) closedArgs))
      let -- Normalize the beta-redex rather than the application: normalizing
          -- that would bring us right back here
          redex = mkApps (bindingTerm b) closedArgs
      old <- Lens.use curFun
      normalization <- getNormalization
      tm <- rewriteExpr ("normalization", normalization)
                        (showPpr (globalIdName f), redex) (g, bindingLoc b)
      curFun .= old
      (extra.workFreeAppCache) %= Map.insert key tm
      pure (sinkClosed tm)
 where
  key = (f, map (bimap AlphaTerm AlphaType) closedArgs)
  closedArgs = map (bimap (notClosed . closeTerm) (notClosed . closeType)) args
  notClosed :: Maybe a -> a
  notClosed = fromMaybe (error "normalizeWorkFreeApp: an argument is not closed")

-- | Rewrite the closed body of a global binder according to the provided
-- transformation.
--
-- It doesn't lint its input with @-fclash-debug-invariants-shadowing@: in
-- scoped Core, binders never shadow one another, and every variable is in
-- scope.
rewriteExpr
  :: (String, Rewrite NormalizeState)
  -- ^ Transformation to apply
  -> (String, Term 'VoidS)
  -- ^ Term to transform
  -> (GlobalId, SrcSpan)
  -- ^ Renew current function being rewritten
  -> NormalizeSession (Term 'VoidS)
rewriteExpr (nrwS, nrw) (bndrS, expr) (nm, sp) = do
  curFun .= (nm, sp)
  opts <- Lens.view debugOpts
  let traceFinal = hasTransformationInfo FinalTerm opts
      expr' = traceIf traceFinal
                (bndrS ++ " before " ++ nrwS ++ ":\n\n" ++ showPpr expr ++ "\n")
                expr
  rewritten <- runRewrite nrwS emptyScope nrw expr'
  traceIf traceFinal
    (bndrS ++ " after " ++ nrwS ++ ":\n\n" ++ showPpr rewritten ++ "\n") $
    pure rewritten

-- * Type equality constraints

-- | Turn type equality constraints into substitutions and apply them. So
-- given:
--
-- > /\dom . \(eq : dom ~ "System") . \(eta : Signal dom Bool) . eta
--
-- it substitutes @[dom := "System"]@ and gives:
--
-- > \(eq : "System" ~ "System") . \(eta : Signal "System" Bool) . eta
--
-- Occurrences of @eq@ are replaced by 'removedArg'.
--
-- It looks at the lambdas and type lambdas at the head of the term, in
-- order. At a lambda whose type is @tv ~ ty@ or @ty ~ tv@, where @tv@ is
-- bound by one of the type lambdas before it, it removes the type lambda of
-- @tv@ and substitutes @ty@ for @tv@ in its scope. Constraints are not
-- revisited: one that couldn't be substituted stays, even if a later
-- substitution would allow it.
--
-- Two subtleties, which the types of scoped Core force:
--
-- * It substitutes in everything in scope of @tv@, including the binders
--   between the type lambda and the lambda of the constraint, e.g. @c@ in
--   @/\\dom. \\(c :: KnownDomain dom). \\(eq : dom ~ System). ...@.
--   Substituting only after the lambda of the constraint would leave @dom@
--   free in the type of @c@, which can't be written in scoped Core: the type
--   of @c@ is in scope of @dom@.
--
-- * It only substitutes if that is well-scoped: if @ty@ is in scope at the
--   type lambda of @tv@, after moving that type lambda inwards past the
--   binders that don't mention @tv@. E.g. in
--   @/\\dom. \\(x :: dom). /\\a. \\(eq : dom ~ a). ...@ it keeps the
--   constraint: the type of @x@ can't mention @a@, and removing the type
--   lambda of @dom@ would leave it free in the type of @x@.
--
-- __NB:__ Users of this function should ensure it's only applied to TopEntities
substWithTyEq :: (HasCallStack, Distinct n) => Scope n -> Term n -> Term n
substWithTyEq sc = go 0
 where
  go i e = case findTmEq sc i e of
    Nothing -> e
    Just (j, k) -> case removeTyLamAt sc j k e of
      -- The lambda of the constraint moved to position k - 1
      Just e' -> go k (replaceEqAt sc (k - 1) e')
      Nothing -> go (k + 1) e

-- | The type equivalent of 'substWithTyEq', with the same subtleties. In
-- particular it substitutes in the constraint itself, e.g.
-- @forall dom. (dom ~ System) -> Signal dom Bool@ becomes
-- @(System ~ System) -> Signal System Bool@, instead of leaving @dom@ free in
-- the constraint. Also, it only substitutes type variables bound by the type
-- itself, which for closed types are all of them.
tvSubstWithTyEq :: (HasCallStack, Distinct n) => Scope n -> Type n -> Type n
tvSubstWithTyEq sc = go 0
 where
  go i ty = case findTyEq sc i ty of
    Nothing -> ty
    Just (j, k) -> case removeForAllAt sc j k ty of
      Just ty' -> go k ty'
      Nothing -> go (k + 1) ty

-- | The variable and the type of a type equality constraint @tv ~ ty@ or
-- @ty ~ tv@
eqConstraint :: Type n -> Maybe (TyName n, Type n)
eqConstraint (tyView -> TyConApp tc args)
  | isEqTyCon tc = tvFirst args
 where
  isEqTyCon :: C.TyConName -> Bool
  isEqTyCon tcNm = C.nameUniq tcNm == fromGhcUnique eqTyConKey

  -- Type equality (~) is symmetrical, so users could write: (dom ~ System) or
  -- (System ~ dom)
  tvFirst [_, VarTy _ _ tv, ty] = Just (tv, ty)
  tvFirst [_, ty, VarTy _ _ tv] = Just (tv, ty)
  tvFirst _ = Nothing
eqConstraint _ = Nothing

-- | The positions @(j, k)@ of the first lambda at position @k >= i@ (counting
-- lambdas and type lambdas at the head of the term) whose type is an equality
-- constraint on the type variable that the type lambda at position @j@ binds
findTmEq :: Distinct n => Scope n -> Int -> Term n -> Maybe (Int, Int)
findTmEq sc0 i = go sc0 IntMap.empty 0
 where
  -- Opening the binders makes their names distinct, so that type variables
  -- can be compared by name
  go :: Distinct x => Scope x -> IntMap.IntMap Int -> Int -> Term x -> Maybe (Int, Int)
  go sc tvs p = \case
    TyLam b e -> openTyBinder sc b e $ \sc' b' e' ->
      go sc' (IntMap.insert (tyNameRaw (tyNameOf b')) p tvs) (p + 1) e'
    Lam b e
      | p >= i
      , Just (tv, _) <- eqConstraint (idBinderType b)
      , Just j <- IntMap.lookup (tyNameRaw tv) tvs
      -> Just (j, p)
      | otherwise -> openIdBinder sc b e $ \sc' _ e' -> go sc' tvs (p + 1) e'
    _ -> Nothing

-- | The type version of 'findTmEq', counting quantified type variables and
-- argument types
findTyEq :: Distinct n => Scope n -> Int -> Type n -> Maybe (Int, Int)
findTyEq sc0 i = go sc0 IntMap.empty 0
 where
  go :: Distinct x => Scope x -> IntMap.IntMap Int -> Int -> Type x -> Maybe (Int, Int)
  go sc tvs p = \case
    ForAllTy b ty -> openTyBinder sc b ty $ \sc' b' ty' ->
      go sc' (IntMap.insert (tyNameRaw (tyNameOf b')) p tvs) (p + 1) ty'
    (tyView -> FunTy arg res)
      | p >= i
      , Just (tv, _) <- eqConstraint arg
      , Just j <- IntMap.lookup (tyNameRaw tv) tvs
      -> Just (j, p)
      | otherwise -> go sc tvs (p + 1) res
    _ -> Nothing

-- | Apply a function to the term under the first @i@ lambdas and type lambdas
-- of a term
underBndrs
  :: Distinct n
  => Scope n
  -> Int
  -> (forall l. Distinct l => Scope l -> Term l -> Maybe (Term l))
  -> Term n
  -> Maybe (Term n)
underBndrs sc i f e
  | i <= 0 = f sc e
  | otherwise = case e of
      Lam b body -> openIdBinder sc b body $ \sc' b' body' ->
        Lam b' <$> underBndrs sc' (i - 1) f body'
      TyLam b body -> openTyBinder sc b body $ \sc' b' body' ->
        TyLam b' <$> underBndrs sc' (i - 1) f body'
      _ -> Nothing

-- | Open the first @i@ lambdas and type lambdas of a term, see 'openBndrs'
openHead
  :: Distinct n
  => Scope n
  -> Int
  -> Term n
  -> (forall l. DExt n l => Scope l -> Tele AbsBinder n l -> Term l -> r)
  -> r
openHead sc i e k
  | i <= 0 = k sc TNil e
  | otherwise = case e of
      Lam b body -> openIdBinder sc b body $ \sc1 b' body' ->
        openHead sc1 (i - 1) body' $ \sc2 bs e' -> k sc2 (TCons (AbsId b') bs) e'
      TyLam b body -> openTyBinder sc b body $ \sc1 b' body' ->
        openHead sc1 (i - 1) body' $ \sc2 bs e' -> k sc2 (TCons (AbsTy b') bs) e'
      _ -> k sc TNil e

-- | Remove the type lambda at position @j@ for the equality constraint of the
-- lambda at position @k@, see 'substWithTyEq'
removeTyLamAt :: Distinct n => Scope n -> Int -> Int -> Term n -> Maybe (Term n)
removeTyLamAt sc j k = underBndrs sc j $ \sc' -> \case
  TyLam b body -> removeTyLam sc' b body (k - j - 1)
  _ -> Nothing

-- | Remove a type lambda for the equality constraint of the lambda that
-- follows it after @m@ other binders. Substitutes the type of the constraint
-- if it is in scope of the type lambda ('substTyLam'), and otherwise tries
-- again after moving the type lambda inwards past the next binder, if that
-- binder doesn't mention the type variable.
removeTyLam :: Distinct n => Scope n -> TyBinder n l -> Term l -> Int -> Maybe (Term n)
removeTyLam sc b0 body0 m = openTyBinder sc b0 body0 $ \scB b body ->
  case substTyLam sc scB b body m of
    Just e -> Just e
    Nothing
      | m <= 0 -> Nothing
      | otherwise -> case body of
          Lam c r -> do
            cTy <- unsinkType (tyBinderNameBinders b) (idBinderType c)
            withFreshIdBinder sc (idBinderOcc c) cTy $ \sc1 c' ->
              withFreshTyBinder sc1 (tyBinderOcc b) (sink (tyBinderKind b)) $ \sc2 b' ->
                let s = extendTmSubst
                          (extendTySubst (extendSubstScope sc2 (idSubst sc)) b (tyBinderVar b'))
                          c (sink (idBinderVar c'))
                in  Lam c' <$> removeTyLam sc1 b' (substTm s r) (m - 1)
          TyLam c r -> do
            cKind <- unsinkType (tyBinderNameBinders b) (tyBinderKind c)
            withFreshTyBinder sc (tyBinderOcc c) cKind $ \sc1 c' ->
              withFreshTyBinder sc1 (tyBinderOcc b) (sink (tyBinderKind b)) $ \sc2 b' ->
                let s = extendTySubst
                          (extendTySubst (extendSubstScope sc2 (idSubst sc)) b (tyBinderVar b'))
                          c (sink (tyBinderVar c'))
                in  TyLam c' <$> removeTyLam sc1 b' (substTm s r) (m - 1)
          _ -> Nothing

-- | Remove a type lambda by substituting the type of the equality constraint
-- of the lambda that follows it after @m@ other binders, if that type is in
-- scope of the type lambda
substTyLam
  :: (Distinct n, DExt n l)
  => Scope n -> Scope l -> TyBinder n l -> Term l -> Int -> Maybe (Term n)
substTyLam sc scB b body m = openHead scB m body $ \_ pre rest -> case rest of
  Lam eqB _
    | teleLength pre == m
    , Just (tv, ty) <- eqConstraint (idBinderType eqB)
    , tyNameRaw tv == tyNameRaw (tyNameOf b)
    , Just ty' <- unsinkType (tyBinderNameBinders b `appendNameBinders` teleNameBinders pre) ty
    -> Just (substTm (extendTySubst (idSubst sc) b ty') body)
  _ -> Nothing

-- | Replace the occurrences of the variable of the lambda at position @i@ by
-- 'removedArg', see 'substWithTyEq'
replaceEqAt :: HasCallStack => Distinct n => Scope n -> Int -> Term n -> Term n
replaceEqAt sc0 i e0 = case underBndrs sc0 i replace e0 of
  Just e -> e
  Nothing -> error "replaceEqAt: no lambda at the given position"
 where
  replace :: Distinct x => Scope x -> Term x -> Maybe (Term x)
  replace sc = \case
    Lam b body -> openIdBinder sc b body $ \sc' b' body' ->
      let removed = TyApp (Prim removedArg) (sink (idBinderType b'))
          s = extendTmSubst (extendSubstScope sc' (idSubst sc)) b' removed
      in  Just (Lam b' (substTm s body'))
    _ -> Nothing

-- | Apply a function to the type under the first @i@ quantified type
-- variables and argument types of a type
underPi
  :: Distinct n
  => Scope n
  -> Int
  -> (forall l. Distinct l => Scope l -> Type l -> Maybe (Type l))
  -> Type n
  -> Maybe (Type n)
underPi sc i f ty
  | i <= 0 = f sc ty
  | otherwise = case ty of
      ForAllTy b r -> openTyBinder sc b r $ \sc' b' r' ->
        ForAllTy b' <$> underPi sc' (i - 1) f r'
      (tyView -> FunTy a r) -> mkFunTy a <$> underPi sc (i - 1) f r
      _ -> Nothing

-- | Open the first @i@ quantified type variables and argument types of a type
openPi
  :: Distinct n
  => Scope n
  -> Int
  -> Type n
  -> (forall l. DExt n l => Scope l -> PiTele n l -> Type l -> r)
  -> r
openPi sc i ty k
  | i <= 0 = k sc PiNil ty
  | otherwise = case ty of
      ForAllTy b r -> openTyBinder sc b r $ \sc1 b' r' ->
        openPi sc1 (i - 1) r' $ \sc2 tele res -> k sc2 (PiForall b' tele) res
      (tyView -> FunTy a r) ->
        openPi sc (i - 1) r $ \sc2 tele res -> k sc2 (PiArg a tele) res
      _ -> k sc PiNil ty

-- | The number of quantified type variables and argument types of a telescope
piTeleLength :: PiTele n l -> Int
piTeleLength = go 0
 where
  go :: Int -> PiTele x y -> Int
  go !acc = \case
    PiNil -> acc
    PiForall _ rest -> go (acc + 1) rest
    PiArg _ rest -> go (acc + 1) rest

-- | The type version of 'removeTyLamAt'
removeForAllAt :: Distinct n => Scope n -> Int -> Int -> Type n -> Maybe (Type n)
removeForAllAt sc j k = underPi sc j $ \sc' -> \case
  ForAllTy b r -> removeForAll sc' b r (k - j - 1)
  _ -> Nothing

-- | The type version of 'removeTyLam'
removeForAll :: Distinct n => Scope n -> TyBinder n l -> Type l -> Int -> Maybe (Type n)
removeForAll sc b0 r0 m = openTyBinder sc b0 r0 $ \scB b r ->
  case substForAll sc scB b r m of
    Just ty -> Just ty
    Nothing
      | m <= 0 -> Nothing
      | otherwise -> case r of
          ForAllTy c r' -> do
            cKind <- unsinkType (tyBinderNameBinders b) (tyBinderKind c)
            withFreshTyBinder sc (tyBinderOcc c) cKind $ \sc1 c' ->
              withFreshTyBinder sc1 (tyBinderOcc b) (sink (tyBinderKind b)) $ \sc2 b' ->
                let s = extendTySubst
                          (extendTySubst (extendSubstScope sc2 (idSubst sc)) b (tyBinderVar b'))
                          c (sink (tyBinderVar c'))
                in  ForAllTy c' <$> removeForAll sc1 b' (substTy s r') (m - 1)
          (tyView -> FunTy a r') -> do
            -- An argument type doesn't bind anything: just move it out
            a' <- unsinkType (tyBinderNameBinders b) a
            mkFunTy a' <$> removeForAll sc b r' (m - 1)
          _ -> Nothing

-- | The type version of 'substTyLam'
substForAll
  :: (Distinct n, DExt n l)
  => Scope n -> Scope l -> TyBinder n l -> Type l -> Int -> Maybe (Type n)
substForAll sc scB b r m = openPi scB m r $ \_ pre rest -> case rest of
  (tyView -> FunTy eqArg _)
    | piTeleLength pre == m
    , Just (tv, ty) <- eqConstraint eqArg
    , tyNameRaw tv == tyNameRaw (tyNameOf b)
    , Just ty' <- unsinkType (tyBinderNameBinders b `appendNameBinders` piTeleNameBinders pre) ty
    -> Just (substTy (extendTySubst (idSubst sc) b ty') r)
  _ -> Nothing
