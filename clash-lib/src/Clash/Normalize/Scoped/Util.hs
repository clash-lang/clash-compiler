{-|
  Copyright  :  (C) 2012-2016, University of Twente,
                    2021-2026, QBayLogic B.V.
  License    :  BSD2 (see the file LICENSE)
  Maintainer :  QBayLogic B.V. <devops@qbaylogic.com>

  Utility functions of the normalization transformations on scoped Clash Core
  ("Clash.Core.Scoped.Syntax"): the ported counterparts of
  "Clash.Normalize.Util".

  = Globals

  Tables of the normalizer that are keyed by global binders (the recursive
  components and the inline history) are looked up by 'GlobalId', so that a
  local variable can never hit an entry of a global with the same unique.
  The tables themselves are still those of Clash Core, in 'NormalizeState'.

  = The boundary with Clash Core

  During the port, the global bindings ('Clash.Rewrite.Types.bindings') are
  those of Clash Core. Phase 4 of the port, which runs the normalizer on
  scoped Core, changes that.
-}

{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}

module Clash.Normalize.Scoped.Util
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
    -- * Call graphs
  , CallGraph
  , callGraph
  , callGraphGlobals
    -- * Classifying functions
  , classifyFunction
  , isCheapFunction
    -- * Type equality constraints
  , substWithTyEq
  , tvSubstWithTyEq
  ) where

import           Control.Lens                ((&), (+~), (%=))
import qualified Control.Lens                as Lens
import           Control.Monad.Extra         (anyM)
import qualified Data.IntMap.Strict          as IntMap
import qualified Data.List                   as List
import qualified Data.Text                   as Text
import           GHC.Builtin.Names           (eqTyConKey)
import           GHC.Stack                   (HasCallStack)

import qualified Clash.Core.FreeVars         as C (globalIdOccursIn)
import qualified Clash.Core.Name             as C
import qualified Clash.Core.Term             as C
import qualified Clash.Core.TyCon            as C (TyConName)
import qualified Clash.Core.Type             as C
import qualified Clash.Core.Var              as C
import           Clash.Core.VarEnv
  (extendVarEnv, extendVarEnvWith, lookupVarEnv, unitVarEnv)
import           Clash.Driver.Types          (Binding (..))
import           Clash.Normalize.Primitives  (removedArg)
import           Clash.Normalize.Types
import           Clash.Normalize.Util        (isConstantArg)
import           Clash.Rewrite.Types         (bindings, extra)
import           Clash.Unique                (fromGhcUnique)

import           Clash.Core.Scoped.FreeVars  (teleNameBinders)
import           Clash.Core.Scoped.NameMap
import           Clash.Core.Scoped.Subst
import           Clash.Core.Scoped.Syntax
import           Clash.Core.Scoped.Term
import           Clash.Core.Scoped.Type
  (PiTele (..), TypeView (..), mkFunTy, piTeleNameBinders, tyView)
import           Clash.Rewrite.Scoped.Types  (FrameKind (..), TransformContext, ctxFrames)

-- * Constant arguments of primitives

-- | Whether the context of a subterm indicates that it is to be reduced to a
-- constant: whether it is an argument of a primitive that wants that argument
-- to be constant (see 'isConstantArg'). See 'Clash.Normalize.Util.shouldReduce'.
shouldReduce :: TransformContext n -> NormalizeSession Bool
shouldReduce = anyM isConstantArg' . ctxFrames
 where
  isConstantArg' (KAppArg (Just (nm, _, i))) = isConstantArg nm i
  isConstantArg' _ = pure False

-- * Inlining

-- | How often a global was inlined into another one. See
-- 'Clash.Normalize.Util.alreadyInlined'.
alreadyInlined
  :: GlobalId
  -- ^ Function we want to inline
  -> GlobalId
  -- ^ Function in which we want to perform the inlining
  -> NormalizeMonad (Maybe Int)
alreadyInlined f cf = do
  inlinedHM <- Lens.use inlineHistory
  pure (lookupVarEnv (globalIdVar f) =<< lookupVarEnv (globalIdVar cf) inlinedHM)

-- | Record a new inlining in the 'inlineHistory'. See
-- 'Clash.Normalize.Util.addNewInline'.
addNewInline
  :: GlobalId
  -- ^ Function we're inlining
  -> GlobalId
  -- ^ Function in which we're inlining it
  -> NormalizeMonad ()
addNewInline (globalIdVar -> f) (globalIdVar -> cf) =
  inlineHistory %= extendVarEnvWith
                     cf
                     (unitVarEnv f 1)
                     (\_ hm -> extendVarEnvWith f 1 (+) hm)

-- | A tick to prefix an inlined expression with the name of the global it
-- came from, see 'Clash.Normalize.Util.mkInlineTick'
mkInlineTick :: GlobalId -> TickInfo n
mkInlineTick n = NameMod C.PrefixName (LitTy (C.SymTy (toStr n)))
 where
  toStr = Text.unpack . snd . Text.breakOnEnd "." . C.nameOcc . C.varName . globalIdVar

-- * Recursive globals

-- | Whether a term is an application of a non-recursive global. See
-- 'Clash.Normalize.Util.isNonRecursiveGlobalVar'. A local variable is not a
-- global: unlike in Clash Core, they can't be confused.
isNonRecursiveGlobalVar :: Term n -> NormalizeSession Bool
isNonRecursiveGlobalVar (collectArgs -> (GlobalVar g, _)) = not <$> isRecursiveBndr g
isNonRecursiveGlobalVar _ = pure False

-- | Whether a global binder is recursive. Caches its answer in
-- 'recursiveComponents'. See 'Clash.Normalize.Util.isRecursiveBndr'.
isRecursiveBndr :: GlobalId -> NormalizeSession Bool
isRecursiveBndr g = do
  cg <- Lens.use (extra.recursiveComponents)
  case lookupVarEnv f cg of
    Just isR -> pure isR
    Nothing -> do
      fBodyM <- lookupVarEnv f <$> Lens.use bindings
      case fBodyM of
        Nothing -> pure False
        Just b -> do
          -- There are no global mutually-recursive functions, only
          -- self-recursive ones, so checking whether 'f' occurs in its body is
          -- sufficient. The bindings are still those of Clash Core, see the
          -- module header.
          let isR = f `C.globalIdOccursIn` bindingTerm b
          (extra.recursiveComponents) %= extendVarEnv f isR
          pure isR
 where
  f = globalIdVar g

-- * Call graphs

-- | A call graph counts the number of occurrences of every global @g@ in the
-- body of a global @f@. See 'Clash.Normalize.Util.CallGraph'.
type CallGraph = GlobalEnv (GlobalEnv Word)

-- | All globals mentioned in a call graph
callGraphGlobals :: CallGraph -> GlobalSet
callGraphGlobals cg =
  unionsGlobalSet (globalEnvKeysSet cg : map globalEnvKeysSet (globalEnvElems cg))

-- | The call graph of the globals reachable from a root. See
-- 'Clash.Normalize.Util.callGraph'.
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
-- terms of ticks, like 'Clash.Core.FreeVars.globalIds'
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
-- See 'Clash.Normalize.Util.classifyFunction'.
classifyFunction :: Term n -> TermClassification
classifyFunction = go (TermClassification 0 0 0)
 where
  go :: TermClassification -> Term x -> TermClassification
  go !c = \case
    Lam _ e -> go c e
    TyLam _ e -> go c e
    -- Only the right-hand sides, like 'Clash.Normalize.Util.classifyFunction'
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
-- other two. See 'Clash.Normalize.Util.isCheapFunction'.
isCheapFunction :: Term n -> Bool
isCheapFunction tm = case classifyFunction tm of
  TermClassification {..}
    | _function  <= 1 -> _primitive <= 0 && _selection <= 0
    | _primitive <= 1 -> _function  <= 0 && _selection <= 0
    | _selection <= 1 -> _function  <= 0 && _primitive <= 0
    | otherwise       -> False

-- * Type equality constraints

-- | Turn type equality constraints into substitutions and apply them. See
-- 'Clash.Normalize.Util.substWithTyEq'. So given:
--
-- > /\dom . \(eq : dom ~ "System") . \(eta : Signal dom Bool) . eta
--
-- it substitutes @[dom := "System"]@ and gives:
--
-- > \(eq : "System" ~ "System") . \(eta : Signal "System" Bool) . eta
--
-- Occurrences of @eq@ are replaced by 'removedArg'.
--
-- Like 'Clash.Normalize.Util.substWithTyEq', it looks at the lambdas and type
-- lambdas at the head of the term, in order. At a lambda whose type is
-- @tv ~ ty@ or @ty ~ tv@, where @tv@ is bound by one of the type lambdas
-- before it, it removes the type lambda of @tv@ and substitutes @ty@ for
-- @tv@ in its scope. Constraints are not revisited: one that couldn't be
-- substituted stays, even if a later substitution would allow it.
--
-- __Differences__ with 'Clash.Normalize.Util.substWithTyEq', which the types
-- of scoped Core force (TODO(substWithTyEq): see the fix of that function):
--
-- * It substitutes in everything in scope of @tv@, including the binders
--   between the type lambda and the lambda of the constraint, e.g. @c@ in
--   @/\\dom. \\(c :: KnownDomain dom). \\(eq : dom ~ System). ...@.
--   'Clash.Normalize.Util.substWithTyEq' only substitutes after the lambda of
--   the constraint, which leaves @dom@ free in the type of @c@. That can't be
--   written in scoped Core: the type of @c@ is in scope of @dom@.
--
-- * It only substitutes if that is well-scoped: if @ty@ is in scope at the
--   type lambda of @tv@, after moving that type lambda inwards past the
--   binders that don't mention @tv@. E.g. in
--   @/\\dom. \\(x :: dom). /\\a. \\(eq : dom ~ a). ...@ it keeps the
--   constraint: the type of @x@ can't mention @a@.
--   'Clash.Normalize.Util.substWithTyEq' removes the type lambda of @dom@,
--   and leaves it free in the type of @x@.
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

-- | The type equivalent of 'substWithTyEq'. See
-- 'Clash.Normalize.Util.tvSubstWithTyEq'.
--
-- It differs from 'Clash.Normalize.Util.tvSubstWithTyEq' like 'substWithTyEq'
-- differs from 'Clash.Normalize.Util.substWithTyEq' (TODO(substWithTyEq)). In
-- particular it substitutes in the constraint itself, e.g.
-- @forall dom. (dom ~ System) -> Signal dom Bool@ becomes
-- @(System ~ System) -> Signal System Bool@, where
-- 'Clash.Normalize.Util.tvSubstWithTyEq' leaves @dom@ free in the constraint.
-- Also, it only substitutes type variables bound by the type itself, which for
-- closed types are all of them.
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
