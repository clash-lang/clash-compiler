{-|
  Copyright  :  (C) 2012-2016, University of Twente,
                    2016     , Myrtle Software Ltd,
                    2017     , Google Inc.,
                    2021-2026, QBayLogic B.V.
  License    :  BSD2 (see the file LICENSE)
  Maintainer :  QBayLogic B.V. <devops@qbaylogic.com>

  Utilities for rewriting scoped Clash Core ("Clash.Core.Scoped.Syntax"):
  creating binders, the representability of terms and types, inlining,
  lifting and removing let-bindings, recognizing join points, and binding
  what the evaluator allocated.

  = Binders

  Binders get fresh names from the scope, so the functions that create them
  ('mkTmBinderFor', 'mkBinderFor') take a 'Scope' and a continuation. The
  unique of a given name is not used, only its human readable part.

  = Let-bindings

  Let-bindings can't be passed around as lists of binders and right-hand
  sides: the binders are a telescope. The functions
  here work on the bindings of an opened let-expression ('openLetBindings'):
  a telescope of binders, and the bindings as t'LetBinding's in the scope of
  the body. Substituting for some of the binders ('substVars') keeps
  the term in that scope, and 'keepBindings' rebuilds the let-expression in
  the outer scope with the bindings that are left, checking that the other
  binders don't occur anymore.

  = Global bindings

  New global bindings are closed terms, created with
  "Clash.Rewrite.Scoped.Globals". 'liftBinding' abstracts a binding over its
  free variables with 'Clash.Core.Scoped.Subst.closeOver'.
-}

{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ViewPatterns #-}

module Clash.Rewrite.Scoped.Util
  ( -- * Changes
    changed
  , setChanged
  , zoomExtra
    -- * Binders
  , mkDerivedName
  , mkTmBinderFor
  , mkBinderFor
    -- * Global bindings
  , mkFunction
  , addGlobalBind
  , liftBinding
  , abstractOver
  , closeOverOrError
    -- * Types
  , isUntranslatable
  , isUntranslatableType
  , runWithHWTypeCache
  , isFromInt
  , normalizeTermTypes
  , dependencySortTyVars
    -- * Let-bindings
  , LetBinding (..)
  , letBindingVar
  , openLetBindings
  , substVars
  , keepBindings
  , inlineBinders
  , inlineOrLiftBinders
  , removeUnusedBinders
  , isJoinPointIn
  , isVoidWrapper
    -- * Evaluation
  , bindPureHeap
  , cleanupHeapBindings
  ) where

import qualified Control.Lens                as Lens
import           Control.Monad.Extra         (partitionM)
import qualified Control.Monad.Writer        as Writer
import qualified Data.IntMap.Strict          as IntMap
import qualified Data.IntSet                 as IntSet
import qualified Data.List                   as List
import           Data.Maybe                  (catMaybes, fromMaybe, isJust, mapMaybe)
import qualified Data.Monoid                 as Monoid
import qualified Data.Text                   as Text
import           GHC.Stack                   (HasCallStack)
import           GHC.Types.Basic             (InlineSpec (..))

import qualified Clash.Core.Name             as C
import           Clash.Netlist.Util          (representableTypeState)
import           Clash.Rewrite.Types
  (RewriteMonad, curFun, customReprs, scopedTypeEnv, typeTranslator)
import           Clash.Rewrite.Util
  (changed, isFromInt, runWithHWTypeCache, setChanged, zoomExtra)

import           Clash.Core.Scoped.FreeVars
  (bindNameBinders, freeTmVars, teleNameBinders, tmNameOccursIn)
import           Clash.Core.Scoped.HasType   (inferCoreKindOf, inferCoreTypeOf)
import           Clash.Core.Scoped.NameMap
  ( NameMap, emptyNameMap, insertNameMap, nameSetToList, nullNameMap
  , singletonNameMap )
import           Clash.Core.Scoped.Subst
import           Clash.Core.Scoped.Syntax
import           Clash.Core.Scoped.Term
  (AbsBinder (..), collectArgs, mkApps, mkLams, mkTyLams, stripTicks)
import           Clash.Core.Scoped.Type      (mkFunTy, normalizeType)
import           Clash.Core.Scoped.TypeEnv   (TypeEnv)
import           Clash.Rewrite.Scoped.Globals
  (addGlobalBind, findAlphaEqBinding, freshGlobalId, isWorkFreeTerm, mkFunction)
import           Clash.Rewrite.Scoped.Types
  (TransformContext, mkDerivedName, tfScope, underLet)

-- * Binders

-- | Make a binder with a fresh name for a term, of the type of the term.
mkTmBinderFor
  :: (HasCallStack, Distinct n)
  => TypeEnv
  -> Scope n
  -> C.Name a
  -- ^ Name of the new binder. Its unique is not used.
  -> Term n
  -- ^ The term to bind
  -> (forall l. DExt n l => Scope l -> IdBinder n l -> r)
  -> r
mkTmBinderFor te sc nm e = withFreshIdBinder sc (C.castName nm) (inferCoreTypeOf te sc e)

-- | Make a binder with a fresh name for a term or a type, of the type of the
-- term or the kind of the type.
mkBinderFor
  :: (HasCallStack, Distinct n)
  => TypeEnv
  -> Scope n
  -> C.Name a
  -- ^ Name of the new binder. Its unique is not used.
  -> Either (Term n) (Type n)
  -- ^ The term or type to bind
  -> (forall l. DExt n l => Scope l -> AbsBinder n l -> r)
  -> r
mkBinderFor te sc nm arg k = case arg of
  Left e -> mkTmBinderFor te sc nm e (\sc' b -> k sc' (AbsId b))
  Right ty ->
    withFreshTyBinder sc (C.castName nm) (inferCoreKindOf te sc ty) (\sc' b -> k sc' (AbsTy b))

-- * Types

-- | Determine if a term cannot be represented in hardware: whether its type
-- can't be, see 'isUntranslatableType'
isUntranslatable
  :: (HasCallStack, Distinct n)
  => Bool
  -- ^ String representable
  -> Scope n
  -> Term n
  -> RewriteMonad extra Bool
isUntranslatable stringRepresentable sc tm = do
  te <- Lens.view scopedTypeEnv
  isUntranslatableType stringRepresentable sc (inferCoreTypeOf te sc tm)

-- | Determine if a type cannot be represented in hardware
isUntranslatableType
  :: Distinct n
  => Bool
  -- ^ String representable
  -> Scope n
  -> Type n
  -> RewriteMonad extra Bool
isUntranslatableType stringRepresentable sc ty = do
  tt <- Lens.view typeTranslator
  reprs <- Lens.view customReprs
  te <- Lens.view scopedTypeEnv
  not <$> runWithHWTypeCache (representableTypeState tt reprs stringRepresentable te sc ty)
{-# INLINE isUntranslatableType #-}

-- | Normalize the types of a cast, or of a variable occurrence
normalizeTermTypes :: Distinct n => TypeEnv -> Scope n -> Term n -> Term n
normalizeTermTypes te sc e = case e of
  Cast e' ty1 ty2 ->
    Cast (normalizeTermTypes te sc e') (normalizeType te sc ty1) (normalizeType te sc ty2)
  Var occ ty nm -> Var occ (normalizeType te sc ty) nm
  -- TODO other terms?
  _ -> e

-- | Order type variable occurrences such that every type variable comes
-- after the type variables in its kind, keeping their order otherwise, e.g.
-- to bind them with 'Clash.Core.Scoped.Subst.closeOver'.
--
-- Only the dependencies between the given type variables are taken into
-- account. Elements that are not type variables are left in place.
dependencySortTyVars :: HasCallStack => [Type n] -> [Type n]
dependencySortTyVars tvs = reverse (snd (List.foldl' visit (IntSet.empty, []) tvs))
 where
  env = IntMap.fromList [(tyNameRaw nm, tv) | tv@(VarTy _ _ nm) <- tvs]

  visit (seen, acc) tv = case tv of
    VarTy _ k nm
      | tyNameRaw nm `IntSet.member` seen -> (seen, acc)
      | otherwise ->
          let kindTvs = mapMaybe (\o -> IntMap.lookup (occRaw o) env) (freeTyVarOccsOfType k)
              (seen1, acc1) = List.foldl' visit (IntSet.insert (tyNameRaw nm) seen, acc) kindTvs
          in  (seen1, tv : acc1)
    _ -> (seen, tv : acc)

  occRaw = \case
    VarTy _ _ nm -> tyNameRaw nm
    _ -> error "dependencySortTyVars: not a type variable"

-- * Let-bindings

-- | A binding of a let-expression, in the scope @l@ of the body of the
-- let-expression, see 'openLetBindings'
data LetBinding (l :: S) = LetBinding
  { lbOcc :: !(C.Name Term)
  -- ^ The human readable name of the binder
  , lbType :: !(Type l)
  -- ^ The type of the binder
  , lbName :: !(TmName l)
  -- ^ The variable the binder binds
  , lbRhs :: !(Term l)
  -- ^ The right-hand side
  }
  deriving (Show)

-- | The variable a binding binds, as a term
letBindingVar :: LetBinding l -> Term l
letBindingVar (LetBinding occ ty nm _) = Var occ ty nm

-- | Go under the binders of a let-expression, see
-- 'Clash.Core.Scoped.Subst.openBind'. Gives its binders as a telescope, and
-- its bindings in the scope of the body, whether the let-expression is
-- recursive or not: the right-hand side of a non-recursive binding is sunk
-- into that scope.
openLetBindings
  :: Distinct n
  => Scope n
  -> Bind n l
  -> Term l
  -- ^ The body
  -> (forall l'. DExt n l' => Scope l' -> Tele IdBinder n l' -> [LetBinding l'] -> Term l' -> r)
  -> r
openLetBindings sc bind body k = openBind sc bind body $ \sc' bind' body' -> case bind' of
  NonRec b x -> k sc' (TCons b TNil) [letBinding (idBinderVar b) (sink x)] body'
  Rec bs xs -> k sc' bs (zipWith letBinding (teleIdVars bs) xs) body'
 where
  letBinding v x = case v of
    Var occ ty nm -> LetBinding occ ty nm x
    _ -> error "openLetBindings: not a variable"

-- | Substitute terms for some of the variables of the scope, e.g. for binders
-- of a let-expression opened with 'openLetBindings', see
-- 'Clash.Core.Scoped.Subst.tmVarsSubst'. Partially apply it to substitute in
-- several terms.
substVars :: Distinct n => Scope n -> NameMap TmName n (Term n) -> Term n -> Term n
substVars sc m
  | nullNameMap m = id
  | otherwise = substTm (tmVarsSubst sc m)

-- | Rebuild a let-expression opened with 'openLetBindings', with the given
-- bindings, in the given order, as a recursive let-binding. Gives just the
-- body if there are none. 'Nothing' if the binders of the other bindings
-- occur in the right-hand sides of the given ones, or in the body.
--
-- The binders keep their names if they are fresh for the outer scope in the
-- given order ('Clash.Core.Scoped.Subst.rebindIds'), as they are if the
-- bindings keep the order of the telescope. Otherwise they get fresh names in
-- the outer scope, which takes a substitution.
keepBindings
  :: (Distinct n, Distinct l)
  => Scope n
  -> Tele IdBinder n l
  -> [LetBinding l]
  -- ^ The bindings to keep, which are bindings of the telescope
  -> Term l
  -- ^ The body
  -> Maybe (Term n)
keepBindings sc bs kept body
  | not (null kept)
  , Just e <- rebindIds sc bs [(lbOcc lb, strengthenTyIds bs (lbType lb), lbName lb) | lb <- kept]
                (body : map lbRhs kept) rebuild
  = Just e
  | any (`IntSet.member` dropped) used = Nothing
  | null kept = unsinkTerm (teleNameBinders bs) body
  | otherwise =
      withFreshIdBinders sc [(lbOcc lb, strengthenTyIds bs (lbType lb)) | lb <- kept] $
        \sc' bs' vars' -> case vars' of
          [] -> error "keepBindings: no binders"
          dummy : _ ->
            let new = IntMap.fromList (zip keptRaws vars')
                -- The dropped binders don't occur, so their entries are never
                -- used
                entries =
                  [ IntMap.findWithDefault dummy (tmNameRaw nm) new
                  | Var _ _ nm <- teleIdVars bs ]
            in  case extendTmSubstTele (extendSubstScope sc' (idSubst sc)) bs entries of
                  Just s -> Just (Let (Rec bs' [substTm s (lbRhs lb) | lb <- kept]) (substTm s body))
                  Nothing -> error "keepBindings: wrong number of variables"
 where
  rebuild :: Scope l' -> Tele IdBinder n l' -> [Term l'] -> Term n
  rebuild _ bs' = \case
    body' : xs' -> Let (Rec bs' xs') body'
    [] -> error "keepBindings: no body"

  keptRaws = map (tmNameRaw . lbName) kept
  dropped =
    IntSet.fromList [tmNameRaw nm | Var _ _ nm <- teleIdVars bs] `IntSet.difference`
      IntSet.fromList keptRaws
  used
    | IntSet.null dropped = []
    | otherwise = map tmNameRaw (nameSetToList (foldMap (freeTmVars . lbRhs) kept <> freeTmVars body))

-- | Inline the given bindings, of a let-expression opened with
-- 'openLetBindings', into each other. Gives the substitution for the
-- binders that could be inlined, and the bindings that could not, as they
-- refer to themselves after inlining the others, in reverse order.
substituteBinders
  :: Distinct l
  => Scope l
  -> [LetBinding l]
  -> (NameMap TmName l (Term l), [LetBinding l])
substituteBinders scL = go emptyNameMap []
 where
  go m inlRec [] = (m, [lb {lbRhs = substVars scL m (lbRhs lb)} | lb <- inlRec])
  go !m inlRec (lb : rest)
    | lbName lb `tmNameOccursIn` e1 = go m (lb {lbRhs = e1} : inlRec) rest
    | otherwise = go (extendSubstitution scL (lbName lb) e1 m) inlRec rest
   where
    e1 = substVars scL m (lbRhs lb)

-- | Add an entry for a variable to a substitution, substituting it in the
-- other entries too
extendSubstitution
  :: Distinct l
  => Scope l
  -> TmName l
  -> Term l
  -> NameMap TmName l (Term l)
  -> NameMap TmName l (Term l)
extendSubstitution scL x e m =
  insertNameMap x e (fmap (substVars scL (singletonNameMap x e)) m)

-- | Inline the bindings of a let-expression that have a certain property.
-- This is a 'Clash.Rewrite.StrategyDSL.onLet' handler: it receives the
-- let-expression itself, its bindings, and its body.
--
-- The property gets the binding in the scope of the body of the
-- let-expression.
inlineBinders
  :: Distinct n
  => (forall o. Distinct o => Scope o -> LetBinding o -> RewriteMonad extra Bool)
  -- ^ Property test
  -> TransformContext n
  -> Term n
  -- ^ The let-expression
  -> Bind n l
  -> Term l
  -> RewriteMonad extra (Term n)
inlineBinders condition ctx e bind body = case bind of
  NonRec b x
    -- Substitution would be a no-op for a binder that does not occur in the
    -- body, so the (potentially expensive) property test can be skipped.
    | Just body1 <- unsinkIdBinder b body -> changed body1
    | otherwise -> openLetBindings sc bind body $ \scL _ lbs _ -> case lbs of
        [lb] -> do
          inline <- condition scL lb
          if inline
            then changed (substTm (extendTmSubst (idSubst sc) b x) body)
            else return e
        _ -> error "inlineBinders: not a single binding"
  Rec {} -> openLetBindings sc bind body $ \scL bs lbs body1 -> do
    (toInline, toKeep) <- partitionM (condition scL) lbs
    case toInline of
      [] -> return e
      _ -> do
        let (m, toInlRec) = substituteBinders scL toInline
            substM = substVars scL m
            toKeep1 = [lb {lbRhs = substM (lbRhs lb)} | lb <- toKeep]
        case keepBindings sc bs (toInlRec ++ toKeep1) (substM body1) of
          Just e1 -> changed e1
          Nothing -> error "inlineBinders: an inlined binder remains"
 where
  sc = tfScope ctx

-- | Lift a binding of a let-expression opened with 'openLetBindings' to a
-- global function, abstracted over the free variables of the binding. Gives
-- the term that replaces the binder: the global applied to those variables.
--
-- Recursive occurrences of the binder are replaced by that term /before/
-- abstracting over the free variables. The function abstracts over the free
-- type variables first, in the order of their raw names, but every type
-- variable after the type variables in its kind; then over the free term
-- variables, in the order of their raw names.
--
-- If a global binding is alpha-equivalent to the new function, it is used
-- instead.
liftBinding
  :: (HasCallStack, Distinct l)
  => Scope l
  -> LetBinding l
  -- ^ The binding to lift
  -> RewriteMonad extra (Term l)
liftBinding scL (LetBinding occ _ x e) = do
  te <- Lens.view scopedTypeEnv
  (cf, sp) <- Lens.use curFun
  let (tvs0, ids0) = freeVarOccs e
      tvs = dependencySortTyVars (List.sortOn tyOccRaw tvs0)
      ids = List.sortOn tmOccRaw [v | v@(Var _ _ y) <- ids0, y /= x]
      eTy = inferCoreTypeOf te scL e
      gTy = closeOverOrError "liftBinding" tvs ids $ \_ tvTele idTele cl -> case closureType cl eTy of
        Just eTy1 ->
          forAlls tvTele (foldr mkFunTy (strengthenTyIds idTele eTy1) (teleIdBinderTypes idTele))
        Nothing -> error "liftBinding: the type of the binding mentions other type variables"
      nm = (C.appendToName (globalIdName cf) ("_" `Text.append` C.nameOcc occ)) {C.nameSort = C.Internal}
  g <- freshGlobalId nm gTy
  let args = map Right tvs ++ map Left ids
      newExpr = mkApps (GlobalVar g) args
      -- Substitute the recursive calls by the new expression
      e1 = substVars scL (singletonNameMap x newExpr) e
      -- Abstract over the free variables
      newBody = abstractOver "liftBinding" tvs ids e1
  findAlphaEqBinding newBody >>= \case
    -- If no alpha-equivalent global binding exists, add the new one.
    --
    -- We mark this function as internal so that it can be inlined at the very
    -- end of the normalisation pipeline as part of the flattening pass. We
    -- don't inline right away because we are lifting this function at this
    -- moment for a reason! (termination, CSE and DEC oppertunities, ,etc.)
    Nothing -> do
      addGlobalBind g sp NoUserInlinePrag newBody
      pure newExpr
    Just g1 -> pure (mkApps (GlobalVar g1) args)
 where
  forAlls :: Tele TyBinder x y -> Type y -> Type x
  forAlls TNil t = t
  forAlls (TCons b rest) t = ForAllTy b (forAlls rest t)

  tyOccRaw = \case
    VarTy _ _ nm -> tyNameRaw nm
    _ -> error "liftBinding: not a type variable"

  tmOccRaw = \case
    Var _ _ nm -> tmNameRaw nm
    _ -> error "liftBinding: not a variable"

-- | 'closeOver', which errors if it can't bind the variables
closeOverOrError
  :: HasCallStack
  => String
  -- ^ Name of the caller
  -> [Type n]
  -> [Term n]
  -> (forall i l. (DExt 'VoidS i, DExt i l)
        => Scope l -> Tele TyBinder 'VoidS i -> Tele IdBinder i l -> Closure n l -> r)
  -> r
closeOverOrError caller tvs ids k = case closeOver tvs ids k of
  Just r -> r
  Nothing -> error (caller <> ": can't abstract over the free variables")

-- | Abstract a term over variables of its scope, giving a closed term: type
-- lambdas for the type variables, then lambdas for the term variables. Errors
-- if the term mentions other variables, or if 'closeOver' can't bind the
-- variables, e.g. because a type variable comes before the type variables in
-- its kind (see 'dependencySortTyVars').
abstractOver
  :: HasCallStack
  => String
  -- ^ Name of the caller
  -> [Type n]
  -- ^ Occurrences of the type variables
  -> [Term n]
  -- ^ Occurrences of the term variables
  -> Term n
  -> Term 'VoidS
abstractOver caller tvs ids e = closeOverOrError caller tvs ids $ \_ tvTele idTele cl ->
  case closureTerm cl e of
    Just e1 -> mkTyLams (mkLams e1 idTele) tvTele
    Nothing -> error (caller <> ": the term mentions other variables")

-- | Lift the given bindings, of a let-expression opened with
-- 'openLetBindings', to global functions (see 'liftBinding'), and substitute
-- them in each other, the other bindings, and the body.
liftAndSubstituteBinders
  :: (HasCallStack, Distinct l)
  => Scope l
  -> [LetBinding l]
  -- ^ Bindings to lift
  -> [LetBinding l]
  -- ^ Bindings in which to substitute the lifted ones
  -> Term l
  -- ^ Body in which to substitute the lifted ones
  -> RewriteMonad extra ([LetBinding l], Term l)
liftAndSubstituteBinders scL toLift toKeep body = do
  m <- go emptyNameMap toLift
  pure ([lb {lbRhs = substVars scL m (lbRhs lb)} | lb <- toKeep], substVars scL m body)
 where
  go m [] = pure m
  go !m (lb : rest) = do
    let e1 = substVars scL m (lbRhs lb)
    e2 <- liftBinding scL lb {lbRhs = e1}
    if lbName lb `tmNameOccursIn` e2
      then error ("liftAndSubstituteBinders: lifting " <> show (lbOcc lb)
                  <> " created a self-recursive let-binding")
      else go (extendSubstitution scL (lbName lb) e2 m) rest

-- | Inline or lift the bindings of a let-expression that have a certain
-- property. This is a 'Clash.Rewrite.StrategyDSL.onLet' handler: it receives
-- the let-expression itself, its bindings, and its body.
--
-- Both tests get the bindings in the scope of the body of the let-expression,
-- see 'openLetBindings'.
inlineOrLiftBinders
  :: (HasCallStack, Distinct n)
  => (forall o. Distinct o => Scope o -> LetBinding o -> RewriteMonad extra Bool)
  -- ^ Property test
  -> (forall o. Distinct o => [LetBinding o] -> Term o -> LetBinding o -> Bool)
  -- ^ Test whether to lift or inline, given the bindings and the body of the
  -- let-expression, and the binding
  --
  -- * True: inline
  -- * False: lift
  -> TransformContext n
  -> Term n
  -- ^ The let-expression
  -> Bind n l
  -> Term l
  -> RewriteMonad extra (Term n)
inlineOrLiftBinders condition inlineOrLift ctx e bind body =
  openLetBindings sc bind body $ \scL bs lbs body1 -> do
    (toReplace, toKeep) <- partitionM (condition scL) lbs
    case toReplace of
      [] -> return e
      _ -> do
        let (toInline, toLift) = List.partition (inlineOrLift lbs body1) toReplace
            -- We first substitute the binders that we can inline both the
            -- binders that we intend to lift, the other binders, and the body
            (m, toLiftExtra) = substituteBinders scL toInline
            substM = substVars scL m
            toReplace1 = [lb {lbRhs = substM (lbRhs lb)} | lb <- toLift ++ toKeep]
            (toLift1, toKeep1) = splitAt (length toLift) toReplace1
        -- We then substitute the lifted binders in the other binders and the
        -- body
        (toKeep2, body2) <-
          liftAndSubstituteBinders scL (toLiftExtra ++ toLift1) toKeep1 (substM body1)
        case (bind, toKeep2) of
          (NonRec {}, _ : _) -> error "inlineOrLiftBinders: kept a non-recursive binding"
          _ -> case keepBindings sc bs toKeep2 body2 of
            Just e1 -> changed e1
            Nothing -> error "inlineOrLiftBinders: an inlined or lifted binder remains"
 where
  sc = tfScope ctx

-- | Remove the unused bindings of a let-expression. 'Nothing' if all of them
-- are used.
--
-- A binding is used if the body refers to it, or a used binding does. The
-- used bindings of a recursive let-binding keep their order.
removeUnusedBinders
  :: Distinct n
  => Scope n
  -> Bind n l
  -> Term l
  -> Maybe (Term n)
removeUnusedBinders _ (NonRec b _) body = unsinkIdBinder b body
removeUnusedBinders sc bind@(Rec bs0 _) body0 = case unsinkTerm (teleNameBinders bs0) body0 of
  -- The body refers to none of the binders
  Just body -> Just body
  Nothing -> openLetBindings sc bind body0 $ \_ bs lbs body1 ->
    let raws = map (tmNameRaw . lbName) lbs
        used = usedBinders raws (map lbRhs lbs) body1
    in  if IntSet.size used == length raws
          then Nothing
          else case keepBindings sc bs [lb | lb <- lbs, tmNameRaw (lbName lb) `IntSet.member` used] body1 of
            Just e -> Just e
            Nothing -> error "removeUnusedBinders: an unused binder is used"

-- | The raw names of the binders of a recursive let-binding that the body
-- refers to, or a used binding does
usedBinders :: [Int] -> [Term l] -> Term l -> IntSet.IntSet
usedBinders raws xs body = go IntSet.empty (rawsOf body)
 where
  rhss = IntMap.fromList (zip raws xs)
  go acc [] = acc
  go acc (r:rs)
    | r `IntSet.member` acc = go acc rs
    | Just rhs <- IntMap.lookup r rhss = go (IntSet.insert r acc) (rawsOf rhs ++ rs)
    | otherwise = go acc rs

  rawsOf = map tmNameRaw . nameSetToList . freeTmVars

-- | Determine whether the binder of a binding is a join-point created for a
-- complex case expression, in the let-expression that binds it, given by its
-- bindings and body (see 'openLetBindings').
--
-- A join-point is when a local function only occurs in tail-call positions,
-- and when it does, more than once.
isJoinPointIn
  :: TmName l
  -- ^ The binder
  -> [LetBinding l]
  -- ^ The bindings of the let-expression
  -> Term l
  -- ^ The body of the let-expression
  -> Bool
isJoinPointIn x lbs body =
  case letTailCalls (map (tmNameRaw . lbName) lbs) (map (tailCalls r . lbRhs) lbs)
                    (`tailCalls` body) (tailCalls r body) of
    Just k | k > 1 -> True
    _ -> False
 where
  r = tmNameRaw x

-- | Count the number of (only) tail calls of a function, given by its raw
-- name, in an expression. 'Nothing' indicates that the function was used in a
-- non-tail call position.
--
-- A binder with the raw name of the function hides it in its scope.
tailCalls :: Int -> Term x -> Maybe Int
tailCalls r = \case
  Var _ _ nm -> Just (if tmNameRaw nm == r then 1 else 0)
  Lam b e -> under (tmNameRaw (tmNameOf b)) e
  TyLam b e -> under (tyNameRaw (tyNameOf b)) e
  App l a -> case tailCalls r a of
    Just 0 -> tailCalls r l
    _ -> Nothing
  TyApp l _ -> tailCalls r l
  Let (NonRec b x) e ->
    letTailCalls [tmNameRaw (tmNameOf b)] [tailCalls r x] (`tailCalls` e)
      (under (tmNameRaw (tmNameOf b)) e)
  Let (Rec bs xs) e
    | r `elem` raws -> Just 0
    | otherwise -> letTailCalls raws (map (tailCalls r) xs) (`tailCalls` e) (tailCalls r e)
   where
    raws = teleRaws bs
  Case scrut _ alts ->
    let altsTl = [if r `elem` patRaws p then Just 0 else tailCalls r a | Alt p a <- alts]
    in  case tailCalls r scrut of
          Just 0 | all isJust altsTl -> Just (sum (catMaybes altsTl))
          _ -> Nothing
  _ -> Just 0
 where
  under :: Int -> Term y -> Maybe Int
  under k e = if k == r then Just 0 else tailCalls r e

  teleRaws :: Tele IdBinder x' y' -> [Int]
  teleRaws TNil = []
  teleRaws (TCons b rest) = tmNameRaw (tmNameOf b) : teleRaws rest

  patRaws :: Pat x' y' -> [Int]
  patRaws = \case
    DataPat _ tvs ids -> tyRaws tvs ++ teleRaws ids
    _ -> []

  tyRaws :: Tele TyBinder x' y' -> [Int]
  tyRaws TNil = []
  tyRaws (TCons b rest) = tyNameRaw (tyNameOf b) : tyRaws rest

-- | The tail calls of a function in a let-expression, given the raw names of
-- its binders, the tail calls of the function in their right-hand sides, the
-- tail calls of a variable in the body, and those of the function in the
-- body. The function is used in a non-tail call position if it is in one of
-- the right-hand sides; if it isn't used in any of them, it is counted in
-- the body only; otherwise, if every binder whose right-hand side calls it is
-- only tail-called in the body, the calls in the right-hand sides are added
-- to those in the body.
letTailCalls :: [Int] -> [Maybe Int] -> (Int -> Maybe Int) -> Maybe Int -> Maybe Int
letTailCalls bndrs bsTls inBody fBody
  | not (all isJust bsTls) = Nothing
  | all (== 0) (catMaybes bsTls) = fBody
  | all (isJust . inBody) [b | (b, Just _) <- zip bndrs bsTls] =
      (sum (catMaybes bsTls) +) <$> fBody
  | otherwise = Nothing

-- | Determines whether a function has the following shape:
--
-- > \(w :: Void) -> f a b c
--
-- i.e. is a wrapper around a (partially) applied function 'f', where the
-- introduced argument 'w' is not used by 'f'.
isVoidWrapper :: Term n -> Bool
isVoidWrapper = \case
  Lam b e@(collectArgs -> (hd, _)) | isVar hd -> not (tmNameOf b `tmNameOccursIn` e)
  _ -> False
 where
  isVar = \case
    Var {} -> True
    GlobalVar {} -> True
    _ -> False

-- * Evaluation

-- | Bind the bindings the evaluator allocated over the result of a rewrite of
-- the evaluated term. The rewrite runs in the scope of the bindings. If it
-- changed the term, the work-free bindings are inlined and the unused ones
-- removed ('cleanupHeapBindings'). Otherwise the result of the rewrite should
-- not mention the bindings: it is only bound by them if it does, which
-- prevents unnecessary rewrites.
bindPureHeap
  :: Distinct n
  => TransformContext n
  -- ^ The context of the evaluated term
  -> Bind n l
  -- ^ The bindings the evaluator allocated
  -> Term l
  -- ^ The evaluated term
  -> (forall l'. DExt n l' => TransformContext l' -> Term l' -> RewriteMonad extra (Term l'))
  -- ^ The rewrite
  -> RewriteMonad extra (Term n)
bindPureHeap ctx bind v k = underLet ctx bind v $ \ctxL bind1 v1 -> do
  (r, Monoid.getAny -> hasChanged) <- Writer.listen (k ctxL v1)
  if hasChanged
    then cleanupHeapBindings ctx bind1 r
    else pure (fromMaybe (Let bind1 r) (unsinkTerm (bindNameBinders bind1) r))

-- | Bind the bindings the evaluator allocated over the result of a rewrite of
-- the evaluated term (see 'bindPureHeap'), inlining the work-free ones and
-- removing the unused ones:
--
--   1. Inline work-free bindings. We've seen cases in the wild† where the
--      evaluator (or rather, 'bindPureHeap') would let-bind work-free
--      binders that were crucial for eliminating case constructs. If these
--      case constructs were used in a self-referential (but terminating)
--      manner, Clash would get stuck in an infinite loop. The proper
--      solution would be to use 'Clash.Rewrite.Scoped.WorkFree.isWorkFree',
--      instead of 'Clash.Rewrite.Scoped.WorkFree.isWorkFreeIsh', in
--      'Clash.Normalize.Scoped.Transformations.Inline.bindConstantVar' such
--      that these work-free constructs would get inlined again. However,
--      this incurs a great performance penalty so we opt to prevent the
--      evaluator from introducing this situation in the first place.
--
--      This is not a proper solution though, as GHC might produce a similar
--      situation. We plan on properly solving this by eliminating the
--      current lift\/bind\/eval strategy, instead replacing it by a partial
--      evaluator‡.
--
--   2. Remove any unused let-bindings. Similar to (1), we risk Clash getting
--      stuck in an infinite loop if we don't remove unused (eliminated by
--      evaluation!) binders.
--
-- † <https://github.com/clash-lang/clash-compiler/pull/1354#issuecomment-635430374>
--
-- ‡ <https://www.microsoft.com/en-us/research/wp-content/uploads/2016/07/supercomp-by-eval.pdf>
cleanupHeapBindings
  :: Distinct n
  => TransformContext n
  -- ^ The context of the evaluated term
  -> Bind n l
  -- ^ The bindings the evaluator allocated
  -> Term l
  -- ^ The result of the rewrite, which may refer to the bindings
  -> RewriteMonad extra (Term n)
cleanupHeapBindings ctx bind e1 =
  -- See the haddock of this function for why
  inlineBinders inlineTest ctx (Let bind e1) bind e1 >>= \case
    e2@(Let bind1 e3) -> pure (fromMaybe e2 (removeUnusedBinders (tfScope ctx) bind1 e3))
    e2 -> pure e2
 where
  inlineTest _ lb = isWorkFreeTerm (stripTicks (lbRhs lb))
