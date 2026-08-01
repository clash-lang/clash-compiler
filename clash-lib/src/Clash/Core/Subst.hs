{-|
  Copyright   :  (C) 2012-2016, University of Twente,
                          2017, Google Inc.
                          2021, QBayLogic B.V.
                          2026, Martijn Bastiaan
  License     :  BSD2 (see the file LICENSE)
  Maintainer  :  QBayLogic B.V. <devops@qbaylogic.com>

  Capture-free substitution function for CoreHW
-}

{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

#include "../../ClashDebug.h"

module Clash.Core.Subst
  ( -- * Substitution into types
    -- ** Substitution environments
    TvSubst (..)
  , TvSubstEnv
  -- , mkTvSubst
  , extendTvSubst
  , extendTvSubstList
    -- ** Applying substitutions
  , substTy
  , substTyWith
  , substTyInVar
  , substGlobalsInExistentials
  , substInExistentials
  , substInExistentialsList
    -- * Substitution into terms
    -- ** Substitution environments
  , Subst (..)
  , mkSubst
  , mkTvSubst
  , extendInScopeId
  , extendInScopeIdList
  , extendIdSubst
  , extendIdSubstList
  , extendGblSubstList
    -- ** Applying substitutions
  , substTm
  , maybeSubstTm
  , unsafeSubstTm
  , substAlt
  , substId
    -- * Variable renaming
  , deShadowTerm
  , deShadowAlt
  , freshenTm
  , deshadowLetExpr
    -- * Alpha equivalence
  , aeqType
  , aeqTerm
  , acmpTerm
  , aeqTickInfo
  , acmpTickInfo
  , aeqTickInfoLevels
  , acmpTickInfoLevels
    -- * Structural equivalence
  , eqTerm
  , eqType
  )
where

import           Data.Coerce               (coerce)

#if MIN_VERSION_prettyprinter(1,7,0)
import           Prettyprinter
#else
import           Data.Text.Prettyprint.Doc
#endif

import           Data.Hashable             (Hashable (hashWithSalt))
import qualified Data.List                 as List
import qualified Data.List.Extra           as List
import           Data.Maybe                (fromMaybe)
import           Data.Ord                  (comparing)
import           GHC.Stack                 (HasCallStack)
import           GHC.SrcLoc.Extra          () -- Hashable RealSrcSpan
import           GHC.Types.SrcLoc
  (SrcSpan (RealSrcSpan, UnhelpfulSpan), leftmost_smallest)

import           Clash.Core.HasFreeVars
import           Clash.Core.Pretty         (ppr, fromPpr)
import           Clash.Core.Term
  (Alt, Bind(..), Pat (..), Term (..), TickInfo (..), PrimInfo(primName))
import           Clash.Core.Type           (Type (..))
import           Clash.Core.VarEnv
import           Clash.Core.Var
  (Id, Var (..), TyVar, isGlobalId, varKey)
import qualified Clash.Data.UniqMap as UniqMap
import           Clash.Debug               (debugIsOn)
import           Clash.Pretty
import           Clash.Util

-- * Subst

-- | A substitution of 'Type's for 'TyVar's
--
-- Note [Extending the TvSubstEnv]
-- See 'TvSubst' for the invariants that must hold
--
-- This invariant allows a short-cut when the subst env is empty: if the
-- TvSubstEnv is empty, i.e. @nullVarEnv TvSubstEnv@ holds, then
-- (substTy subst ty) does nothing.
--
-- For example, consider:
--
--    (/\a -> /\b(a ~ Int) -> ... b ...) Int
--
-- We substitute Int for 'a'. The Unique of 'b' does not change, but
-- nevertheless we add 'b' to the 'TvSubstEnv' because b's kind does change
--
-- This invariant has several consequences:
--
--   * In 'substTyVarBndr', we extend TvSubstEnv if the unique has changed, or
--     if the kind has changed
--
--   * In 'substTyVar', we do not need to consult the 'InScopeSet'; the
--     TvSubstEnv is enough
--
--   * In 'substTy', we can short-circuit when TvSubstEnv is empty
type TvSubstEnv = VarEnv Type

-- | Type substitution
--
-- The following invariants must hold:
--
--   1. The 'InScopeSet' is needed only to guide the generation of fresh uniques
--
--   2. In particular, the kind of the type variables in the 'InScopeSet' is not
--      relevant.
--
--   3. The substitution is only applied once
--
-- Note [Apply Once]
--
-- We might instantiate @forall a b. ty@ with the types @[a, b]@ or @[b, a]@.
-- So the substitution might go like @[a -> b, b -> a]@. A similar situation
-- arises in terms when we find a redex like @(/\a -> /\b -> e) b a@. Then we
-- also end up with a substitution that permutes variables. Other variations
-- happen to; for example @[a -> (a,b)]@.
--
-- SO A TvSubst MUST BE APPLIED PRECISELY ONCE, OR THINGS MIGHT LOOP
--
-- Note [The substitution invariant]
--
-- When calling (substTy subst ty) it should be the case that the 'InScopeSet'
-- is a superset of both:
--
--   * The free variables of the range of the substitution
--
--   * The free variables of /ty/ minus the domain of the substitution
data TvSubst
  = TvSubst InScopeSet -- Variable in scope /after/ substitution
            TvSubstEnv -- Substitution for types

instance ClashPretty TvSubst where
  clashPretty (TvSubst ins tenv) =
    brackets $ sep [ "TvSubst"
                   , nest 2 ("In scope:" <+> clashPretty ins)
                   , nest 2 ("Type env:" <+> clashPretty tenv)]

-- | A substitution  of 'Term's for 'Id's
--
-- Note [Extending the Subst]
--
-- For a term 'Subst', which binds 'Id's as well, we make a different choice for
-- Ids than we do for TyVars.
--
-- For TyVars see 'TvSubstEnv's Note [Extending the TvSubstEnv]
--
-- For Ids, we have a different invariant:
--
--   The IdSubstEnv is extended only when the Unique on an Id changes.
--   Otherwise, we just extend the InScopeSet
--
-- In consequence:
--
--   * If all subst envs are empty, substsTm would be a no-op
--
--     However, substTm still goes ahead and substitutes. Reason: we may want
--     to replace existing Ids with new ones from the in-scope set, to avoid
--     space leaks.
--
--   * In substIdBndr, we extend the 'IdSubstEnv' only when the unique changes
--
--   * If TvSubstEnv and IdSubstEnv are all empty, substExpr does nothing
--     (Note that the above rule for 'substIdBndr' maintains this property.)
--
--   * In 'lookupIdSubst', we must look up the Id in the in-scope set, because
--     it may contain non-trivial changes. Exmaple:
--
--     (/\a -> \x:a. ... x ...) Int
--
--     We extend the 'TvSubstEnv' with a @[a |-> Int]@; but x's unique does not
--     change so we only extend the in-scope set. Then we must look up in the
--     in-scope set when we find the occurrence of x.
--
--   * The requirement to look  up the Id in the in-scope set means that we
--     must not take no-op short cut when the 'IdSubstEnv' is empty. We must
--     still look up ever Id in the in-scope set.
--
--   * (However, we don't need to do so for the expression found in the
--     IdSubstEnv, whose range is assumed to be correct wrt the in-scope set)
type IdSubstEnv = VarEnv Term

-- | A substitution environment containing containing both 'Id' and 'TyVar'
-- substitutions.
--
-- Some invariants apply to how you use the substitution:
--
--   1. The 'InScopeSet' contains at least those 'Id's and 'TyVar's that will
--      be in scope /after/ applying the substitution  to a term. Precisely,
--      the in-scope set must be a superset of the free variables of the
--      substitution range that might possibly clash with locally-bound
--      variables in the thing being substituted in.
--
--   2. You may only apply the substitution once. See 'TvSubst'
--
-- There are various ways of setting up the in-scope set such that the first of
-- of these invariants holds:
--
--   * Arrange that the in-scope set really is all the things in scope
--
--   * Arrange that it's the  free vars of the range of the substitution
--
--   * Make it empty, if you know that all the free variables of the
--     substitution are fresh, and hence can´t possibly clash
data Subst
  = Subst
  { substInScope :: InScopeSet -- Variables in scope /after/ substitution
  , substTmEnv   :: IdSubstEnv -- Substitution for terms
  , substTyEnv   :: TvSubstEnv -- Substitution for types
  , substGblEnv  :: IdSubstEnv -- Substitution of globals (in terms)
  }

emptySubst
  :: Subst
emptySubst = Subst emptyInScopeSet emptyVarEnv emptyVarEnv emptyVarEnv

-- | An empty substitution, starting the variables currently in scope
mkSubst
  :: InScopeSet
  -> Subst
mkSubst is = Subst is emptyVarEnv emptyVarEnv emptyVarEnv

-- | Create a type substitution
mkTvSubst
  :: InScopeSet
  -> VarEnv Type
  -> Subst
mkTvSubst is env = Subst is emptyVarEnv env emptyVarEnv

-- | Generates the in-scope set for the 'Subst' from the types in the incoming
-- environment.
--
-- Should only be used the type we're substituting into has no free variables
-- outside of the domain of substitution
zipTvSubst
  :: [TyVar]
  -> [Type]
  -> Subst
zipTvSubst tvs tys
  | debugIsOn
  , not (List.equalLength tvs tys)
  = pprTrace "zipTvSubst" (ppr tvs <> line <> ppr tys) emptySubst
  | otherwise
  = Subst (mkInScopeSet (freeVarsOf tys)) emptyVarEnv tenv emptyVarEnv
 where
  tenv = zipTyEnv tvs tys

zipTyEnv
  :: [TyVar]
  -> [Type]
  -> VarEnv Type
zipTyEnv tvs tys = mkVarEnv (List.zipEqual tvs tys)

-- | Extend the substitution environment with a new 'Id' substitution
extendIdSubst
  :: Subst
  -> Id
  -> Term
  -> Subst
extendIdSubst (Subst is env tenv genv) i e =
  Subst is (extendVarEnv i e env) tenv genv

-- | Extend the substitution environment with a list of 'Id' substitutions
extendIdSubstList
  :: Subst
  -> [(Id,Term)]
  -> Subst
extendIdSubstList (Subst is env tenv genv) es =
  Subst is (extendVarEnvList env es) tenv genv

-- | Extend the substitution environment with a list of global 'Id' substitutions
extendGblSubstList
  :: Subst
  -> [(Id,Term)]
  -> Subst
extendGblSubstList (Subst is env tenv genv) es =
  Subst is env tenv (extendVarEnvList genv es)

-- | Extend the substitution environment with a new 'TyVar' substitution
extendTvSubst
  :: Subst
  -> TyVar
  -> Type
  -> Subst
extendTvSubst (Subst is env tenv genv) tv t =
  Subst is env (extendVarEnv tv t tenv) genv

-- | Extend the substitution environment with a list of 'TyVar' substitutions
extendTvSubstList
  :: Subst
  -> [(TyVar, Type)]
  -> Subst
extendTvSubstList (Subst is env tenv genv) ts =
  Subst is env (extendVarEnvList tenv ts) genv

-- | Add an 'Id' to the in-scope set: as a side effect, remove any existing
-- substitutions for it.
extendInScopeId
  :: Subst
  -> Id
  -> Subst
extendInScopeId (Subst inScope env tenv genv) id' =
  Subst inScope' env' tenv genv
 where
  inScope' = extendInScopeSet inScope id'
  env'     = delVarEnv env id'

-- | Add 'Id's to the in-scope set. See also 'extendInScopeId'
extendInScopeIdList
  :: Subst
  -> [Id]
  -> Subst
extendInScopeIdList (Subst inScope env tenv genv) ids =
  Subst inScope' env' tenv genv
 where
  inScope' = extendInScopeSetList inScope ids
  env'     = delVarEnvList env ids

-- | Substitute within a 'Type'
--
-- The substitution has to satisfy the invariant described in
-- 'TvSubst's Note [The substitution environment]
substTy
  :: HasCallStack
  => Subst
  -> Type
  -> Type
substTy (Subst inScope _ tvS _) ty
  | nullVarEnv tvS
  = ty
  | otherwise
  = checkValidSubst s' [ty] (substTy' s' ty)
 where
  s' = TvSubst inScope tvS

-- | Substitute within a 'TyVar'. See 'substTy'.
substTyInVar
  :: HasCallStack
  => Subst
  -> Var a
  -> Var a
substTyInVar subst tyVar =
  tyVar { varType = (substTy subst (varType tyVar)) }

-- | Like 'substTy', but skips the checks for the invariants described in
-- 'TvSubts' Note [The substitution environment]. Should be used inside this
-- module only.
substTyUnchecked
  :: HasCallStack
  => TvSubst
  -> Type
  -> Type
substTyUnchecked subst@(TvSubst _ tvS) ty
  | nullVarEnv tvS
  = ty
  | otherwise
  = substTy' subst ty

-- Safely substitute global type variables in a list of potentially
-- shadowing type variables.
substGlobalsInExistentials
  :: HasCallStack
  => InScopeSet
  -- ^ Variables in scope
  -> [TyVar]
  -- ^ List of existentials to apply the substitution for
  -> [(TyVar, Type)]
  -- ^ Substitutions
  -> [TyVar]
substGlobalsInExistentials is exts substs0 = result
  -- TODO: Is is actually possible that existentials shadow each other? If they
  -- TODO: can't, we can remove this function
  where
    iss     = scanl extendInScopeSet is exts
    substs1 = map (\is_ -> extendTvSubstList (mkSubst is_) substs0) iss
    result  = zipWith substTyInVar substs1 exts

-- | Safely substitute type variables in a list of existentials. This function
-- will account for cases where existentials shadow each other.
substInExistentialsList
  :: HasCallStack
  => InScopeSet
  -- ^ Variables in scope
  -> [TyVar]
  -- ^ List of existentials to apply the substitution for
  -> [(TyVar, Type)]
  -- ^ Substitutions
  -> [TyVar]
substInExistentialsList is exts substs =
  foldl (substInExistentials is) exts substs

-- | Safely substitute a type variable in a list of existentials. This function
-- will account for cases where existentials shadow each other.
substInExistentials
  :: HasCallStack
  => InScopeSet
  -- ^ Variables in scope
  -> [TyVar]
  -- ^ List of existentials to apply the substitution for
  -> (TyVar, Type)
  -- ^ Substitution
  -> [TyVar]
substInExistentials is exts subst@(typeVar, _type) =
  -- TODO: Is is actually possible that existentials shadow each other? If they
  -- TODO: can't, we can remove this function
  case List.elemIndices typeVar exts of
    [] ->
      -- We're not replacing any of the existentials, but a global variable
      substGlobalsInExistentials is exts [subst]
    (last -> i) ->
      -- We're replacing an existential. That means we're not touching any
      -- variables that were introduced before it. For all variables after it,
      -- it is as we would replace global variables in them.
      take (i+1) exts ++ substGlobalsInExistentials is (drop (i+1) exts) [subst]

-- | This checks if the substitution satisfies the invariant from 'TvSubst's
-- Note [The substitution invariant].
checkValidSubst
  :: HasCallStack
  => TvSubst
  -> [Type]
  -> a
  -> a
checkValidSubst subst@(TvSubst inScope tenv) tys a =
  WARN( not (isValidSubst subst),
        "inScope" <+> clashPretty inScope <> line <>
        "tenv" <+> clashPretty tenv <> line <>
        "tenvFVs" <+> clashPretty (freeVarsOf tenv) <> line <>
        "tys" <+> fromPpr tys)
  WARN( not tysFVsInSope,
       "inScope" <+> clashPretty inScope <> line <>
       "tenv" <+> clashPretty tenv <> line <>
       "tys" <+> fromPpr tys <> line <>
       "needsInScope" <+> clashPretty needsInScope)
  a
 where
  needsInScope = UniqMap.foldrWithUnique (\k _ s -> delVarSetByKey k s)
                   (freeVarsOf tys)
                   tenv
  tysFVsInSope = needsInScope `varSetInScope` inScope

-- | When calling 'substTy' it should be the case that the in-scope set in the
-- substitution is a superset of the free variables of the range of the
-- substitution.
--
-- See also 'TvSubst's Note [The substitution invariant].
isValidSubst
  :: TvSubst
  -> Bool
isValidSubst (TvSubst inScope tenv) = tenvFVs `varSetInScope` inScope
 where
  tenvFVs = freeVarsOf tenv

-- | The work-horse of 'substTy'
substTy'
  :: HasCallStack
  => TvSubst
  -> Type
  -> Type
substTy' subst = go where
  go = \case
    VarTy tv -> substTyVar subst tv
    ForAllTy tv ty -> case substTyVarBndr subst tv of
      (subst', tv') -> ForAllTy tv' (substTy' subst' ty)
    AppTy fun arg -> AppTy (go fun) (go arg)
    ty -> ty

-- | Substitute a variable with a type if it's within the substitution's domain.
--
-- Does not substitute within the kind of free variables.
substTyVar
  :: TvSubst
  -> TyVar
  -> Type
substTyVar (TvSubst _ tenv) tv = case lookupVarEnv tv tenv of
  Just ty -> ty
  _       -> VarTy tv

-- | Substitute a type variable in a binding position, returning an extended
-- substitution environment and a new type variable.
--
-- Substitutes within the kind of the type variable
substTyVarBndr
  :: TvSubst
  -> TyVar
  -> (TvSubst, TyVar)
substTyVarBndr subst@(TvSubst inScope tenv) oldVar =
  ASSERT2( no_capture, clashPretty oldVar <> line
                    <> clashPretty newVar <> line
                    <> clashPretty subst )
  (TvSubst (inScope `extendInScopeSet` newVar) newEnv, newVar)
 where
  newEnv | noChange  = delVarEnv tenv oldVar
         | otherwise = extendVarEnv oldVar (VarTy newVar) tenv

  -- Assertion that we're not capturing something in the substitution
  no_capture = not (newVar `elemVarSet` freeVarsOf tenv)

  oldKi        = varType oldVar
  -- verify that the kind is closed
  noKindChange = isClosed oldKi
  -- noChange means that the new type variable is identical in all respects to
  -- the old type variable (same unique, same kind)
  -- See 'TvSubstEnv's Note [Extending the TvSubstEnv]
  --
  -- In that case we don't need to extend the substitution to map old to new.
  -- But instead we must zap any current substitution for the variable. For
  -- example
  --
  --   (\x.e) with subst = [x | -> e']
  --
  -- Here we must simply zap the substitution for x
  noChange     = noKindChange && (newVar == oldVar)

  -- uniqAway ensures that the new variable is not already in scope
  newVar | noKindChange = uniqAway inScope oldVar
         | otherwise    = uniqAway inScope
                            (oldVar {varType = substTyUnchecked subst oldKi})

-- | Substitute within a 'Term'. Just return original term if given
-- substitution is "Nothing".
maybeSubstTm
  :: HasCallStack
  => Doc ()
  -> Maybe Subst
  -> Term
  -> Term
maybeSubstTm _doc Nothing = id
maybeSubstTm doc (Just s) = substTm doc s

-- | Substitute within a 'Term'
substTm
  :: HasCallStack
  => Doc ()
  -> Subst
  -> Term
  -> Term
substTm doc subst = go where
  go = \case
    Var v -> lookupIdSubst (doc <> line <> "subsTm") subst v
    Lam v e -> case substIdBndr subst v of
      (subst',v') -> Lam v' (substTm doc subst' e)
    TyLam v e -> case substTyVarBndr' subst v of
      (subst',v') -> TyLam v' (substTm doc subst' e)
    App l r -> App (go l) (go r)
    TyApp l r -> TyApp (go l) (substTy subst r)
    Let bs e -> case substBind doc subst bs of
      (subst',bs') -> Let bs' (substTm doc subst' e)
    Case subj ty alts -> Case (go subj) (substTy subst ty) (map goAlt alts)
    Cast e t1 t2 -> Cast (go e) (substTy subst t1) (substTy subst t2)
    Tick tick e -> Tick (goTick tick) (go e)
    tm -> tm

  goAlt (pat,alt) = case pat of
    DataPat dc tvs ids -> case List.mapAccumL substTyVarBndr' subst tvs of
      (subst1,tvs') -> case List.mapAccumL substIdBndr subst1 ids of
        (subst2,ids') -> (DataPat dc tvs' ids',substTm doc subst2 alt)
    _ -> (pat,go alt)

  goTick t@(SrcSpan _)  = t
  goTick (NameMod m ty) = NameMod m (substTy subst ty)
  goTick t@DeDup        = t
  goTick t@NoDeDup      = t
  goTick (Attributes ty tm) = Attributes (substTy subst ty) (go tm)

-- | Like 'substTm', but doesn't account for shadowing or free variable capture.
--
-- An example of shadowing: in @(x, \x -> x)@ with a substitution @x |-> 5@
-- 'unsafeSubstTm' will yield @(5, \x -> 5)@, whereas a safe substitution would
-- yield @(5, \x -> x)@.
--
-- An example of free variable capture: in @\x -> y@ and a substitution
-- @y |-> f x@, 'unsafeSubstTm' will yield @\x -> f x@, whereas a safe substitution
-- would yield @\x' -> f x@.
--
-- You should therefore only use this function if:
--
--   1. No binder in the term binds variables in the domain of the local
--      substitution.
--
--   2. No binder in the term has the same unique as a local, free variable of any
--      replacement term in the range of either substitution.
--
-- Both conditions hold if the term is deshadowed (see 'deShadowTerm') with
-- respect to an in-scope set that contains the domains of the substitutions
-- and the free variables of the replacement terms.
--
-- Note that global substituations are always safe. They never have local free
-- variables and cannot be introduced through a binder. I.e., neither rule 1
-- nor 2 applies.
unsafeSubstTm
  :: VarEnv Term
  -- ^ Substitution: global variable to replacement term
  -> VarEnv Term
  -- ^ Substitution: local variable to replacement term
  -> Term
  -- ^ Term to substitute in
  -> Term
unsafeSubstTm globals locals = \term -> fromMaybe term (go term)
 where
  go :: Term -> Maybe Term
  go = \case
    Var v
      | isGlobalId v -> lookupVarEnv v globals
      | otherwise -> lookupVarEnv v locals
    Lam v e -> Lam v <$> go e
    TyLam tv e -> TyLam tv <$> go e
    App l r -> case (go l, go r) of
      (Nothing, Nothing) -> Nothing
      (l1, r1) -> Just (App (fromMaybe l l1) (fromMaybe r r1))
    TyApp e ty -> (`TyApp` ty) <$> go e
    Let bs body -> case (goBind bs, go body) of
      (Nothing, Nothing) -> Nothing
      (bs1, body1) -> Just (Let (fromMaybe bs bs1) (fromMaybe body body1))
    Case subject ty alternatives ->
      case (go subject, goList goAlternative alternatives) of
        (Nothing, Nothing) -> Nothing
        (subject1, alternatives1) ->
          Just (Case (fromMaybe subject subject1) ty
                     (fromMaybe alternatives alternatives1))
    Cast e t1 t2 -> (\e1 -> Cast e1 t1 t2) <$> go e
    Tick tickInfo e -> case (goTickInfo tickInfo, go e) of
      (Nothing, Nothing) -> Nothing
      (tick1, e1) -> Just (Tick (fromMaybe tickInfo tick1) (fromMaybe e e1))
    Data{} -> Nothing
    Literal{} -> Nothing
    Prim{} -> Nothing

  goBind (NonRec v rhs) = NonRec v <$> go rhs
  goBind (Rec bindings0) = Rec <$> goList goBinding bindings0

  goBinding (v, rhs) = (,) v <$> go rhs

  goAlternative (pat, alternative) = (,) pat <$> go alternative

  -- Types contain no term variables, so of the tick constructors only
  -- 'Attributes', which carries a term, needs a traversal.
  goTickInfo (Attributes ty e) = Attributes ty <$> go e
  goTickInfo SrcSpan{} = Nothing
  goTickInfo NameMod{} = Nothing
  goTickInfo DeDup = Nothing
  goTickInfo NoDeDup = Nothing

  goList :: (a -> Maybe a) -> [a] -> Maybe [a]
  goList f = goElements
   where
    goElements [] = Nothing
    goElements (x:xs) = case (f x, goElements xs) of
      (Nothing, Nothing) -> Nothing
      (x1, xs1) -> Just (fromMaybe x x1 : fromMaybe xs xs1)

-- | Substitute within a case-alternative
substAlt
  :: HasCallStack
  => Doc ()
  -> Subst
  -- ^ The substitution
  -> (Pat, Term)
  -- ^ The alternative in which to apply the substitution
  -> (Pat, Term)
substAlt doc subst (pat,alt) = case pat of
  DataPat dc tvs ids -> case List.mapAccumL substTyVarBndr' subst tvs of
    (subst1,tvs1) -> case List.mapAccumL substIdBndr subst1 ids of
      (subst2,ids1) -> (DataPat dc tvs1 ids1,substTm doc subst2 alt)
  _ -> (pat, substTm doc subst alt)

substId
  :: HasCallStack
  => Subst
  -> Id
  -> Id
substId subst oldId = snd $ substIdBndr subst oldId

-- | Find the substitution for an 'Id' in the 'Subst'
lookupIdSubst
  :: HasCallStack
  => Doc ()
  -> Subst
  -> Id
  -> Term
lookupIdSubst doc (Subst inScope tmS _ genv) v
  | isGlobalId v = case lookupVarEnv v genv of
                     Just e -> e
                     _      -> Var v
  | Just e <- lookupVarEnv v tmS = e
  -- Vital! See 'IdSubstEnv' Note [Extending the Subst]
  --
  -- TODO: We match on Id here to workaround an issue where type variables
  -- TODO: "shadow" term variables. Omitting the check would make 'lookupIdSubst'
  -- TODO: potentially replace an "Id" with a TyVar. For more information:
  -- TODO:
  -- TODO:   https://github.com/clash-lang/clash-compiler/issues/1046
  -- TODO:
  | Just v'@(Id {}) <- lookupInScope inScope v = Var (coerce v')
  | otherwise = WARN(True, "Subst.lookupIdSubst" <+> doc <+> fromPpr v)
                Var v

-- | Substitute an 'Id' for another one according to the 'Subst' given,
-- returning the result and an update 'Subst' that should be used in subsequent
-- substitutions.
substIdBndr
  :: HasCallStack
  => Subst
  -> Id
  -> (Subst,Id)
substIdBndr subst@(Subst inScope env tenv genv) oldId =
  (Subst (inScope `extendInScopeSet` newId) newEnv tenv genv, newId)
 where
  id1 = uniqAway inScope oldId
  newId | noTypeChange = id1
        | otherwise    = id1 {varType = substTy subst (varType id1)}

  oldTy = varType oldId
  noTypeChange = nullVarEnv tenv || isClosed oldTy

  -- Extend the substitution if the unique has changed.
  --
  -- In case it hasn't changed we don't need to extend the substitution to map
  -- old to new. But instead we must zap any current substitution for the
  -- variable. For example
  --
  --   (\x.e) with subst = [x | -> e']
  --
  -- Here we must simply zap the substitution for x
  newEnv | noChange  = delVarEnv env oldId
         | otherwise = extendVarEnv oldId (Var newId) env

  -- See Note [Extending the Subst] why it's not necessary to check noTypeChange
  noChange = id1 == oldId

-- | Like 'substTyVarBndr' but takes a 'Subst' instead of a 'TvSubst'
substTyVarBndr'
  :: HasCallStack
  => Subst
  -> TyVar
  -> (Subst,TyVar)
substTyVarBndr' (Subst inScope tmS tyS tgS) tv =
  case substTyVarBndr (TvSubst inScope tyS) tv of
    (TvSubst inScope' tyS',tv') -> (Subst inScope' tmS tyS' tgS, tv')

-- | Apply a substitution to an entire set of let-bindings, additionally
-- returning an updated 'Subst' that should be used by subsequent substitutions.
substBind
  :: HasCallStack
  => Doc ()
  -> Subst
  -> Bind Term
  -> (Subst, Bind Term)
substBind doc subst (NonRec i x) =
  (subst', NonRec i' x')
 where
  (subst', i') = substIdBndr subst i
  x' = substTm ("substBind" <+> doc) subst x

substBind doc subst (Rec xs) =
  (subst', Rec (zip bndrs' rhss'))
 where
  (bndrs,rhss)    = unzip xs
  (subst',bndrs') = List.mapAccumL substIdBndr subst bndrs
  rhss'           = map (substTm ("substBind" <+> doc) subst') rhss

-- | Type substitution, see 'zipTvSubst'
--
-- Works only if the domain of the substitution is superset of the type being
-- substituted into
substTyWith
  :: HasCallStack
  => [TyVar]
  -> [Type]
  -> Type
  -> Type
substTyWith tvs tys =
  ASSERT( List.equalLength tvs tys )
  substTy (zipTvSubst tvs tys)

-- | Ensure that non of the binders in an expression shadow each-other, nor
-- conflict with he in-scope set
deShadowTerm
  :: HasCallStack
  => InScopeSet
  -> Term
  -> Term
deShadowTerm is e = substTm "deShadowTerm" (mkSubst is) e

-- | Ensure that non of the binders in an alternative shadow each-other, nor
-- conflict with the in-scope set
deShadowAlt ::
  HasCallStack =>
  InScopeSet ->
  (Pat, Term) ->
  (Pat, Term)
deShadowAlt is = substAlt "deShadowAlt" (mkSubst is)

-- | Ensure that non of the let-bindings of a let-expression shadow w.r.t the
-- in-scope set
deshadowLetExpr
  :: HasCallStack
  => InScopeSet
  -- ^ Current InScopeSet
  -> Bind Term
  -- ^ Bindings of the let-expression
  -> Term
  -- ^ The body of the let-expression
  -> (Bind Term, Term)
  -- ^ Deshadowed let-bindings, where let-bound expressions and the let-body
  -- properly reference the renamed variables
deshadowLetExpr is bs e =
  case substBind "deshadowLetBindings" (mkSubst is) bs of
    (s1,bs1) -> (bs1, substTm "deShadowLetBody" s1 e)

-- | A much stronger variant of `deShadowTerm` that ensures that all bound
-- variables are unique.
--
-- Also returns an extended 'InScopeSet' additionally containing the (renamed)
-- unique bound variables of the term.
freshenTm
  :: InScopeSet
  -- ^ Current set of variables in scope
  -> Term
  -> (InScopeSet, Term)
freshenTm is0 = go (mkSubst is0) where
  go subst0 = \case
    Var v -> (substInScope subst0, lookupIdSubst "freshenTm" subst0 v)
    Lam v e -> case substIdBndr subst0 v of
      (subst1,v') -> case go subst1 e of
        (is2,e') -> (is2, Lam v' e')
    TyLam v e -> case substTyVarBndr' subst0 v of
      (subst1,v') -> case go subst1 e of
        (is2,e') -> (is2,TyLam v' e')
    App l r -> case go subst0 l of
      (is1,l') -> case go subst0 {substInScope = is1} r of
        (is2,r') -> (is2, App l' r')
    TyApp l r -> case go subst0 l of
      (is1,l') -> (is1, TyApp l' (substTy subst0 r))
    Let bs e -> case goBind subst0 bs of
      (subst1,bs') -> case go subst1 e of
        (is2,e') -> (is2,Let bs' e')
    Case subj ty alts -> case go subst0 subj of
      (is1,subj') -> case List.mapAccumL (\isN -> goAlt subst0 {substInScope = isN}) is1 alts of
        (is2,alts') -> (is2, Case subj' (substTy subst0 ty) alts')
    Cast e t1 t2 -> case go subst0 e of
      (is1, e') -> (is1, Cast e' (substTy subst0 t1) (substTy subst0 t2))
    Tick tick e -> case goTick subst0 tick of
      (is1, tick') -> case go subst0 {substInScope = is1} e of
        (is2, e') -> (is2, Tick tick' e')
    tm -> (substInScope subst0, tm)

  goBind subst0 (NonRec i x) =
    let (subst1, i') = substIdBndr subst0 i
        (is2, x') = go subst0 x
     in (subst1 { substInScope = extendInScopeSet is2 i' }, NonRec i' x')

  goBind subst0 (Rec xs) =
    let (bndrs,rhss)    = unzip xs
        (subst1,bndrs') = List.mapAccumL substIdBndr subst0 bndrs
        (is2,rhss')     = List.mapAccumL (\isN -> go subst1 {substInScope = isN})
                                         (substInScope subst1)
                                         rhss
    in  (subst1 {substInScope = is2}, Rec $ zip bndrs' rhss')

  goAlt subst0 (pat,alt) = case pat of
    DataPat dc tvs ids -> case List.mapAccumL substTyVarBndr' subst0 tvs of
      (subst1,tvs') -> case List.mapAccumL substIdBndr subst1 ids of
        (subst2,ids') -> case go subst2 alt of
          (is3,alt') -> (is3,(DataPat dc tvs' ids',alt'))
    _ -> case go subst0 alt of
      (is1,alt') -> (is1,(pat,alt'))

  goTick subst0 t@(SrcSpan _) = (substInScope subst0, t)
  goTick subst0 (NameMod m ty) =
    (substInScope subst0, NameMod m (substTy subst0 ty))
  goTick subst0 t@DeDup = (substInScope subst0, t)
  goTick subst0 t@NoDeDup = (substInScope subst0, t)
  goTick subst0 (Attributes ty tm) = case go subst0 tm of
    (is1, tm') -> (is1, Attributes (substTy subst0 ty) tm')

-- * AEQ

-- | Alpha equality for types
aeqType
  :: Type
  -> Type
  -> Bool
aeqType t1 t2 = acmpType t1 t2 == EQ
{-# INLINE aeqType #-}

-- | Alpha comparison for types
acmpType
  :: Type
  -> Type
  -> Ordering
acmpType = acmpTypeLevels 0 emptyVarEnv emptyVarEnv

-- | Structural equality on 'Type'
eqType
  :: Type
  -> Type
  -> Bool
eqType = go
 where
  go (VarTy tv1) (VarTy tv2) = tv1 == tv2
  go (ConstTy c1) (ConstTy c2) = c1 == c2
  go (ForAllTy tv1 t1) (ForAllTy tv2 t2) =
    tv1 == tv2 && go (varType tv1) (varType tv2) && go t1 t2
  go (AppTy s1 t1) (AppTy s2 t2) = go s1 s2 && go t1 t2
  go (LitTy l1) (LitTy l2) = l1 == l2
  go (AnnType a1 t1) (AnnType a2 t2) = a1 == a2 && go t1 t2
  go _ _ = False

-- | Alpha equality for terms
aeqTerm
  :: Term
  -> Term
  -> Bool
aeqTerm t1 t2 = acmpTerm t1 t2 == EQ
{-# INLINE aeqTerm #-}

-- | Alpha comparison for terms
acmpTerm
  :: Term
  -> Term
  -> Ordering
acmpTerm =
  acmpTermLevels 0 emptyVarEnv emptyVarEnv emptyVarEnv emptyVarEnv

-- | Alpha equality for ticks
aeqTickInfo
  :: TickInfo
  -> TickInfo
  -> Bool
aeqTickInfo t1 t2 = acmpTickInfo t1 t2 == EQ
{-# INLINE aeqTickInfo #-}

-- | Alpha comparison for ticks
acmpTickInfo
  :: TickInfo
  -> TickInfo
  -> Ordering
acmpTickInfo =
  acmpTickInfoLevels 0 emptyVarEnv emptyVarEnv emptyVarEnv emptyVarEnv

-- | Structural equality on 'Term'
eqTerm :: Term -> Term -> Bool
eqTerm = go
 where
  go (Var id1) (Var id2) = id1 == id2
  go (Data dc1) (Data dc2) = dc1 == dc2
  go (Literal l1) (Literal l2) = l1 == l2
  go (Prim p1) (Prim p2) = primName p1 == primName p2
  go (Lam b1 e1) (Lam b2 e2) =
    b1 == b2 && eqType (varType b1) (varType b2) && go e1 e2
  go (TyLam b1 e1) (TyLam b2 e2) =
    b1 == b2 && eqType (varType b1) (varType b2) && go e1 e2
  go (App l1 r1) (App l2 r2) = go l1 l2 && go r1 r2
  go (TyApp l1 r1) (TyApp l2 r2) = go l1 l2 && eqType r1 r2
  go (Let bs1 e1) (Let bs2 e2) =
    go e1 e2 &&
    goBind bs1 bs2
   where
    goBind (NonRec b1 r1) (NonRec b2 r2) =
      -- No need to check types of NonRec bindings, when the RHSs match the
      -- types must be the same.
      b1 == b2 && go r1 r2
    goBind (Rec brs1) (Rec brs2) =
      List.all2
        (\(b1,r1) (b2,r2) ->
          b1 == b2 &&
          -- We need to check the types of Rec bindings, because:
          --
          -- letrec (x : Bool) = x in X
          --
          -- is not structurally equivalent to
          --
          -- letrec (x : Int) = x in x
          eqType (varType b1) (varType b2) &&
          go r1 r2)
        brs1 brs2
    goBind _ _ = False
  -- Note [Case result types and alpha-equivalence]
  go (Case e1 _ a1) (Case e2 _ a2) =
    go e1 e2 &&
    List.all2 goAlt a1 a2
   where
    goAlt (p1,r1) (p2,r2) = p1 == p2 && go r1 r2
  go (Cast e1 l1 r1) (Cast e2 l2 r2) =
    go e1 e2 &&
    eqType l1 l2 &&
    eqType r1 r2
  go (Tick t1 e1) (Tick t2 e2) = goTick t1 t2 && go e1 e2
  go _ _ = False

  -- @Eq TickInfo@ compares 'NameMod' and 'Attributes' with alpha-equivalence,
  -- which is too coarse here: this is structural equality.
  goTick (SrcSpan s1) (SrcSpan s2) = s1 == s2
  goTick (NameMod m1 t1) (NameMod m2 t2) = m1 == m2 && eqType t1 t2
  goTick DeDup DeDup = True
  goTick NoDeDup NoDeDup = True
  goTick (Attributes t1 a1) (Attributes t2 a2) = eqType t1 t2 && go a1 a2
  goTick _ _ = False

{- Note [Case result types and alpha-equivalence]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
'acmpTermLevels' (and hence 'aeqTerm', 'acmpTerm', @Eq Term@ and @Ord Term@)
does not compare the result type stored in a 'Case' constructor, and 'eqTerm'
follows it. This is OK for case expressions with at least one alternative: the
result type is determined by the alternatives. Every alternative's right-hand
side has exactly the result type, and (alpha-)equal right-hand sides have
(alpha-)equal types, so comparing the result type as well would be redundant
work.

For a case expression with no alternatives the result type is /not/ determined
by the subterms, so @(case x of {}) :: A@ and @(case x of {}) :: B@ compare
equal even though their types differ. Such case expressions don't occur in
CoreHW: @Clash.GHC.GHC2Core@ turns them into an @undefined@ or @undefinedX@.
-}

instance Eq Type where
  (==) = aeqType

instance Ord Type where
  compare = acmpType

instance Eq Term where
  (==) = aeqTerm

{- Note [Numbering binders by De Bruijn level]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Alpha comparison compares two variable occurrences by their De Bruijn level:
how many binders enclose the binder an occurrence resolves to. Both sides are
walked in lockstep, so at any point they are under equally many binders, and a
level therefore identifies a binder position rather than a name. Two
occurrences are alpha-equal exactly when they resolve to the same level.

Binders that come into scope together, such as those of a 'Rec' or a 'DataPat',
get consecutive levels: they are numbered as if they were nested.
-}

-- | Compare a pair of variable occurrences, under the levels of the binders
-- enclosing them. See Note [Numbering binders by De Bruijn level].
acmpOccLevels ::
  -- | Levels of the binders enclosing the left occurrence
  VarEnv Int ->
  -- | Levels of the binders enclosing the right occurrence
  VarEnv Int ->
  Var a ->
  Var a ->
  Ordering
acmpOccLevels envL envR v1 v2 =
  case (lookupVarEnv v1 envL, lookupVarEnv v2 envR) of
    -- Both bound: equal exactly when bound at the same level
    (Just lvlL, Just lvlR) -> compare lvlL lvlR
    -- Neither bound: compare the variables themselves
    (Nothing, Nothing) -> compare (varKey v1) (varKey v2)
    -- A bound variable is never equal to a free one. Which way round is
    -- arbitrary, it only has to be consistent to keep the order total.
    (Just _, Nothing) -> LT
    (Nothing, Just _) -> GT

-- | Give a group of binders that come into scope together, such as a 'Rec' or
-- a 'DataPat', consecutive levels, and return the level following the group.
extendLevels ::
  Int ->
  [Var a] ->
  [Var a] ->
  VarEnv Int ->
  VarEnv Int ->
  (Int, VarEnv Int, VarEnv Int)
extendLevels lvl vs1 vs2 envL envR =
  List.foldl' one (lvl, envL, envR) (List.zipEqual vs1 vs2)
 where
  one (!l, eL, eR) (v1, v2) =
    (l + 1, extendVarEnv v1 l eL, extendVarEnv v2 l eR)

-- | Alpha comparison for 'Type's, under the binders enclosing them.
-- See Note [Numbering binders by De Bruijn level].
acmpTypeLevels ::
  -- | Number of enclosing binders
  Int ->
  -- | Levels of the type binders enclosing the left type
  VarEnv Int ->
  -- | Levels of the type binders enclosing the right type
  VarEnv Int ->
  Type ->
  Type ->
  Ordering
acmpTypeLevels !lvl tyL tyR = go
 where
  go :: Type -> Type -> Ordering
  go (VarTy tv1) (VarTy tv2) = acmpOccLevels tyL tyR tv1 tv2
  go (ConstTy c1) (ConstTy c2) = compare c1 c2
  go (ForAllTy tv1 t1) (ForAllTy tv2 t2) =
    go (varType tv1) (varType tv2) `thenCompare`
      acmpTypeLevels
        (lvl + 1) (extendVarEnv tv1 lvl tyL) (extendVarEnv tv2 lvl tyR) t1 t2
  go (AppTy s1 t1) (AppTy s2 t2) = go s1 s2 `thenCompare` go t1 t2
  go (LitTy l1) (LitTy l2) = compare l1 l2
  go (AnnType a1 t1) (AnnType a2 t2) = compare a1 a2 `thenCompare` go t1 t2
  go t1 t2 = compare (getRank t1) (getRank t2)

  getRank :: Type -> Word
  getRank (VarTy {})    = 0
  getRank (LitTy {})    = 1
  getRank (ConstTy {})  = 2
  getRank (AnnType {})  = 3
  getRank (AppTy {})    = 4
  getRank (ForAllTy {}) = 5

-- | Alpha comparison for 'Term's. See 'acmpTypeLevels'.
acmpTermLevels ::
  -- | Number of enclosing binders
  Int ->
  -- | Levels of the term binders enclosing the left term
  VarEnv Int ->
  -- | Levels of the term binders enclosing the right term
  VarEnv Int ->
  -- | Levels of the type binders enclosing the left term
  VarEnv Int ->
  -- | Levels of the type binders enclosing the right term
  VarEnv Int ->
  Term ->
  Term ->
  Ordering
acmpTermLevels !lvl tmL tmR tyL tyR = go
 where
  goType = acmpTypeLevels lvl tyL tyR
  goTick = acmpTickInfoLevels lvl tmL tmR tyL tyR

  -- Compare underneath one more term or type binder
  underTmBndr b1 b2 =
    acmpTermLevels
      (lvl + 1) (extendVarEnv b1 lvl tmL) (extendVarEnv b2 lvl tmR) tyL tyR
  underTyBndr b1 b2 =
    acmpTermLevels
      (lvl + 1) tmL tmR (extendVarEnv b1 lvl tyL) (extendVarEnv b2 lvl tyR)

  go :: Term -> Term -> Ordering
  go (Var id1) (Var id2) = goVar id1 id2
  go (Data dc1) (Data dc2) = compare dc1 dc2
  go (Literal l1) (Literal l2) = compare l1 l2
  go (Prim p1) (Prim p2) = comparing primName p1 p2
  go (Lam b1 e1) (Lam b2 e2) =
    goType (varType b1) (varType b2) `thenCompare` underTmBndr b1 b2 e1 e2
  go (TyLam b1 e1) (TyLam b2 e2) =
    goType (varType b1) (varType b2) `thenCompare` underTyBndr b1 b2 e1 e2
  go (App l1 r1) (App l2 r2) = go l1 l2 `thenCompare` go r1 r2
  go (TyApp l1 r1) (TyApp l2 r2) = go l1 l2 `thenCompare` goType r1 r2
  go (Let (NonRec i1 x1) e1) (Let (NonRec i2 x2) e2) =
    go x1 x2 `thenCompare` underTmBndr i1 i2 e1 e2
  go (Let (Rec bs1) e1) (Let (Rec bs2) e2) =
    -- The lengths are compared first: the binder lists are only numbered
    -- against each other when they match
    compare (length bs1) (length bs2) `thenCompare`
      let (ids1, rhs1) = unzip bs1
          (ids2, rhs2) = unzip bs2
          (lvl', tmL', tmR') = extendLevels lvl ids1 ids2 tmL tmR
          under = acmpTermLevels lvl' tmL' tmR' tyL tyR
      -- Note that we compare types, because:
      --
      --   let (x :: Int) = x in x
      --
      -- is not alpha equivalent to:
      --
      --   let (x :: Word) = x in x
      --
      in goList goType (map varType ids1) (map varType ids2) `thenCompare`
           (goList under rhs1 rhs2 `thenCompare` under e1 e2)
  -- Note [Case result types and alpha-equivalence]
  go (Case e1 _ a1) (Case e2 _ a2) =
    compare (length a1) (length a2) `thenCompare`
      (go e1 e2 `thenCompare` goAlts a1 a2)
  go (Cast e1 l1 r1) (Cast e2 l2 r2) =
    go e1 e2 `thenCompare` (goType l1 l2 `thenCompare` goType r1 r2)
  go (Tick t1 e1) (Tick t2 e2) = goTick t1 t2 `thenCompare` go e1 e2
  go e1 e2 = compare (getRank e1) (getRank e2)

  goList :: (a -> a -> Ordering) -> [a] -> [a] -> Ordering
  goList cmp (x : xs) (y : ys) = cmp x y `thenCompare` goList cmp xs ys
  goList _ _ _ = EQ

  goAlts :: [Alt] -> [Alt] -> Ordering
  goAlts (x : xs) (y : ys) = goAlt x y `thenCompare` goAlts xs ys
  goAlts _ _ = EQ

  goAlt :: Alt -> Alt -> Ordering
  goAlt (DataPat c1 tvs1 ids1, e1) (DataPat c2 tvs2 ids2, e2) =
    -- Two 'DataPat's for the same 'DataCon' necessarily bind equally many
    -- variables, so 'extendLevels' erroring on lists of unequal length is the
    -- right response to Core that is already ill-formed
    compare c1 c2 `thenCompare`
      let (lvlTy, tyL', tyR') = extendLevels lvl tvs1 tvs2 tyL tyR
          (lvl', tmL', tmR') = extendLevels lvlTy ids1 ids2 tmL tmR
      in acmpTermLevels lvl' tmL' tmR' tyL' tyR' e1 e2
  goAlt (c1, e1) (c2, e2) = compare c1 c2 `thenCompare` go e1 e2

  goVar :: Id -> Id -> Ordering
  goVar id1 id2
    -- A global is never bound by an enclosing binder, so it never resolves to
    -- a level. Checked before the environments, because those are keyed on
    -- unique alone and a global may share a unique with a bound local.
    | isGlobalId id1 || isGlobalId id2 = compare (varKey id1) (varKey id2)
    | otherwise = acmpOccLevels tmL tmR id1 id2

  getRank :: Term -> Word
  getRank = \case
    Var {}     -> 0
    Data {}    -> 1
    Literal {} -> 2
    Prim {}    -> 3
    Cast {}    -> 4
    App {}     -> 5
    TyApp {}   -> 6
    Lam {}     -> 7
    TyLam {}   -> 8
    Let NonRec{} _ -> 9
    Let Rec{} _ -> 10
    Case {}    -> 11
    Tick {}    -> 12

-- | Alpha equality for ticks, under the binders enclosing them.
-- See 'acmpTickInfoLevels'.
aeqTickInfoLevels ::
  -- | Number of enclosing binders
  Int ->
  -- | Levels of the term binders enclosing the left tick
  VarEnv Int ->
  -- | Levels of the term binders enclosing the right tick
  VarEnv Int ->
  -- | Levels of the type binders enclosing the left tick
  VarEnv Int ->
  -- | Levels of the type binders enclosing the right tick
  VarEnv Int ->
  TickInfo ->
  TickInfo ->
  Bool
aeqTickInfoLevels lvl tmL tmR tyL tyR t1 t2 =
  acmpTickInfoLevels lvl tmL tmR tyL tyR t1 t2 == EQ
{-# INLINE aeqTickInfoLevels #-}

-- | Alpha comparison for ticks, under the binders enclosing them.
--
-- The 'Type' in 'NameMod' and the 'Term' in 'Attributes' live in the scope
-- enclosing the tick, so they are compared under the same binders as the term
-- the tick is attached to. 'SrcSpan's are compared with 'leftmost_smallest',
-- which treats all unhelpful spans alike, so it is coarser than @Eq TickInfo@.
acmpTickInfoLevels ::
  -- | Number of enclosing binders
  Int ->
  -- | Levels of the term binders enclosing the left tick
  VarEnv Int ->
  -- | Levels of the term binders enclosing the right tick
  VarEnv Int ->
  -- | Levels of the type binders enclosing the left tick
  VarEnv Int ->
  -- | Levels of the type binders enclosing the right tick
  VarEnv Int ->
  TickInfo ->
  TickInfo ->
  Ordering
acmpTickInfoLevels !lvl tmL tmR tyL tyR = go
 where
  goType = acmpTypeLevels lvl tyL tyR
  goTerm = acmpTermLevels lvl tmL tmR tyL tyR

  go :: TickInfo -> TickInfo -> Ordering
  go (SrcSpan s1) (SrcSpan s2) = leftmost_smallest s1 s2
  go (NameMod m1 t1) (NameMod m2 t2) = compare m1 m2 `thenCompare` goType t1 t2
  go (Attributes t1 a1) (Attributes t2 a2) =
    goType t1 t2 `thenCompare` goTerm a1 a2
  go t1 t2 = compare (getRank t1) (getRank t2)

  getRank :: TickInfo -> Word
  getRank = \case
    SrcSpan {}    -> 0
    NameMod {}    -> 1
    DeDup         -> 2
    NoDeDup       -> 3
    Attributes {} -> 4

instance Ord Term where
  compare = acmpTerm

-- * Alpha hashing
--
-- Hashing is modulo alpha equivalence, so that it agrees with @Eq Term@ and
-- @Eq Type@: alpha-equivalent terms hash alike. Only that direction holds,
-- terms that are not alpha-equivalent may collide as with any hash. A bound
-- variable is hashed by its De Bruijn level, see
-- Note [Numbering binders by De Bruijn level].

-- | Mix the tag of a constructor into a salt, so that terms differing only in
-- which constructor they use do not hash alike.
hashTag :: Int -> Int -> Int
hashTag = hashWithSalt

-- | Give a group of binders that come into scope together, such as a 'Rec' or a
-- 'DataPat', consecutive levels, and return the level following the group.
extendLevelsOf :: Int -> [Var a] -> VarEnv Int -> (Int, VarEnv Int)
extendLevelsOf lvl vs env = List.foldl' one (lvl, env) vs
 where
  one (!l, e) v = (l + 1, extendVarEnv v l e)

-- | Hash a 'SrcSpan' only as finely as @Eq Term@ tells one apart: all
-- 'UnhelpfulSpan's alike, and a 'RealSrcSpan' by its file name and its start
-- and end positions. The structural @Hashable SrcSpan@ orphan from
-- "GHC.SrcLoc.Extra" is finer than that, as it takes in the buffer span and the
-- reason of an unhelpful span as well, and would tell alpha-equivalent terms
-- apart.
hashSrcSpan :: Int -> SrcSpan -> Int
hashSrcSpan salt = \case
  RealSrcSpan realSrcSpan _bufSpan -> hashWithSalt salt (0 :: Int, realSrcSpan)
  UnhelpfulSpan _reason -> hashWithSalt salt (1 :: Int)

-- | Hash a 'Type' modulo alpha, under the binders enclosing it.
-- See Note [Numbering binders by De Bruijn level].
aTypeHashLevels ::
  -- | Number of enclosing binders
  Int ->
  -- | Levels of the type binders enclosing the type
  VarEnv Int ->
  -- | Salt
  Int ->
  Type ->
  Int
aTypeHashLevels !lvl tyEnv = go
 where
  go :: Int -> Type -> Int
  go salt = \case
    VarTy tv ->
      -- N.B.: Variables are hashed with a "tag" to differentiate between, e.g.,
      --       a bound local variable bound at level "2" and a free local variable
      --       with unique "2".
      case lookupVarEnv tv tyEnv of
        Just boundLvl -> hashWithSalt salt (0 :: Int, boundLvl)
        Nothing -> hashWithSalt salt (1 :: Int, varUniq tv)
    LitTy l -> hashWithSalt (hashTag salt 1) l
    ConstTy c -> hashWithSalt (hashTag salt 2) c
    AnnType attrs t -> go (hashWithSalt (hashTag salt 3) attrs) t
    AppTy t1 t2 -> go (go (hashTag salt 4) t1) t2
    ForAllTy tv t ->
      aTypeHashLevels (lvl + 1) (extendVarEnv tv lvl tyEnv)
        (go (hashTag salt 5) (varType tv)) t

-- | Hash a 'Term' modulo alpha, under the binders enclosing it.
-- See Note [Numbering binders by De Bruijn level].
aTermHashLevels ::
  -- | Number of enclosing binders
  Int ->
  -- | Levels of the term binders enclosing the term
  VarEnv Int ->
  -- | Levels of the type binders enclosing the term
  VarEnv Int ->
  -- | Salt
  Int ->
  Term ->
  Int
aTermHashLevels !lvl tmEnv tyEnv = go
 where
  goType = aTypeHashLevels lvl tyEnv

  underTmBndr b = aTermHashLevels (lvl + 1) (extendVarEnv b lvl tmEnv) tyEnv
  underTyBndr b = aTermHashLevels (lvl + 1) tmEnv (extendVarEnv b lvl tyEnv)

  go :: Int -> Term -> Int
  go salt = \case
    Var i -> goVar (hashTag salt 0) i
    Data dc -> hashWithSalt (hashTag salt 1) dc
    Literal l -> hashWithSalt (hashTag salt 2) l
    -- A primitive is identified by its name
    Prim p -> hashWithSalt (hashTag salt 3) (primName p)
    Cast e t1 t2 -> goType (goType (go (hashTag salt 4) e) t1) t2
    App e1 e2 -> go (go (hashTag salt 5) e1) e2
    TyApp e t -> goType (go (hashTag salt 6) e) t
    Lam b e -> underTmBndr b (goType (hashTag salt 7) (varType b)) e
    TyLam b e -> underTyBndr b (goType (hashTag salt 8) (varType b)) e
    -- A 'NonRec' binder's type is pinned down by its right-hand side, so it is
    -- left out. A 'Rec' binder may occur in its own right-hand side, and then
    -- it is not: @let x = x in x@ is the same term whether @x@ is an Int or a
    -- Bool.
    Let (NonRec i x) e -> underTmBndr i (go (hashTag salt 9) x) e
    Let (Rec bs) e ->
      let (ids, rhss) = unzip bs
          (lvl', tmEnv') = extendLevelsOf lvl ids tmEnv
          under = aTermHashLevels lvl' tmEnv' tyEnv
          types = goList goType (hashTag salt 10) (map varType ids)
      in under (goList under types rhss) e
    -- Note [Case result types and alpha-equivalence]
    Case subj _ty alts -> goList goAlt (go (hashTag salt 11) subj) alts
    Tick tick e -> go (goTick (hashTag salt 12) tick) e

  -- Hash a list, mixing in its length, so that a list does not hash like one of
  -- its prefixes
  goList :: (Int -> a -> Int) -> Int -> [a] -> Int
  goList hashElement salt xs =
    hashWithSalt (List.foldl' hashElement salt xs) (length xs)

  goAlt :: Int -> Alt -> Int
  goAlt salt = \case
    -- A 'DataCon' fixes how many variables its 'DataPat' binds, so the binder
    -- counts need no hashing of their own
    (DataPat dc tvs ids, e) ->
      let (lvlTy, tyEnv') = extendLevelsOf lvl tvs tyEnv
          (lvl', tmEnv') = extendLevelsOf lvlTy ids tmEnv
      in aTermHashLevels lvl' tmEnv' tyEnv'
           (hashWithSalt (hashTag salt 0) dc) e
    (LitPat l, e) -> go (hashWithSalt (hashTag salt 1) l) e
    (DefaultPat, e) -> go (hashTag salt 2) e

  -- N.B.: Variables are hashed with a "tag" to differentiate between, e.g., a
  --       local free variable with unique "2" and a bound free variable which
  --       happens to be bound at level 2.
  goVar :: Int -> Id -> Int
  goVar salt i
    -- Global, never bound with respect to 'tmEnv'
    | isGlobalId i = hashWithSalt salt (0 :: Int, varUniq i)
    -- Local, bound variable
    | Just boundLvl <- lookupVarEnv i tmEnv = hashWithSalt salt (1 :: Int, boundLvl)
    -- Free, local variable
    | otherwise = hashWithSalt salt (2 :: Int, varUniq i)

  -- A tick's payload lives in the scope enclosing the tick, so it is hashed
  -- under the enclosing binders rather than in isolation
  goTick :: Int -> TickInfo -> Int
  goTick salt = \case
    SrcSpan s -> hashSrcSpan (hashTag salt 0) s
    NameMod m t -> goType (hashWithSalt (hashTag salt 1) m) t
    DeDup -> hashTag salt 2
    NoDeDup -> hashTag salt 3
    Attributes t e -> go (goType (hashTag salt 4) t) e

-- | Hash a 'Type' modulo alpha.
-- See Note [Numbering binders by De Bruijn level].
aTypeHashWithSalt :: Int -> Type -> Int
aTypeHashWithSalt = aTypeHashLevels 0 emptyVarEnv

-- | Hash a 'Term' modulo alpha.
-- See Note [Numbering binders by De Bruijn level].
aTermHashWithSalt :: Int -> Term -> Int
aTermHashWithSalt = aTermHashLevels 0 emptyVarEnv emptyVarEnv

instance Hashable Type where
  hashWithSalt = aTypeHashWithSalt

instance Hashable Term where
  hashWithSalt = aTermHashWithSalt
