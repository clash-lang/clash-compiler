{-|
  Copyright   :  (C) 2012-2016, University of Twente,
                          2017, Google Inc.
  License     :  BSD2 (see the file LICENSE)
  Maintainer  :  Christiaan Baaij <christiaan.baaij@gmail.com>

  Capture-free substitution function for CoreHW
-}

{-# LANGUAGE CPP               #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns      #-}

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
    -- * Variable renaming
  , deShadowTerm
  , freshenTm
    -- * Alpha equivalence
  , aeqType
  , aeqTerm
  )
where

import           Data.Coerce               (coerce)
import           Data.Text.Prettyprint.Doc
import qualified Data.List                 as List

import           Clash.Core.FreeVars
  (noFreeVarsOfType, localFVsOfTerms, tyFVsOfTypes)
import           Clash.Core.Pretty         (ppr, fromPpr)
import           Clash.Core.Term
  (LetBinding, Pat (..), Term (..), TickInfo (..))
import           Clash.Core.Type           (Type (..))
import           Clash.Core.VarEnv
import           Clash.Core.Var            (Id, Var (..), TyVar, isGlobalId)
import           Clash.Unique
import           Clash.Util
import           Clash.Pretty

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
  , neLength tvs tys
  = pprTrace "zipTvSubst" (ppr tvs <> line <> ppr tys) emptySubst
  | otherwise
  = Subst (mkInScopeSet (tyFVsOfTypes tys)) emptyVarEnv tenv emptyVarEnv
 where
  tenv = zipTyEnv tvs tys

zipTyEnv
  :: [TyVar]
  -> [Type]
  -> VarEnv Type
zipTyEnv tvs tys = mkVarEnv (zipEqual tvs tys)

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

-- | This checks if the substitution satisfies the invariant from 'TvSbust's
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
        "tenvFVs" <+> clashPretty (tyFVsOfTypes tenv) <> line <>
        "tys" <+> fromPpr tys)
  WARN( not tysFVsInSope,
       "inScope" <+> clashPretty inScope <> line <>
       "tenv" <+> clashPretty tenv <> line <>
       "tys" <+> fromPpr tys <> line <>
       "needsInScope" <+> clashPretty needsInScope)
  a
 where
  needsInScope = foldrWithUnique (\k _ s -> delVarSetByKey k s)
                   (tyFVsOfTypes tys)
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
  tenvFVs = tyFVsOfTypes tenv

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
  no_capture = not (newVar `elemVarSet` tyFVsOfTypes tenv)

  oldKi        = varType oldVar
  -- verify that the kind is closed
  noKindChange = noFreeVarsOfType oldKi
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

-- | Substitute within a 'Type'
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
    Letrec bs e -> case substBind doc subst bs of
      (subst',bs') -> Letrec bs' (substTm doc subst' e)
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
  | Just v' <- lookupInScope inScope v = Var (coerce v')
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
  noTypeChange = nullVarEnv tenv || noFreeVarsOfType oldTy

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
  -> [LetBinding]
  -> (Subst,[LetBinding])
substBind doc subst xs =
  (subst',zip bndrs' rhss')
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
  ASSERT( tvs `equalLength` tys )
  substTy (zipTvSubst tvs tys)

-- | Ensure that non of the binders in an expression shadow each-other, nor
-- conflict with he in-scope set
deShadowTerm
  :: HasCallStack
  => InScopeSet
  -> Term
  -> Term
deShadowTerm is e = substTm "deShadowTerm" (mkSubst is) e

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
    Letrec bs e -> case goBind subst0 bs of
      (subst1,bs') -> case go subst1 e of
        (is2,e') -> (is2,Letrec bs' e')
    Case subj ty alts -> case go subst0 subj of
      (is1,subj') -> case List.mapAccumL (\isN -> goAlt subst0 {substInScope = isN}) is1 alts of
        (is2,alts') -> (is2, Case subj' (substTy subst0 ty) alts')
    Cast e t1 t2 -> case go subst0 e of
      (is1, e') -> (is1, Cast e' (substTy subst0 t1) (substTy subst0 t2))
    Tick tick e -> case go subst0 e of
       (is1, e') -> (is1, Tick (goTick subst0 tick) e')
    tm -> (substInScope subst0, tm)

  goBind subst0 xs =
    let (bndrs,rhss)    = unzip xs
        (subst1,bndrs') = List.mapAccumL substIdBndr subst0 bndrs
        (is2,rhss')     = List.mapAccumL (\isN -> go subst1 {substInScope = isN})
                                         (substInScope subst1)
                                         rhss
    in  (subst1 {substInScope = is2},zip bndrs' rhss')

  goAlt subst0 (pat,alt) = case pat of
    DataPat dc tvs ids -> case List.mapAccumL substTyVarBndr' subst0 tvs of
      (subst1,tvs') -> case List.mapAccumL substIdBndr subst1 ids of
        (subst2,ids') -> case go subst2 alt of
          (is3,alt') -> (is3,(DataPat dc tvs' ids',alt'))
    _ -> case go subst0 alt of
      (is1,alt') -> (is1,(pat,alt'))

  goTick subst0 (NameMod m ty) = NameMod m (substTy subst0 ty)
  goTick _      tick           = tick

-- * AEQ

-- | Alpha equality for types
aeqType
  :: Type
  -> Type
  -> Bool
aeqType t1 t2 = acmpType' rnEnv t1 t2 == EQ
 where
  rnEnv = mkRnEnv (mkInScopeSet (tyFVsOfTypes [t1,t2]))

-- | Alpha comparison for types
acmpType
  :: Type
  -> Type
  -> Ordering
acmpType t1 t2 = acmpType' (mkRnEnv inScope) t1 t2
 where
  inScope = mkInScopeSet (tyFVsOfTypes [t1,t2])

-- | Alpha comparison for types. Faster than 'acmpType' as it doesn't need to
-- calculate the free variables to create the 'InScopeSet'
acmpType'
  :: RnEnv
  -> Type
  -> Type
  -> Ordering
acmpType' = go
 where
  go env (VarTy tv1) (VarTy tv2) = compare (rnOccLTy env tv1) (rnOccRTy env tv2)
  go _   (ConstTy c1) (ConstTy c2) = compare c1 c2
  go env (ForAllTy tv1 t1) (ForAllTy tv2 t2) =
    go env (varType tv1) (varType tv2) `thenCompare` go (rnTyBndr env tv1 tv2) t1 t2
  go env (AppTy s1 t1) (AppTy s2 t2) =
    go env s1 s2 `thenCompare` go env t1 t2
  go _ (LitTy l1) (LitTy l2) = compare l1 l2
  go _ t1 t2 = compare (getRank t1) (getRank t2)

  getRank :: Type -> Word
  getRank (VarTy {})    = 0
  getRank (LitTy {})    = 1
  getRank (ConstTy {})  = 2
  getRank (AnnType {})  = 3
  getRank (AppTy {})    = 4
  getRank (ForAllTy {}) = 5

-- | Alpha equality for terms
aeqTerm
  :: Term
  -> Term
  -> Bool
aeqTerm t1 t2 = aeqTerm' inScope t1 t2
 where
  inScope = mkInScopeSet (localFVsOfTerms [t1,t2])

-- | Alpha equality for terms. Faster than 'aeqTerm' as it doesn't need to
-- calculate the free variables to create the 'InScopeSet'
aeqTerm'
  :: InScopeSet
  -- ^ Superset of variables in scope of the left and right term
  -> Term
  -> Term
  -> Bool
aeqTerm' inScope t1 t2 = acmpTerm' inScope t1 t2 == EQ

-- | Alpha comparison for types
acmpTerm
  :: Term
  -> Term
  -> Ordering
acmpTerm t1 t2 = acmpTerm' inScope t1 t2
 where
  inScope = mkInScopeSet (localFVsOfTerms [t1,t2])

-- | Alpha comparison for types. Faster than 'acmpTerm' as it doesn't need to
-- calculate the free variables to create the 'InScopeSet'
acmpTerm'
  :: InScopeSet
  -- ^ Superset of variables in scope of the left and right term
  -> Term
  -> Term
  -> Ordering
acmpTerm' inScope = go (mkRnEnv inScope)
 where
  thenCmpTm EQ  rel = rel
  thenCmpTm rel _   = rel

  go env (Var id1) (Var id2)   = compare (rnOccLId env id1) (rnOccRId env id2)
  go _   (Data dc1) (Data dc2) = compare dc1 dc2
  go _   (Literal l1) (Literal l2) = compare l1 l2
  go _   (Prim p1 _) (Prim p2 _) = compare p1 p2
  go env (Lam b1 e1) (Lam b2 e2) =
    acmpType' env (varType b1) (varType b2) `thenCompare`
    go (rnTmBndr env b1 b2) e1 e2
  go env (TyLam b1 e1) (TyLam b2 e2) =
    acmpType' env (varType b1) (varType b2) `thenCompare`
    go (rnTyBndr env b1 b2) e1 e2
  go env (App l1 r1) (App l2 r2) =
    go env l1 l2 `thenCompare` go env r1 r2
  go env (TyApp l1 r1) (TyApp l2 r2) =
    go env l1 l2 `thenCompare` acmpType' env r1 r2
  go env (Letrec bs1 e1) (Letrec bs2 e2) =
    compare (length bs1) (length bs2) `thenCompare`
    foldr thenCmpTm EQ (zipWith (go env') rhs1 rhs2) `thenCompare`
    go env' e1 e2
   where
    (ids1,rhs1) = unzip bs1
    (ids2,rhs2) = unzip bs2
    env' = rnTmBndrs env ids1 ids2
  go env (Case e1 _ a1) (Case e2 _ a2) =
    compare (length a1) (length a2) `thenCompare`
    go env e1 e2 `thenCompare`
    foldr thenCmpTm EQ (zipWith (goAlt env) a1 a2)
  go env (Cast e1 l1 r1) (Cast e2 l2 r2) =
    go env e1 e2 `thenCompare`
    acmpType' env l1 l2 `thenCompare`
    acmpType' env r1 r2
  -- Look through ticks for aeq
  go env (Tick _ e1) e2 = go env e1 e2
  go env e1 (Tick _ e2) = go env e1 e2

  go _ e1 e2 = compare (getRank e1) (getRank e2)

  goAlt env (DataPat c1 tvs1 ids1,e1) (DataPat c2 tvs2 ids2,e2) =
    compare c1 c2 `thenCompare` go env' e1 e2
   where
    env' = rnTmBndrs (rnTyBndrs env tvs1 tvs2) ids1 ids2
  goAlt env (c1,e1) (c2,e2) =
    compare c1 c2 `thenCompare` go env e1 e2

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
    Letrec {}  -> 9
    Case {}    -> 10
    Tick {}    -> 11

thenCompare :: Ordering -> Ordering -> Ordering
thenCompare EQ rel = rel
thenCompare rel _  = rel

instance Eq Type where
  (==) = aeqType

instance Ord Type where
  compare = acmpType

instance Eq Term where
  (==) = aeqTerm

instance Ord Term where
  compare = acmpTerm
