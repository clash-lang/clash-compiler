module Clash.Core.Rename
  ( -- * Dual renaming
    RnEnv
    -- ** Construction
  , mkRnEnv
    -- ** Renaming
  , rnTmBndr
  , rnTyBndr
  , rnTmBndrs
  , rnTyBndrs
  , rnOccLId
  , rnOccRId
  , rnOccLTy
  , rnOccRTy
  ) where

import qualified Data.List as List (foldl')
import qualified Data.List.Extra as List (zipEqual)
import           Data.Maybe (fromMaybe)

import           Clash.Core.Var (Id, TyVar, Var)
import           Clash.Core.VarEnv

-- | Rename environment for e.g. alpha equivalence
--
-- When going under binders for e.g.
--
-- @
-- \x -> e1  `aeq` \y -> e2
-- @
--
-- We want to rename @[x -> y]@  or @[y -> x]@, but we have to pick a binder
-- that is neither free in @e1@ nor @e2@ or we risk accidental capture.
--
-- So we must maintain:
--
--   1. A renaming for the left term
--
--   2. A renaming for the right term
--
--   3. A set of in scope variables
data RnEnv = RnEnv
  { rnEnvLTy :: VarEnv TyVar
    -- ^ Type renaming for the left term
  , rnEnvLTm :: VarEnv Id
    -- ^ Term renaming for the left term
  , rnEnvRTy :: VarEnv TyVar
    -- ^ Type renaming for the right term
  , rnEnvRTm :: VarEnv Id
    -- ^ Term renaming for the right term
  , rnEnvIss :: InScopeSet
    -- ^ In scope in left or right terms
  }

-- | Create an empty renaming environment
mkRnEnv :: InScopeSet -> RnEnv
mkRnEnv vars = RnEnv
  { rnEnvLTy = emptyVarEnv
  , rnEnvLTm = emptyVarEnv
  , rnEnvRTy = emptyVarEnv
  , rnEnvRTm = emptyVarEnv
  , rnEnvIss = vars
  }

-- | Look up the renaming an an occurrence in the given map.
rnOcc :: (RnEnv -> VarEnv (Var a)) -> RnEnv -> Var a -> Var a
rnOcc f rn v = fromMaybe v (lookupVarEnv v (f rn))

-- | Look up the renaming of an type-variable occurrence in the left term
rnOccLTy :: RnEnv -> TyVar -> TyVar
rnOccLTy = rnOcc rnEnvLTy

-- | Look up the renaming of an type-variable occurrence in the right term
rnOccRTy :: RnEnv -> TyVar -> TyVar
rnOccRTy = rnOcc rnEnvRTy

-- | Look up the renaming of an occurrence in the left term
rnOccLId :: RnEnv -> Id -> Id
rnOccLId = rnOcc rnEnvLTm

-- | Look up the renaming of an occurrence in the left term
rnOccRId :: RnEnv -> Id -> Id
rnOccRId = rnOcc rnEnvRTm

{-
NOTE [Rebinding and shadowing]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Imagine:

@
\x -> \x -> e1  `aeq` \y -> \x -> e2
@

Then inside

@
\x \y  { [x->p] [y->p]  {p} }
\x \z  { [x->q] [y->p, z->q] {p,q} }
@

i.e. if the new var is the same as the old var, the renaming is deleted by
'extendVarEnv'
-}

-- | Simultaneously go under the type-variable binders /bL/ and /bR/, find a
-- new binder /newB/, and return an environment mapping @[bL -> newB]@ and
-- @[bR -> newB]@.
rnTyBndr :: RnEnv -> TyVar -> TyVar -> RnEnv
rnTyBndr rv@(RnEnv lenv _ renv _ inScope) bL bR =
  -- See Note [Rebinding and shadowing]
  rv { rnEnvLTy = extendVarEnv bL newB lenv
     , rnEnvRTy = extendVarEnv bR newB renv
     , rnEnvIss = extendInScopeSet inScope newB
     }
 where
  -- Find a new binder not in scope in either type
  newB | not (bL `elemInScopeSet` inScope) = bL
       | not (bR `elemInScopeSet` inScope) = bR
       | otherwise                         = uniqAway inScope bL

-- | Simultaneously go under the binders /bL/ and /bR/, find a new binder
-- /newB/, and return an environment mapping @[bL -> newB]@ and @[bR -> newB]@.
rnTmBndr :: RnEnv -> Id -> Id -> RnEnv
rnTmBndr rv@(RnEnv _ lenv _ renv inScope) bL bR =
  -- See Note [Rebinding and shadowing]
  rv { rnEnvLTm = extendVarEnv bL newB lenv
     , rnEnvRTm = extendVarEnv bR newB renv
     , rnEnvIss = extendInScopeSet inScope newB
     }
 where
  -- Find a new binder not in scope in either term
  newB | not (bL `elemInScopeSet` inScope) = bL
       | not (bR `elemInScopeSet` inScope) = bR
       | otherwise                         = uniqAway inScope bL

rnBndrs :: (RnEnv -> a -> a -> RnEnv) -> RnEnv -> [a] -> [a] -> RnEnv
rnBndrs f env xs ys =
  List.foldl' (\acc (x, y) -> f acc x y) env (List.zipEqual xs ys)

-- | Applies 'rnTyBndr' to several variables: the two variable lists must be of
-- equal length.
rnTyBndrs :: RnEnv -> [TyVar] -> [TyVar] -> RnEnv
rnTyBndrs = rnBndrs rnTyBndr

-- | Applies 'rnTmBndr' to several variables: the two variable lists must be of
-- equal length.
rnTmBndrs :: RnEnv -> [Id] -> [Id] -> RnEnv
rnTmBndrs = rnBndrs rnTmBndr
