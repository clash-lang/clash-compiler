{-|
Copyright   : (C) 2021-2026, QBayLogic B.V.
License     : BSD2 (see the file LICENSE)
Maintainer  : QBayLogic B.V. <devops@qbaylogic.com>

Random generation of core variables, and the environments of the variables in
scope where types and terms are generated.

Types and terms of scoped Clash Core ("Clash.Core.Scoped.Syntax") are indexed
by their scope. The generators generate them in an 'Env', which has the
'Scope' and the variables in it that a generated type or term may mention.
The generators that bind variables give them fresh binders in that scope
('genTyVar', 'genLocalId'), and pass the extended environment to a
continuation, so generated types and terms are well-scoped by construction.
-}

{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}

module Clash.Hedgehog.Core.Var
  ( -- * Environments
    Env(..)
  , emptyEnv
  , extendEnv
    -- * Variables
  , genTyVar
  , genTyVars
  , genLocalId
  , genLocalIds
  , genGlobalId
  ) where

import Hedgehog (MonadGen)

import qualified Clash.Core.Name as C
import qualified Clash.Core.Term as C (Term)
import qualified Clash.Core.Type as C (Type)
import Clash.Core.Scoped.Convert (mkGlobalId)
import Clash.Core.Scoped.Subst (withFreshIdBinder, withFreshIdBinders, withFreshTyBinder)
import Clash.Core.Scoped.Syntax

-- | The variables in scope where a type or term is generated
data Env n = Env
  { envScope :: Scope n
  , envTyVars :: [Type n]
  -- ^ The type variables in scope, as occurrences ('VarTy'), which carry
  -- their kinds. Innermost first.
  , envTmVars :: [Term n]
  -- ^ The term variables in scope, as occurrences ('Var'), which carry their
  -- types. Innermost first.
  }

-- | The environment of closed types and terms
emptyEnv :: Env 'VoidS
emptyEnv = Env emptyScope [] []

-- | Extend an environment with the given scope, and the type variables and
-- term variables that it adds
extendEnv :: DExt n l => Scope l -> [Type l] -> [Term l] -> Env n -> Env l
extendEnv sc tvs ids (Env _ tvs0 ids0) =
  Env sc (tvs <> map sink tvs0) (ids <> map sink ids0)

-- | Bind a fresh type variable of the specified kind, with a name from the
-- given generator, and add it to the environment.
genTyVar
  :: forall m n r
   . (MonadGen m, Distinct n)
  => Env n
  -> Kind n
  -> m (C.Name C.Type)
  -> (forall l. DExt n l => Env l -> TyBinder n l -> m r)
  -> m r
genTyVar env kn genName k = do
  name <- genName
  withFreshTyBinder (envScope env) name kn $ \sc b ->
    k (extendEnv sc [tyBinderVar b] [] env) b

-- | Bind fresh type variables of the specified kinds, in order, see
-- 'genTyVar'. The kinds are in the scope of the environment. Gives the new
-- type variables too.
genTyVars
  :: forall m n r
   . (MonadGen m, Distinct n)
  => Env n
  -> [Kind n]
  -> m (C.Name C.Type)
  -> (forall l. DExt n l => Env l -> Tele TyBinder n l -> [Type l] -> m r)
  -> m r
genTyVars env [] _ k = k env TNil []
genTyVars env (kn:kns) genName k =
  genTyVar env kn genName $ \(env1 :: Env i) b ->
    genTyVars env1 (map sink kns) genName $ \(env2 :: Env l) bs vs ->
      withExtTrans @n @i @l (k env2 (TCons b bs) (sink (tyBinderVar b) : vs))

-- | Bind a fresh local identifier of the specified type, with a name from the
-- given generator, and add it to the environment.
genLocalId
  :: forall m n r
   . (MonadGen m, Distinct n)
  => Env n
  -> Type n
  -> m (C.Name C.Term)
  -> (forall l. DExt n l => Env l -> IdBinder n l -> m r)
  -> m r
genLocalId env ty genName k = do
  name <- genName
  withFreshIdBinder (envScope env) name ty $ \sc b ->
    k (extendEnv sc [] [idBinderVar b] env) b

-- | Bind fresh local identifiers of the specified types, see 'genLocalId'.
-- The types are in the scope of the environment. Gives the new variables too.
genLocalIds
  :: forall m n r
   . (MonadGen m, Distinct n)
  => Env n
  -> [Type n]
  -> m (C.Name C.Term)
  -> (forall l. DExt n l => Env l -> Tele IdBinder n l -> [Term l] -> m r)
  -> m r
genLocalIds env tys genName k = do
  names <- traverse (const genName) tys
  withFreshIdBinders (envScope env) (zip names tys) $ \sc bs vs ->
    k (extendEnv sc [] vs env) bs vs

-- | Generate a global identifier of the specified (closed) type.
genGlobalId :: forall m. MonadGen m => Type 'VoidS -> m (C.Name C.Term) -> m GlobalId
genGlobalId ty genName = do
  name <- genName
  pure (mkGlobalId name ty)
