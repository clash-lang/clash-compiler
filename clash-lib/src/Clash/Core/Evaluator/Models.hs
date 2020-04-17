{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Clash.Core.Evaluator.Models where

import Control.Concurrent.Supply (Supply)
import Control.Monad ((>=>))
import Control.Monad.State.Strict (State)
import qualified Control.Monad.State.Strict as State (runState)
import Control.DeepSeq (NFData(..))
import Data.Bifunctor (first, second)
import Data.Foldable (foldl')
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import GHC.Generics (Generic)

import Debug.Trace -- TODO

import Clash.Core.DataCon
import Clash.Core.FreeVars (localFVsOfTerms, tyFVsOfTypes)
import Clash.Core.Literal (Literal)
import Clash.Core.Subst (extendTvSubstList, mkSubst, substTm)
import Clash.Core.Term (Term(..), Pat, PrimInfo, TickInfo, mkApps)
import Clash.Core.TyCon (TyConMap)
import Clash.Core.Type
import Clash.Core.Var (Id, IdScope(..), TyVar)
import Clash.Core.VarEnv
  ( InScopeSet, extendInScopeSet, mkInScopeSet
  , VarEnv, delVarEnv, extendVarEnv, lookupVarEnv
  , unionVarSet, uniqAway
  )

import Clash.Driver.Types (BindingMap, Binding(..))

nf
  :: Evaluator
  -> BindingMap
  -> EnvPrimsIO
  -> TyConMap
  -> InScopeSet
  -> Supply
  -> Term
  -> (Nf, EnvPrimsIO, EnvTmMap)
nf = partialEval evaluateNf

whnf
  :: Evaluator
  -> BindingMap
  -> EnvPrimsIO
  -> TyConMap
  -> InScopeSet
  -> Supply
  -> Term
  -> (Value, EnvPrimsIO, EnvTmMap)
whnf = partialEval evaluateWhnf

partialEval
  :: (AsTerm a)
  => (Evaluator -> Term -> State Env a)
  -> Evaluator
  -> BindingMap
  -> EnvPrimsIO
  -> TyConMap
  -> InScopeSet
  -> Supply
  -> Term
  -> (a, EnvPrimsIO, EnvTmMap)
partialEval f eval bs ps tcm iss ids x =
    (x', envPrimsIO env', envLocals env')
 where
  (x', env') = State.runState (f eval x) env
  env = mkEnv bs ps tcm iss ids

-- | An evaluator contains the basic functions that define partial evaluation.
-- This consists of:
--
--   * evaluateWhnf, which evaluates terms to WHNF
--   * quoteNf, which recursively evaluates sub-terms and eta-expands to NF
--
-- Users of an evalautor can choose whether they convert terms to WHNF or NF,
-- as the 'AsTerm' class (which converts back to Term) has instances for both.
--
data Evaluator = Evaluator
  { evaluateWhnf :: Term -> State Env Value
  , quoteNf      :: Value -> State Env Nf
  }

-- | Evaluate a term to normal form without stopping at WHNF.
--
evaluateNf :: Evaluator -> Term -> State Env Nf
evaluateNf (Evaluator e q) = e >=> q

-- Local bindings are not stored in a VarEnv, as we still want to be
-- able to access the keys (VarEnv simply indexes by unique).

type EnvTyMap = Map TyVar Type
type EnvTmMap = Map Id (Either Term Value)

-- | EnvGlobals refers to global bindings in scope during evalaution, i.e.
-- other top-level functions used in a program.
--
type EnvGlobals = VarEnv (Binding (Either Term Value))

-- | EnvPrimsIO refers to the result of IO operations performed in primitives
-- during evalaution e.g. creating a new ByteArray in the GHC frontend.
--
type EnvPrimsIO = (IntMap Value, Int)

-- | An Environment contains all in scope terms and types while evaluating.
-- This consists of
--
--   * local types and terms from the term being evaluated
--   * top-level definitions in scope (i.e. global bindings)
--   * IO results of primitive operations
--   * in scope uniques and a means of generating uniques
--
-- When running the partial evaluator at different times, it may be desirable
-- to keep the globals (some of which may have been partially evaluated) and
-- the results of primitives in IO for future runs.
--
data Env = Env
  { envTypes   :: !EnvTyMap
  , envLocals  :: !EnvTmMap
  , envGlobals :: !EnvGlobals
  , envPrimsIO :: !EnvPrimsIO
  , envTcMap   :: !TyConMap
  , envInScope :: !InScopeSet
  , envSupply  :: Supply
  }

instance Show Env where
  show env = show (envTypes env, envLocals env, envPrimsIO env)

-- This instance cannot be derived with Generic, as Supply does not have an
-- NFData instance. Instead, we use this instance which forces all fields
-- which can be forced.
--
instance NFData Env where
  rnf env = rnf (envTypes env)
    `seq` rnf (envLocals env)
    `seq` rnf (envGlobals env)
    `seq` rnf (envPrimsIO env)
    `seq` rnf (envTcMap env)
    `seq` rnf (envInScope env)

mkEnv :: BindingMap -> EnvPrimsIO -> TyConMap -> InScopeSet -> Supply -> Env
mkEnv bs = Env mempty mempty gs
 where
  gs = fmap Left <$> bs

-- lookup

lookupLocal :: Id -> Env -> Maybe (Either Term Value)
lookupLocal i = Map.lookup i . envLocals

lookupGlobal :: Id -> Env -> Maybe (Binding (Either Term Value))
lookupGlobal i = lookupVarEnv i . envGlobals

lookupPrim :: Int -> Env -> Maybe Value
lookupPrim i = IntMap.lookup i . fst . envPrimsIO

-- insert

-- | Add a new type to the environment. The unique for the new
-- element is guaranteed to be unique within the environment.
--
insertType :: TyVar -> Type -> Env -> Env
insertType i ty env = env
  { envTypes = Map.insert i' ty (envTypes env)
  , envInScope = extendInScopeSet (envInScope env) i'
  }
 where
  i' = uniqAway (envInScope env) i

insertTypes :: [(TyVar, Type)] -> Env -> Env
insertTypes xs env = foldl' (flip $ uncurry insertType) env xs

-- | Add a new term / value to the local environment. The unique for the new
-- element is guaranteed to be unique within the environment.
--
insertLocal :: Id -> Either Term Value -> Env -> Env
insertLocal i etv env = env
  { envLocals  = Map.insert i' etv (envLocals env)
  , envInScope = extendInScopeSet (envInScope env) i'
  }
 where
  i' = uniqAway (envInScope env) i

insertLocals :: [(Id, Either Term Value)] -> Env -> Env
insertLocals xs env = foldl' (flip $ uncurry insertLocal) env xs

-- | Add a new primitive to the environment. Primitives are keyed by an
-- integer ID in the evaluator. If the prim already exists in the environment,
-- you should call 'updateEnvPrim' instead.
--
insertPrim :: Value -> Env -> Env
insertPrim v env =
  env { envPrimsIO = (IntMap.insert n v pm, n + 1) }
 where
  (pm, n) = envPrimsIO env

-- update

-- | Update a local binding in the environment. If the Id is not in the
-- local environment, the original environment is returned.
--
updateLocal :: Id -> Either Term Value -> Env -> Env
updateLocal = updatePure LocalId

-- | Update a global binding in the environment. If the Id is not in the
-- global environment, the original environment is returned.
--
updateGlobal :: Id -> Either Term Value -> Env -> Env
updateGlobal = updatePure GlobalId

updatePure :: IdScope -> Id -> Either Term Value -> Env -> Env
updatePure scope i etv env =
  case scope of
    LocalId
      | Map.member i (envLocals env) ->
          env { envLocals = Map.insert i etv (envLocals env) }

    GlobalId
      | Just b <- lookupVarEnv i (envGlobals env) ->
          let b' = b { bindingTerm = etv}
           in env { envGlobals = extendVarEnv i b' (envGlobals env) }

    _ -> env

updatePrim :: Int -> Value -> Env -> Env
updatePrim i v env =
  env { envPrimsIO = (IntMap.insert i v pm, n) }
 where
  (pm, n) = envPrimsIO env

-- delete

deleteLocal :: Id -> Env -> Env
deleteLocal = deletePure LocalId

deleteGlobal :: Id -> Env -> Env
deleteGlobal = deletePure GlobalId

deletePure :: IdScope -> Id -> Env -> Env
deletePure scope i env =
  case scope of
    LocalId -> env { envLocals = Map.delete i (envLocals env) }
    GlobalId -> env { envGlobals = delVarEnv (envGlobals env) i }

-- | Neutral terms cannot be reduced, as they represent things like variables
-- which are unknown, partially applied functions, or case expressions where
-- the scrutinee is not yet an inspectable value. Consider:
--
-- v              Stuck if "v" is a free variable
-- c x1 ... xn    Stuck if constructor "c" is not fully applied
-- p x1 ... xn    Stuck if primitive "p" is not fully applied
-- x $ y          Stuck if "x" is not known to be a lambda
-- x @ A          Stuck if "x" is not known to be a type lambda
-- case x of ...  Stuck if "x" is neutral (cannot choose an alternative)
--
data Neutral a
  = NeVar   Id
  | NeData  DataCon [Either Value Type]
  | NePrim  PrimInfo [Either Value Type]
  | NeApp   (Neutral a) a
  | NeTyApp (Neutral a) Type
  | NeCase  a Type [(Pat, a)]
  deriving (Show, Generic, NFData)

-- | A term which has been normalised to weak head normal form (WHNF). This has
-- no redexes at the head of the term, but subterms may still contain redexes.
--
-- Primitives and data constructors are only considered to be values when
-- fully applied. When partially applied, they are both considered the same as
-- stuck function application (as data constructors and primitives have the
-- same types as normal functions).
--
-- TODO: Document why Env is only in VLam and VTyLam. Needs semantics to
-- reference for clarity.
--
data Value
  = VNeu    (Neutral Value)
  | VLit    Literal
  | VData   DataCon [Either Value Type]
  | VPrim   PrimInfo [Either Value Type]
  | VLam    Id Term Env
  | VTyLam  TyVar Term Env
  | VCast   Value Type Type
  | VTick   Value TickInfo
  deriving (Show, Generic, NFData)

collectValueTicks :: Value -> (Value, [TickInfo])
collectValueTicks = go []
 where
  go acc (VTick v ti) = go (ti:acc) v
  go acc v = (v, acc)

addTicks :: Value -> [TickInfo] -> Value
addTicks = foldl' VTick

-- | A term which is in beta-long eta-normal (NF). This has no redexes, and all
-- partially applied functions in subterms are eta-expanded.
--
data Nf
  = NNeu    (Neutral Nf)
  | NLit    Literal
  | NData   DataCon [Either Nf Type]
  | NPrim   PrimInfo [Either Nf Type]
  | NLam    Id Nf
  | NTyLam  TyVar Nf
  | NCast   Nf Type Type
  | NTick   Nf TickInfo
  deriving (Show, Generic, NFData)

-- Embedding WHNF and HNF values back into Term.
--
class AsTerm a where
  asTerm :: a -> Term

instance AsTerm Term where
  asTerm = id

instance (AsTerm a) => AsTerm (Neutral a) where
  asTerm = \case
    NeVar v -> Var v
    NeData dc args -> traceShow binders $ mkApps (Data dc) (first asTerm <$> args)
      where
       binders = drop (length args) . fst $ splitFunForallTy (dcType dc)

    NePrim p args -> mkApps (Prim p) (first asTerm <$> args)
    NeApp x y -> App (asTerm x) (asTerm y)
    NeTyApp x ty -> TyApp (asTerm x) ty
    NeCase x ty alts -> Case (asTerm x) ty (second asTerm <$> alts)

instance AsTerm Value where
  asTerm = \case
    VNeu n -> asTerm n
    VLit l -> Literal l
    VData dc args -> mkApps (Data dc) (first asTerm <$> args)
    VPrim p args -> mkApps (Prim p) (first asTerm <$> args)
    VLam x e env -> instHeap env $ bindHeap env (Lam x e)
    VTyLam x e env -> instHeap env $ bindHeap env (TyLam x e)
    VCast x a b -> Cast (asTerm x) a b
    VTick x ti -> Tick ti (asTerm x)
   where
    -- Instantiate types which have been bound in the environment. This
    -- performs all type substitutions from the environment at once.
    --
    instHeap env x = substTm "instHeap" subst x
     where
      termIds = localFVsOfTerms [x]
      termTvs = tyFVsOfTypes (envTypes env)
      inScope = mkInScopeSet (unionVarSet termIds termTvs)
      subst   = extendTvSubstList (mkSubst inScope) (Map.toList $ envTypes env)

    -- To prevent a potential explosion of common subexpressions, the term
    -- is turned to a letrec if the local environment is not empty. It is
    -- feasible to remove this and substitute (like instHeap) if CSE is
    -- particularly effective.
    --
    bindHeap env x
      | null bs = x
      | otherwise = Letrec bs x
     where
      bs = Map.toList $ fmap asTerm (envLocals env)

instance AsTerm Nf where
  asTerm = \case
    NNeu n -> asTerm n
    NLit l -> Literal l
    NData dc args -> mkApps (Data dc) (first asTerm <$> args)
    NPrim p args -> mkApps (Prim p) (first asTerm <$> args)
    NLam x e -> Lam x (asTerm e)
    NTyLam x e -> TyLam x (asTerm e)
    NCast x a b -> Cast (asTerm x) a b
    NTick x ti -> Tick ti (asTerm x)

instance (AsTerm a, AsTerm b) => AsTerm (Either a b) where
  asTerm = either asTerm asTerm
