{-|
Copyright     : (C) 2020, QBayLogic B.V.
License       : BSD2 (see the file LICENSE)
Maintainer    : QBayLogic B.V. <devops@qbaylogic.com>

Data types and main API for the partial evaluator. This defines the type of
evaluation, Eval, and specifies the basic operations that are used to define
evaluation operations. This module also provides the types for WHNF terms
(Value), beta-normal eta-long form terms (NF) and stuck terms (Neutral).
-}

{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Clash.Core.Evaluator.Models where

import Control.Concurrent.Supply (Supply)
import Control.Monad.RWS.Strict (MonadReader, MonadState, RWS)
import qualified Control.Monad.RWS.Strict as RWS
import Data.Bifunctor (first, second)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Data.IntMap.Strict (IntMap)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Set.Lens as Set
import Data.Text (Text)

import Clash.Core.DataCon
import Clash.Core.FreeVars (localFVsOfTerms, tyFVsOfTypes, freeLocalIds)
import Clash.Core.Literal
import Clash.Core.Name (OccName)
import Clash.Core.Subst (extendTvSubstList, mkSubst, substTm)
import Clash.Core.Term
import Clash.Core.Termination
import Clash.Core.TyCon (TyConMap)
import Clash.Core.Type
import Clash.Core.Util (mkUniqSystemId, mkUniqSystemTyVar)
import Clash.Core.Var (Id, Var)
import Clash.Core.VarEnv
import Clash.Driver.Types (Binding(..), BindingMap)

-- | The type of partial evaluation. This keeps local bindings in a Reader and
-- global bindings in State to preserve scoping. This allows changes to global
-- state to bubble up, while preventing the same in local bindings (to avoid
-- bindings escaping their scope). For example, consider the term
--
--   (let ... in f) (let ... in x)
--
-- We want any global bindings evaluated in the subterms to be evaluated only
-- once, otherwise there is potentially a lot of redundant work performed.
-- However, if State is used for all parts of the environment, the local state
-- would have to be saved before evaluating (let ... in f), restored before
-- evaluating (let ... in x), and restored yet again before returning the
-- result. If this is not done let-bound definitions would float upwards,
-- potentially changing the result of evaluation.
--
newtype Eval a = Eval { unEval :: RWS LocalEnv () GlobalEnv a }
  deriving
    ( Functor
    , Applicative
    , Monad
    , MonadReader LocalEnv
    , MonadState GlobalEnv
    )

-- | Run a computation with the partial evaluator, starting from the given
-- global environment. The local environment is discarded at the end of
-- computation, and the new global environment is returned with the result.
--
runEval :: GlobalEnv -> Eval a -> (a, GlobalEnv)
runEval genv x =
  let lenv = LocalEnv mempty mempty
      (x', genv', _) = RWS.runRWS (unEval x) lenv genv
   in (x', genv')

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
  { evaluateWhnf :: Term -> Eval Value
  , quoteNf      :: Value -> Eval Nf
  }

-- | Evaluate a term to normal form without stopping at WHNF.
--
evaluateNf :: Evaluator -> Term -> Eval Nf
evaluateNf (Evaluator e q) x = e x >>= q

type TermOrValue = Either Term Value

-- | Local Environment
--
data LocalEnv = LocalEnv
  { lenvTypes  :: Map TyVar Type
    -- ^ Local type environment. These are types that are introduced while
    -- evaluating the current term (e.g. by type applications).
  , lenvTerms  :: Map Id TermOrValue
    -- ^ Local term environment. These are the terms that are introduced while
    -- evaluating the current term (e.g. by applications).
  } deriving (Show)

getLocalEnv :: Eval LocalEnv
getLocalEnv = RWS.ask

withLocalEnv :: LocalEnv -> Eval a -> Eval a
withLocalEnv env = RWS.local (const env)

-- | Lookup a local term binding in the environment.
--
getLocal :: Id -> Eval (Maybe TermOrValue)
getLocal i = Map.lookup i <$> RWS.asks lenvTerms

-- | Add a local term binding to the environment, then evaluate the given
-- action. The new binding only exists when evaluating the given action.
--
withLocal :: (Id, TermOrValue) -> Eval a -> Eval a
withLocal (i, tm) = RWS.local addBinding
 where
  addBinding env@(LocalEnv _ tms) =
    env { lenvTerms = Map.insert i tm tms }

-- | Remove a local term binding from the environment, then evaluate the given
-- action. The binding is only removed when evaluating the given action.
--
withoutLocal :: Id -> Eval a -> Eval a
withoutLocal i = RWS.local deleteBinding
 where
  deleteBinding env@(LocalEnv _ tms) =
    env { lenvTerms = Map.delete i tms }

withLocals :: [(Id, TermOrValue)] -> Eval a -> Eval a
withLocals bs x = foldr withLocal x bs

-- | Lookup a local type binding in the environment.
--
getType :: TyVar -> Eval (Maybe Type)
getType i = Map.lookup i <$> RWS.asks lenvTypes

-- | Add a local type binding to the environment, then evaluate the given
-- action. The new binding only exists when evaluating the given action.
--
withType :: (TyVar, Type) -> Eval a -> Eval a
withType (i, ty) = RWS.local addBinding
 where
  addBinding env@(LocalEnv tys _) =
    env { lenvTypes = Map.insert i ty tys }

withTypes :: [(TyVar, Type)] -> Eval a -> Eval a
withTypes bs x = foldr withType x bs

-- | Global Environment
--
data GlobalEnv = GlobalEnv
  { genvGlobals :: VarEnv (Binding TermOrValue)
    -- ^ Global term environment. These are functions in global scope which
    -- are evaluated on lookup, and updated after evaluation.
  , genvRecInfo :: RecInfo
    -- ^ Information about which global bindings are recursive, used to decide
    -- whether or not to inline a global binding.
  , genvPrimInfos :: HashMap Text PrimInfo
    -- ^ The PrimInfo for each primtiive used in the design.
  , genvFuel :: Word
    -- ^ Remaining fuel for inlining. This decreases when a potentially
    -- non-terminating binder is inlined and increases when moving up the AST.
  , genvPrimsIO :: GlobalIO
    -- ^ The results of IO actions performed while evaluating primitives. This
    -- allows primtives in IO to be potentially evaluated at compile-time.
  , genvTyCons  :: TyConMap
    -- ^ The type constructors known about by Clash.
  , genvInScope :: InScopeSet
    -- ^ The set of in scope variables. This is used to prevent collisions
    -- when generating new identifiers during evaluation.
  , genvSupply  :: Supply
    -- ^ The supply of fresh names for generating identifiers.
  }

-- | The result of IO actions performed during evaluation are stored in the
-- global environment. This allows IO actions to be evaluated at compile time
-- where possible.
--
type GlobalIO = (IntMap Value, Int)

mkGlobalEnv
  :: BindingMap
  -> RecInfo
  -> HashMap Text PrimInfo
  -> Word
  -> GlobalIO
  -> TyConMap
  -> InScopeSet
  -> Supply
  -> GlobalEnv
mkGlobalEnv bs =
  GlobalEnv (fmap Left <$> bs)

getGlobal :: Id -> Eval (Maybe (Binding TermOrValue))
getGlobal i = lookupVarEnv i <$> RWS.gets genvGlobals

-- | Update a global binding, replacing it with a WHNF representation.
--
updateGlobal :: Id -> Value -> Eval ()
updateGlobal i x =
  getGlobal i >>= \case
    Just b -> do
      env <- RWS.get
      let bs = genvGlobals env
      let b' = b { bindingTerm = Right x }

      RWS.put (env { genvGlobals = extendVarEnv i b' bs })

    Nothing ->
      pure ()

getRecInfo :: Eval RecInfo
getRecInfo = RWS.gets genvRecInfo

getPrimInfo :: Text -> Eval (Maybe PrimInfo)
getPrimInfo x = HashMap.lookup x <$> RWS.gets genvPrimInfos

getFuel :: Eval Word
getFuel = RWS.gets genvFuel

putFuel :: Word -> Eval ()
putFuel x = RWS.modify' (\env -> env { genvFuel = x })

-- | Run the given action, preserving the amount of fuel before the action was
-- run. This allows branching computations to use the same amount of fuel for
-- each branch.
--
preserveFuel :: Eval a -> Eval a
preserveFuel x = do
  fuel <- RWS.gets genvFuel
  res  <- x
  env  <- RWS.get

  RWS.put (env { genvFuel = fuel })
  pure res

getTyConMap :: Eval TyConMap
getTyConMap = RWS.gets genvTyCons

mkUniqueVar
  :: ((Supply, InScopeSet)
        -> (OccName, KindOrType)
        -> ((Supply, InScopeSet), Var a))
  -> OccName
  -> KindOrType
  -> Eval (Var a)
mkUniqueVar f n x = do
  env <- RWS.get
  let iss = genvInScope env
      ids = genvSupply env
      ((ids', iss'), i) = f (ids, iss) (n, x)

  RWS.put (env { genvInScope = iss', genvSupply = ids' })
  pure i

mkUniqueId :: OccName -> Type -> Eval Id
mkUniqueId = mkUniqueVar mkUniqSystemId

mkUniqueTyVar :: OccName -> Kind -> Eval TyVar
mkUniqueTyVar = mkUniqueVar mkUniqSystemTyVar

-- | The result of evaluating the pattern in a case expression. This identifies
-- whether the pattern matched, and contains any identifiers that the pattern
-- brings into scope.
--
data PatResult
  = NoMatch
  | Match ![(TyVar, Type)] ![(Id, Term)]

-- | Neutral terms cannot be reduced, as they represent things like variables
-- which are unknown, partially applied functions, or case expressions where
-- the scrutinee is not yet an inspectable value. Consider:
--
-- v              Stuck if "v" is a free variable
-- p x1 ... xn    Stuck if "p" is a primitive that cannot be reduced
-- x $ y          Stuck if "x" is not known to be a lambda
-- x @ A          Stuck if "x" is not known to be a type lambda
-- case x of ...  Stuck if "x" is neutral (cannot choose an alternative)
--
data Neutral a
  = NeVar   !Id
  | NePrim  !PrimInfo ![Either a Type]
  | NeApp   !(Neutral a) !a
  | NeTyApp !(Neutral a) !Type
  | NeCase  !a !Type ![(Pat, a)]
  deriving (Show)

-- | A term which has been normalised to weak head normal form (WHNF). This has
-- no redexes at the head of the term, but subterms may still contain redexes.
--
-- Primitives and data constructors are only considered to be values when
-- fully applied. When partially applied, they are both considered the same as
-- stuck function application (as data constructors and primitives have the
-- same types as normal functions).
--
-- Lambdas / type lambdas include the environment used by the evaluator when it
-- was encountered, This is needed when
--
--   * embedding a Value back into a Term.
--
--     When we embed a Value into a Term, applied types and terms must be
--     substituted (or otherwise bound) into the resulting Term. When turning
--     a lambda back to a term, the unevaluated body of the lambda must have
--     these substitutions applied to it.
--
--   * evaluating to NF.
--
--     Recursively evaluating a term can introduce scope issues if the
--     recursive traversals do not use the same local enviroment. By storing
--     the environment in lambdas / type lambdas, recursive evaluation always
--     respects lexical scoping.
--
data Value
  = VNeu    !(Neutral Value)
  | VLit    !Literal
  | VData   !DataCon ![Either Term Type] !LocalEnv
  | VPrim   !PrimInfo ![Either Term Type] !LocalEnv
  | VLam    !Id !Term !LocalEnv
  | VTyLam  !TyVar !Term !LocalEnv
  | VCast   !Value !Type !Type
  | VTick   !Value !TickInfo
  | VThunk  !Term !LocalEnv
  deriving (Show)

collectValueTicks :: Value -> (Value, [TickInfo])
collectValueTicks = go []
 where
  go acc (VTick v ti) = go (ti:acc) v
  go acc v = (v, acc)

addTicks :: Value -> [TickInfo] -> Value
addTicks = foldr (flip VTick)

-- | A term which is in beta-normal eta-long form (NF). This has no redexes,
-- and all partially applied functions in subterms are eta-expanded.
--
data Nf
  = NNeu    !(Neutral Nf)
  | NLit    !Literal
  | NData   !DataCon ![Either Nf Type]
  | NPrim   !PrimInfo ![Either Nf Type]
  | NLam    !Id !Nf
  | NTyLam  !TyVar !Nf
  | NCast   !Nf !Type !Type
  | NTick   !Nf !TickInfo
  deriving (Show)

-- Embedding WHNF and HNF values back into Term.
--
class AsTerm a where
  asTerm :: a -> Term

instance AsTerm Term where
  asTerm = id

instance (AsTerm a) => AsTerm (Neutral a) where
  asTerm = \case
    NeVar v -> Var v
    NePrim p args -> mkApps (Prim p) (first asTerm <$> args)
    NeApp x y -> App (asTerm x) (asTerm y)
    NeTyApp x ty -> TyApp (asTerm x) ty
    NeCase x ty alts -> Case (asTerm x) ty (second asTerm <$> alts)

instance AsTerm Value where
  asTerm = \case
    VNeu n -> asTerm n
    VLit l -> Literal l
    VData dc args env -> instHeap env . bindHeap env $ mkApps (Data dc) args
    VPrim p args env -> instHeap env . bindHeap env $ mkApps (Prim p) args
    VLam x e env -> instHeap env $ bindHeap env (Lam x e)
    VTyLam x e env -> instHeap env $ bindHeap env (TyLam x e)
    VCast x a b -> Cast (asTerm x) a b
    VTick x ti -> Tick ti (asTerm x)
    VThunk x env -> instHeap env $ bindHeap env x
   where
    -- Instantiate types which have been bound in the environment. This
    -- performs all type substitutions from the environment at once.
    --
    instHeap env x = substTm "instHeap" subst x
     where
      termIds = localFVsOfTerms [x]
      termTvs = tyFVsOfTypes (lenvTypes env)
      inScope = mkInScopeSet (unionVarSet termIds termTvs)
      subst   = extendTvSubstList (mkSubst inScope) (Map.toList $ lenvTypes env)

    -- To prevent a potential explosion of common subexpressions, the term
    -- is turned to a letrec if the local environment is not empty. It is
    -- feasible to remove this and substitute (like instHeap) if CSE is
    -- particularly effective.
    --
    bindHeap env x
      | null bs = x
      | otherwise = Letrec bs x
     where
      -- Only bind things which are used in the term
      free = Set.setOf freeLocalIds x
      bs   = Map.toList . fmap asTerm $ Map.restrictKeys (lenvTerms env) free

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

