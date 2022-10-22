{-|
Copyright   : (C) 2020-2021, QBayLogic B.V.
License     : BSD2 (see the file LICENSE)
Maintainer  : QBayLogic B.V. <devops@qbaylogic.com>

The monad for partial evaluation, and its API. This should contain all
auxiliary functions needed to define new evaluator implementations. This
module is only needed to define new evaluators, for calling an existing
evaluator see Clash.Core.PartialEval.
-}

{-# LANGUAGE CPP #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}

module Clash.Core.PartialEval.Monad
  ( -- * Partial Evaluation Monad
    Eval
  , runEval
    -- * Local and Global Environments
  , getLocalEnv
  , setLocalEnv
  , modifyLocalEnv
  , getGlobalEnv
  , modifyGlobalEnv
    -- * Evaluation Context
  , getContext
  , withContext
    -- * Local Type Bindings
  , getTvSubst
  , findTyVar
  , withTyVar
  , withTyVars
    -- * Local Term Bindings
  , findId
  , withId
  , withIds
  , withoutId
    -- * Global Term Bindings
  , findBinding
  , replaceBinding
    -- * IO Heap Bindings
  , getRef
  , setRef
    -- * Lifted Data Constructors
  , isKeepingLifted
  , keepLifted
    -- * Fuel
  , getFuel
  , withFuel
  , preserveFuel
    -- * Accessing Global State
  , getTyConMap
  , getInScope
    -- * Fresh Variable Generation
  , getUniqueId
  , getUniqueTyVar
    -- * Work free check
  , workFreeValue
  ) where

import           Control.Applicative (Alternative)
import           Control.Concurrent.Supply (Supply)
import           Control.Monad.Catch (MonadThrow, MonadCatch, MonadMask)
import           Control.Monad.IO.Class (MonadIO)

#if !MIN_VERSION_base(4,13,0)
import           Control.Monad.Fail (MonadFail)
#endif

import           Control.Monad.RWS.Strict (RWST, MonadReader, MonadState)
import qualified Control.Monad.RWS.Strict as RWS
import qualified Data.IntMap.Strict as IntMap
import qualified Data.Map.Strict as Map

import           Clash.Core.Binding (Binding(..))
import           Clash.Core.HasFreeVars
import           Clash.Core.InScopeSet (InScopeSet)
import qualified Clash.Core.InScopeSet as InScopeSet
import           Clash.Core.Name (OccName)
import           Clash.Core.PartialEval.AsTerm
import           Clash.Core.PartialEval.NormalForm
import           Clash.Core.Subst (Subst, mkTvSubst)
import           Clash.Core.TyCon (TyConMap)
import           Clash.Core.Type (Kind, KindOrType, Type)
import           Clash.Core.Util (mkUniqSystemId, mkUniqSystemTyVar)
import           Clash.Core.Var (Id, TyVar, Var)
import qualified Clash.Core.VarSet as VarSet
import qualified Clash.Data.UniqMap as UniqMap
import           Clash.Rewrite.WorkFree (isWorkFree)

{-
NOTE [RWS monad]
~~~~~~~~~~~~~~~~
Local bindings are kept in the Reader monad and global bindings in the State
monad. This ensures that global changes are propagated to later evaluation
actions whereas local changes only exist when evaluating a particular sub-term.
For example, consider the term

   (let ... in f) (let ... in x)

When evaluating this, the let bindings in the left sub-term should not be in
scope when evaluating the right sub-term. By using only the State monad for
local and global state, too much care needs to be given to ensuring that local
bindings are saved and restored when evaluating different sub-terms.

The MonadWriter instance is deliberately not derived here, as the Writer monad
functionality of RWST is not wanted.
-}

-- TODO The inner monad here could be changed to STM to allow the evaluator
-- to work on evaluating sub-terms concurrently. That would require slightly
-- different environment types, where data can be stored in STM types.

-- | The monad of partial evaluation. The inner monad is IO, as primitive
-- evaluation can attempt to evaluate IO actions.
--
newtype Eval a = Eval
  { unEval :: RWST LocalEnv () GlobalEnv IO a }
  deriving
    ( Functor
    , Applicative
    , Alternative
    , Monad
    , MonadFail
    , MonadIO
    , MonadReader LocalEnv
    , MonadState GlobalEnv
    , MonadThrow
    , MonadCatch
    , MonadMask
    )

-- | Evaluate an action in the partial evaluator, returning the result,
-- and the final state of the global environment.
--
runEval :: GlobalEnv -> LocalEnv -> Eval a -> IO (a, GlobalEnv)
runEval g l x =
  let extract (a, g', _) = (a, g')
   in extract <$> RWS.runRWST (unEval x) l g
{-# INLINE runEval #-}

getLocalEnv :: Eval LocalEnv
getLocalEnv = RWS.ask
{-# INLINE getLocalEnv #-}

setLocalEnv :: LocalEnv -> Eval a -> Eval a
setLocalEnv = RWS.local . const
{-# INLINE setLocalEnv #-}

modifyLocalEnv :: (LocalEnv -> LocalEnv) -> Eval a -> Eval a
modifyLocalEnv = RWS.local
{-# INLINE modifyLocalEnv #-}

getGlobalEnv :: Eval GlobalEnv
getGlobalEnv = RWS.get
{-# INLINE getGlobalEnv #-}

modifyGlobalEnv :: (GlobalEnv -> GlobalEnv) -> Eval ()
modifyGlobalEnv = RWS.modify'
{-# INLINE modifyGlobalEnv #-}

getContext :: Eval Id
getContext = lenvContext <$> getLocalEnv

withContext :: Id -> Eval a -> Eval a
withContext i = modifyLocalEnv go
 where
  go env = env { lenvContext = i }

findTyVar :: TyVar -> Eval (Maybe Type)
findTyVar i = Map.lookup i . lenvTypes <$> getLocalEnv

withTyVar :: TyVar -> Type -> Eval a -> Eval a
withTyVar i a x = do
  modifyGlobalEnv goGlobal
  modifyLocalEnv goLocal x
 where
  goGlobal env@GlobalEnv{genvInScope=inScope} =
    let fvs = VarSet.insert i (freeVarsOf a)
        iss = InScopeSet.fromVarSet fvs <> inScope
     in env { genvInScope = iss }

  goLocal env@LocalEnv{lenvTypes=types} =
    env { lenvTypes = Map.insert i a types }

withTyVars :: [(TyVar, Type)] -> Eval a -> Eval a
withTyVars = flip $ foldr (uncurry withTyVar)

getTvSubst :: Eval Subst
getTvSubst = do
  inScope <- getInScope
  tys <- lenvTypes <$> getLocalEnv
  let vars = UniqMap.fromList (Map.toList tys)

  pure (mkTvSubst inScope vars)

findId :: Id -> Eval (Maybe Value)
findId i = Map.lookup i . lenvValues <$> getLocalEnv

withId :: Id -> Value -> Eval a -> Eval a
withId i v x = do
  modifyGlobalEnv goGlobal
  modifyLocalEnv goLocal x
 where
  goGlobal env@GlobalEnv{genvInScope=inScope} =
    -- TODO Change this to use an instance HasFreeVars Value
    let fvs = VarSet.insert i (freeVarsOf (asTerm v))
        iss = InScopeSet.fromVarSet fvs <> inScope
     in env { genvInScope = iss }

  goLocal env@LocalEnv{lenvValues=values} =
    env { lenvValues = Map.insert i v values }

withIds :: [(Id, Value)] -> Eval a -> Eval a
withIds = flip $ foldr (uncurry withId)

withoutId :: Id -> Eval a -> Eval a
withoutId i = modifyLocalEnv go
 where
  go env@LocalEnv{lenvValues=values} =
    env { lenvValues = Map.delete i values }

findBinding :: Id -> Eval (Maybe (Binding Value))
findBinding i = UniqMap.lookup i . genvBindings <$> getGlobalEnv

replaceBinding :: Binding Value -> Eval ()
replaceBinding b = modifyGlobalEnv go
 where
  go env@GlobalEnv{genvBindings=bindings} =
    env { genvBindings = UniqMap.insert (bindingId b) b bindings }

getRef :: Int -> Eval Value
getRef addr = do
  heap <- genvHeap <$> getGlobalEnv

  case IntMap.lookup addr heap of
    Just val -> pure val
    Nothing  -> error ("getHeap: Address " <> show addr <> " out of bounds")

setRef :: Int -> Value -> Eval ()
setRef addr val = modifyGlobalEnv go
 where
  go env@GlobalEnv{genvHeap=heap,genvAddr=next}
    | addr == next =
        env { genvHeap = IntMap.insert addr val heap, genvAddr = addr + 1 }

    | otherwise =
        env { genvHeap = IntMap.insert addr val heap }

isKeepingLifted :: Eval Bool
isKeepingLifted = lenvKeepLifted <$> getLocalEnv

keepLifted :: Eval a -> Eval a
keepLifted = modifyLocalEnv forceLifted
 where
  forceLifted env = env { lenvKeepLifted = True }

getFuel :: Eval Word
getFuel = do
  lenv <- getLocalEnv
  genv <- getGlobalEnv

  pure (min (lenvFuel lenv) (genvFuel genv))

withFuel :: Eval a -> Eval a
withFuel x = modifyGlobalEnv go >> x
 where
  go env@GlobalEnv{genvFuel=fuel} =
    env { genvFuel = fuel - 1 }

preserveFuel :: Eval a -> Eval a
preserveFuel x = do
  fuel <- getFuel
  res  <- x

  modifyGlobalEnv (go fuel)
  pure res
 where
  go fuel env = env { genvFuel = fuel }

getTyConMap :: Eval TyConMap
getTyConMap = genvTyConMap <$> getGlobalEnv

getInScope :: Eval InScopeSet
getInScope = genvInScope <$> getGlobalEnv

getUniqueId :: OccName -> Type -> Eval Id
getUniqueId = getUniqueVar mkUniqSystemId

getUniqueTyVar :: OccName -> Kind -> Eval TyVar
getUniqueTyVar = getUniqueVar mkUniqSystemTyVar

getUniqueVar
  :: ((Supply, InScopeSet)
         -> (OccName, KindOrType)
         -> ((Supply, InScopeSet), Var a))
  -> OccName
  -> KindOrType
  -> Eval (Var a)
getUniqueVar f name ty = do
  env <- getGlobalEnv
  let iss = genvInScope env
      ids = genvSupply env
      ((ids', iss'), i) = f (ids, iss) (name, ty)

  modifyGlobalEnv (go ids' iss')
  pure i
 where
  go ids iss env =
    env { genvInScope = iss, genvSupply = ids }

workFreeValue :: Value -> Eval Bool
workFreeValue = \case
  VNeutral _ -> pure False
  VThunk x _ -> do
    bindings <- fmap (fmap asTerm) . genvBindings <$> getGlobalEnv
    isWorkFree workFreeCache bindings x

  _ -> pure True
