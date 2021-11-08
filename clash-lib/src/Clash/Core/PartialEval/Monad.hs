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
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskellQuotes #-}

module Clash.Core.PartialEval.Monad
  ( -- * Partial Evaluation Monad
    Eval
  , runEval
    -- * Partial Evaluation Exception
  , EvalException(..)
    -- * Local and Global Environments
  , getLocalEnv
  , setLocalEnv
  , modifyLocalEnv
  , getGlobalEnv
  , modifyGlobalEnv
    -- * Evaluation Context
  , getTarget
  , withTarget
    -- * Local Type Bindings
  , findTyVar
  , withTyVar
  , withTyVars
  , normTy
  , normVarTy
    -- * Local Term Bindings
  , findId
  , withId
  , withIds
    -- * Global Term Bindings
  , findBinding
  , replaceBinding
    -- * IO Heap Bindings
  , getRef
  , setRef
    -- * Fuel
  , getFuel
  , withFuel
    -- * Accessing Global State
  , getTyConMap
  , getInScope
  , withInScope
  , withInScopeList
  , getAddr
    -- * Fresh Variable Generation
  , getUniqueId
  , getUniqueTyVar
    -- * Primitives info
  , primUsedArguments
  ) where

import           Control.Applicative (Alternative)
import           Control.Concurrent.Supply (Supply)
import           Control.Monad.Catch
import           Control.Monad.IO.Class (MonadIO)

#if !MIN_VERSION_base(4,13,0)
import           Control.Monad.Fail (MonadFail)
#endif

import           Control.Monad.RWS.Strict (RWST, MonadReader, MonadState)
import qualified Control.Monad.RWS.Strict as RWS
import           Data.Bitraversable (bitraverse)
import           Data.Either (rights)
import qualified Data.HashMap.Lazy as HashMap (lookup)
import qualified Data.IntMap.Strict as IntMap
import           Data.List ((\\))
import qualified Data.Map.Strict as Map
import           Data.Text (Text)
import qualified Data.Text.Extra as Text (showt)

import           Clash.Annotations.BitRepresentation.Deriving (dontApplyInHDL)
import qualified Clash.Sized.Vector as Vec (splitAt)

import           Clash.Annotations.Primitive (extractPrim)
import           Clash.Core.HasFreeVars
import           Clash.Core.Name (OccName)
import           Clash.Core.PartialEval.AsTerm
import           Clash.Core.PartialEval.NormalForm
import           Clash.Core.Subst
import           Clash.Core.Term (Pat, PrimInfo(..))
import           Clash.Core.TyCon (TyConMap)
import           Clash.Core.Type (Kind, KindOrType, Type, normalizeType, splitFunForallTy)
import           Clash.Core.Util (mkUniqSystemId, mkUniqSystemTyVar)
import           Clash.Core.Var (Id, TyVar, Var(varType))
import           Clash.Core.VarEnv
import           Clash.Driver.Types (Binding(..))
import           Clash.Netlist.BlackBox.Util (getUsedArguments)
import           Clash.Primitives.Types (Primitive(..), UsedArguments(..))

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

-- | Exceptions specific to partial evaluation. Note that other exceptions
-- may still be thrown, such as ArithException or IOException.
--
data EvalException
  = ResultUndefined
    -- ^ The result of an evaluation is undefined. This can be used as an early
    -- exit for some primitive definitions.
  | CannotApply Value (Arg Value)
    -- ^ An attempt to apply an argument to an incompatible value was made,
    -- for instance applying to a non-function value.
  | CannotMatch Value [Pat]
    -- ^ An attempt to match the given value on the following patterns did
    -- not succeed. This likely means the supplied patterns were non-exhaustive.
  | CannotConvert (Maybe Value)
    -- ^ An attempt to convert a value between a Haskell value and an AST did
    -- not succeed. This likely means a primitive failed to evaluate from not
    -- all relevant arguments being statically known.
  | UnexpectedArgs PrimInfo (Args Value)
    -- ^ Arguments which are being examined are not in an expected form. This
    -- is used in primitive rules to debug bad primitive definitions.
  | NoHeapBinding Int
    -- ^ An attempt to read from the IO heap failed as there was no binding.
    -- This is indicative of an internal error in the evaluator.
  deriving (Show)

instance Exception EvalException

getLocalEnv :: Eval LocalEnv
getLocalEnv = RWS.ask
{-# INLINE getLocalEnv #-}

setLocalEnv :: LocalEnv -> Eval a -> Eval a
setLocalEnv new =
  let mergeInScope = unionInScope (lenvInScope new) . lenvInScope
      minFuel      = min (lenvFuel new) . lenvFuel
   in RWS.local (\env -> new { lenvInScope = mergeInScope env, lenvFuel = minFuel env })
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

getTarget :: Eval Id
getTarget = lenvTarget <$> getLocalEnv

withTarget :: Id -> Eval a -> Eval a
withTarget i = modifyLocalEnv go
 where
  go env = env { lenvTarget = i }

findTyVar :: TyVar -> Eval (Maybe Type)
findTyVar i = Map.lookup i . lenvTypes <$> getLocalEnv

withTyVar :: TyVar -> Type -> Eval a -> Eval a
withTyVar i ty = withTyVars [(i, ty)]

withTyVars :: [(TyVar, Type)] -> Eval a -> Eval a
withTyVars tys action = do
  normTys <- traverse (bitraverse pure normTy) tys
  modifyLocalEnv (goLocal normTys) action
 where
  goLocal xs env@LocalEnv{lenvTypes=types,lenvInScope=inScope} =
    let fvs = mkVarSet (fst <$> xs) `unionVarSet` freeVarsOf (snd <$> xs)
        iss = mkInScopeSet fvs `unionInScope` inScope
        env' = substEnvTys xs env
     in env' { lenvTypes = Map.fromList xs <> types , lenvInScope = iss }

-- | Substitute all bound types in the environment with the list of bindings.
-- This must be used after normTy to ensure that the substitution does not
-- introduce new free variables into a type.
--
substEnvTys :: [(TyVar, Type)] -> LocalEnv -> LocalEnv
substEnvTys tys env =
  env { lenvTypes = fmap go (lenvTypes env) }
 where
  substFvs = freeVarsOf (snd <$> tys)
  substVars = mkVarSet (fst <$> tys)

  go ty =
    let domFvs = freeVarsOf ty
        inScope = unionVarSet (differenceVarSet domFvs substVars) substFvs
        subst = mkTvSubst (mkInScopeSet inScope) (mkVarEnv tys)
     in substTy subst ty

-- | Normalize a binding of type to tyvar using the existing environment. This
-- is needed to ensure that new bindings in the environment do not contain
-- variable references to bindings already in the environment, as these tyvars
-- may become free when substituted into the result.
--
-- TODO Only do if type contains tyvars, otherwise it's a waste of time making
-- the substitution from the environment.
--
normTy :: Type -> Eval Type
normTy ty = do
  tcm <- getTyConMap
  tys <- Map.toList . lenvTypes <$> getLocalEnv

  let substFvs = freeVarsOf (snd <$> tys)
      substVars = mkVarSet (fst <$> tys)
      domFvs = freeVarsOf ty
      inScope = unionVarSet (differenceVarSet domFvs substVars) substFvs
      subst = mkTvSubst (mkInScopeSet inScope) (mkVarEnv tys)

  pure (normalizeType tcm (substTy subst ty))

normVarTy :: Var a -> Eval (Var a)
normVarTy var = do
  ty <- normTy (varType var)
  pure (var { varType = ty })

findId :: Id -> Eval (Maybe Value)
findId i = Map.lookup i . lenvValues <$> getLocalEnv

withId :: Id -> Value -> Eval a -> Eval a
withId i v = withIds [(i, v)]

withIds :: [(Id, Value)] -> Eval a -> Eval a
withIds ids = modifyLocalEnv goLocal
 where
  goLocal env@LocalEnv{lenvValues=values,lenvInScope=inScope} =
    -- TODO Change this to use an instance HasFreeVars Value
    let fvs = mkVarSet (fst <$> ids) `unionVarSet` freeVarsOf (asTerm . snd <$> ids)
        iss = inScope `unionInScope` mkInScopeSet fvs
     in env { lenvValues = Map.fromList ids <> values, lenvInScope = iss }

findBinding :: Id -> Eval (Maybe (Binding Value))
findBinding i = lookupVarEnv i . genvBindings <$> getGlobalEnv

replaceBinding :: Binding Value -> Eval ()
replaceBinding b = modifyGlobalEnv go
 where
  go env@GlobalEnv{genvBindings=bindings} =
    env { genvBindings = extendVarEnv (bindingId b) b bindings }

getRef :: Int -> Eval Value
getRef addr = do
  heap <- genvHeap <$> getGlobalEnv

  case IntMap.lookup addr heap of
    Just val -> pure val
    Nothing  -> throwM (NoHeapBinding addr)

setRef :: Int -> Value -> Eval ()
setRef addr val = modifyGlobalEnv go
 where
  go env@GlobalEnv{genvHeap=heap,genvAddr=next}
    | addr == next =
        env { genvHeap = IntMap.insert addr val heap, genvAddr = addr + 1 }

    | otherwise =
        env { genvHeap = IntMap.insert addr val heap }

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

getTyConMap :: Eval TyConMap
getTyConMap = genvTyConMap <$> getGlobalEnv

getInScope :: Eval InScopeSet
getInScope = lenvInScope <$> getLocalEnv

withInScope :: Var a -> Eval b -> Eval b
withInScope var = withInScopeList [var]

withInScopeList :: [Var a] -> Eval b -> Eval b
withInScopeList vars = modifyLocalEnv go
 where
  go env@LocalEnv{lenvInScope=inScope} =
    env { lenvInScope = extendInScopeSetList inScope vars }

getAddr :: Eval Int
getAddr = genvAddr <$> getGlobalEnv

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
  iss <- getInScope
  ids <- genvSupply <$> getGlobalEnv
  let ((ids', _), i) = f (ids, iss) (name, ty)

  modifyGlobalEnv (go ids')
  pure i
 where
  go ids env =
    env { genvSupply = ids }

-- Stolen from removeUnusedExpr in Clash.Normalize.Transformations.Letrec
primUsedArguments :: PrimInfo -> Eval [Int]
primUsedArguments pr = do
  primMap <- RWS.asks lenvPrimitives

  case HashMap.lookup (primName pr) primMap >>= extractPrim of
    Just BlackBoxHaskell{usedArguments} ->
      case usedArguments of
        UsedArguments used ->
          pure used

        IgnoredArguments ignored ->
          pure ([0 .. length args - 1] \\ ignored)

    Just (BlackBox pNm _ _ _ _ _ _ _ _ _ inc r ri templ)
      | isFromInt pNm -> pure [0..2]
      | primName pr `elem` [Text.showt 'dontApplyInHDL, Text.showt 'Vec.splitAt] -> pure [0,1]
      | otherwise -> pure $ concat
          [ concatMap getUsedArguments r
          , concatMap getUsedArguments ri
          , getUsedArguments templ
          , concatMap (getUsedArguments . snd) inc
          ]

    _ ->
      -- Assume all arguments are used if we don't know any better.
      pure [0..]
 where
  args = rights . fst $ splitFunForallTy (primType pr)

-- Stolen from Clash.Rewrite.Util to prevent import loop
isFromInt :: Text -> Bool
isFromInt nm = nm == "Clash.Sized.Internal.BitVector.fromInteger##" ||
               nm == "Clash.Sized.Internal.BitVector.fromInteger#" ||
               nm == "Clash.Sized.Internal.Index.fromInteger#" ||
               nm == "Clash.Sized.Internal.Signed.fromInteger#" ||
               nm == "Clash.Sized.Internal.Unsigned.fromInteger#"
