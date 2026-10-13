{-|
  Copyright  :  (C) 2012-2016, University of Twente,
                    2016     , Myrtle Software Ltd,
                    2017     , Google Inc.,
                    2021-2026, QBayLogic B.V.
  License    :  BSD2 (see the file LICENSE)
  Maintainer :  QBayLogic B.V. <devops@qbaylogic.com>

  Type and instance definitions for Rewrite modules
-}

{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module Clash.Rewrite.Types where

import Control.DeepSeq                       (NFData)
import Control.Lens                          (use, (.=))
import qualified Control.Lens as Lens
import Control.Monad.Fix                     (MonadFix (..))
import Control.Monad.IO.Class                (MonadIO (..))
import Control.Monad.State.Strict            (State)
import Control.Monad.Reader                  (MonadReader (..))
import Control.Monad.State                   (MonadState (..))
import Control.Monad.Writer                  (MonadWriter (..))
import Data.Binary                           (Binary)
import Data.HashMap.Strict                   (HashMap)
import Data.IntMap.Strict                    (IntMap)
import Data.Monoid                           (Any)
import Data.Text                             (Text)
import GHC.Exts                              (oneShot)
import GHC.Generics

import Clash.Core.PartialEval as PE          (Evaluator)
import Clash.Core.Evaluator.Types as WHNF    (Evaluator, PrimHeap)

import Clash.Core.Term           (Term, Context)
import Clash.Core.Type           (Type)
import Clash.Core.TyCon          (TyConMap, TyConName)
import Clash.Core.Var            (Id)
import Clash.Core.VarEnv         (InScopeSet, VarSet, VarEnv)
import Clash.Driver.Types
  (ClashEnv(..), ClashOpts(..), BindingMap, DebugOpts, HasClashOpts(..))
import Clash.Driver.Warning      (CanWarn)
import Clash.Netlist.Types       (FilteredHWType, HWMap)
import Clash.Primitives.Types    (CompiledPrimMap)
import Clash.Util
import Clash.Util.Supply         (Supply, freshId)

import Clash.Annotations.BitRepresentation.Internal (CustomReprs)

-- | State used by the inspection mechanism for recording rewrite steps.
data RewriteStep
  = RewriteStep
  { t_ctx    :: Context
  -- ^ current context
  , t_name   :: String
  -- ^ Name of the transformation
  , t_bndrS  :: String
  -- ^ Name of the current binder
  , t_before :: Term
  -- ^ Term before `apply`
  , t_after  :: Term
  -- ^ Term after `apply`
  } deriving (Show, Generic, NFData, Binary)

-- | State of a rewriting session
data RewriteState extra
  = RewriteState
    -- TODO Given we now keep transformCounter, this should just be 'fold'
    -- over that map, otherwise the two counts could fall out of sync.
  { _transformCounter :: {-# UNPACK #-} !Word
  -- ^ Total number of applied transformations
  , _transformAppliedCounters :: HashMap Text Word
  -- ^ Map that tracks how many times each transformation is applied
  , _transformTriedCounters :: HashMap Text Word
  -- ^ Map that tracks how many times each transformation has been tried
  , _bindings         :: !BindingMap
  -- ^ Global binders
  , _uniqSupply       :: !Supply
  -- ^ Supply of unique numbers
  , _curFun           :: (Id,SrcSpan) -- Initially set to undefined: no strictness annotation
  -- ^ Function which is currently normalized
  , _nameCounter      :: {-# UNPACK #-} !Int
  -- ^ Used for 'Fresh'
  , _globalHeap       :: PrimHeap
  -- ^ Used as a heap for compile-time evaluation of primitives that live in I/O
  , _workFreeBinders  :: VarEnv Bool
  -- ^ Map telling whether a binder's definition is work-free
  , _hwTypeCache      :: HWMap
  -- ^ Cache for the Core-type to HWType translation. The translation only
  -- depends on environment that is constant for the whole rewrite session
  -- (the type translator, custom representations, and the TyConMap), so the
  -- cache never has to be invalidated.
  , _extra            :: !extra
  -- ^ Additional state
  }

Lens.makeLenses ''RewriteState

-- | Read-only environment of a rewriting session
data RewriteEnv
  = RewriteEnv
  { _clashEnv       :: ClashEnv
  -- ^ The global environment of the compiler
  , _typeTranslator :: CustomReprs
                    -> TyConMap
                    -> Type
                    -> State HWMap (Maybe (Either String FilteredHWType))
  -- ^ Hardcode Type -> FilteredHWType translator
  , _peEvaluator    :: PE.Evaluator
  -- ^ Hardcoded evaluator for partial evaluation
  , _evaluator      :: WHNF.Evaluator
  -- ^ Hardcoded evaluator for WHNF (old evaluator)
  , _topEntities    :: VarSet
  -- ^ Functions that are considered TopEntities
  }

Lens.makeLenses ''RewriteEnv

clashOpts :: Lens.Getter RewriteEnv ClashOpts
clashOpts = clashEnv . Lens.to envOpts

debugOpts :: Lens.Getter RewriteEnv DebugOpts
debugOpts = clashEnv . Lens.to (opt_debug . envOpts)

aggressiveXOpt :: Lens.Getter RewriteEnv Bool
aggressiveXOpt = clashEnv . Lens.to (opt_aggressiveXOpt . envOpts)

tcCache :: Lens.Getter RewriteEnv TyConMap
tcCache = clashEnv . Lens.to envTyConMap

tupleTcCache :: Lens.Getter RewriteEnv (IntMap TyConName)
tupleTcCache = clashEnv . Lens.to envTupleTyCons

customReprs :: Lens.Getter RewriteEnv CustomReprs
customReprs = clashEnv . Lens.to envCustomReprs

fuelLimit :: Lens.Getter RewriteEnv Word
fuelLimit = clashEnv . Lens.to (opt_evaluatorFuelLimit . envOpts)

primitives :: Lens.Getter RewriteEnv CompiledPrimMap
primitives = clashEnv . Lens.to envPrimitives

inlineLimit :: Lens.Getter RewriteEnv Int
inlineLimit = clashEnv . Lens.to (opt_inlineLimit . envOpts)

inlineFunctionLimit :: Lens.Getter RewriteEnv Word
inlineFunctionLimit = clashEnv . Lens.to (opt_inlineFunctionLimit . envOpts)

inlineConstantLimit :: Lens.Getter RewriteEnv Word
inlineConstantLimit = clashEnv . Lens.to (opt_inlineConstantLimit . envOpts)

inlineWFCacheLimit :: Lens.Getter RewriteEnv Word
inlineWFCacheLimit = clashEnv . Lens.to (opt_inlineWFCacheLimit . envOpts)

newInlineStrategy :: Lens.Getter RewriteEnv Bool
newInlineStrategy = clashEnv . Lens.to (opt_newInlineStrat . envOpts)

specializationLimit :: Lens.Getter RewriteEnv Int
specializationLimit = clashEnv . Lens.to (opt_specLimit . envOpts)

normalizeUltra :: Lens.Getter RewriteEnv Bool
normalizeUltra = clashEnv . Lens.to (opt_ultra . envOpts)

-- | Monad that keeps track how many transformations have been applied and can
-- generate fresh variables and unique identifiers. In addition, it keeps track
-- if a transformation/rewrite has been successfully applied.
--
-- A reader, writer and state monad over 'IO', whose functions are one-shot:
-- see Note [one-shot rewrite monad].
newtype RewriteMonad extra a = MkR
  { unR :: RewriteEnv -> RewriteState extra -> Any -> IO (a, RewriteState extra, Any) }

-- | Make a 'RewriteMonad' action, marking its function one-shot, see Note
-- [one-shot rewrite monad]
pattern R
  :: (RewriteEnv -> RewriteState extra -> Any -> IO (a, RewriteState extra, Any))
  -> RewriteMonad extra a
pattern R m <- MkR m
  where
    R m = MkR (oneShot (\r -> oneShot (\s -> oneShot (\w -> m r s w))))
{-# COMPLETE R #-}

{-
Note [one-shot rewrite monad]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
An action of the rewrite monad is a function of the environment, the state and
the change flag. When GHC sees an action built inside such a function from
values bound outside of it, full laziness floats the building out of the
function, so that it is shared between runs of the function. But an action
almost always runs once: the floating only allocates the action, and the
closures it captures, earlier and unconditionally. In the traversals of
"Clash.Rewrite.Combinators" and "Clash.Rewrite.StrategyDSL.TH", that
happened for every node: GHC built the actions for all constructors before the
node was matched.

'R' marks the functions one-shot ('oneShot'), which tells GHC that they run at
most once, so it leaves the building where it is. When an action does run
more than once, work inside it is redone rather than shared, which is what
the code says anyway. GHC's own monads do the same, see Note [The one-shot
state monad trick] in GHC.Utils.Monad.

The instances are those of
@'Control.Monad.Trans.RWS.CPS.RWST' 'RewriteEnv' 'Any' ('RewriteState' extra) 'IO'@,
which this monad was before.
-}

instance Functor (RewriteMonad extra) where
  fmap f (R m) = R $ \r s w -> do
    (a, s', w') <- m r s w
    pure (f a, s', w')
  {-# INLINE fmap #-}

instance Applicative (RewriteMonad extra) where
  pure a = R $ \_ s w -> pure (a, s, w)
  {-# INLINE pure #-}
  R mf <*> R mx = R $ \r s w -> do
    (f, s', w') <- mf r s w
    (x, s'', w'') <- mx r s' w'
    pure (f x, s'', w'')
  {-# INLINE (<*>) #-}

instance Monad (RewriteMonad extra) where
  R m >>= k = R $ \r s w -> do
    (a, s', w') <- m r s w
    unR (k a) r s' w'
  {-# INLINE (>>=) #-}

instance MonadFix (RewriteMonad extra) where
  mfix f = R $ \r s w -> mfix $ \ ~(a, _, _) -> unR (f a) r s w

instance MonadIO (RewriteMonad extra) where
  liftIO io = R $ \_ s w -> do
    a <- io
    pure (a, s, w)
  {-# INLINE liftIO #-}

instance MonadState (RewriteState extra) (RewriteMonad extra) where
  get = R $ \_ s w -> pure (s, s, w)
  {-# INLINE get #-}
  put s = R $ \_ _ w -> pure ((), s, w)
  {-# INLINE put #-}
  state f = R $ \_ s w -> let (a, s') = f s in pure (a, s', w)
  {-# INLINE state #-}

instance MonadReader RewriteEnv (RewriteMonad extra) where
  ask = R $ \r s w -> pure (r, s, w)
  {-# INLINE ask #-}
  local f (R m) = R $ \r s w -> m (f r) s w
  {-# INLINE local #-}
  reader f = R $ \r s w -> pure (f r, s, w)
  {-# INLINE reader #-}

instance MonadWriter Any (RewriteMonad extra) where
  tell w' = R $ \_ s w -> let wt = w <> w' in wt `seq` pure ((), s, wt)
  {-# INLINE tell #-}
  listen (R m) = R $ \r s w -> do
    (a, s', w') <- m r s mempty
    let wt = w <> w'
    wt `seq` pure ((a, w'), s', wt)
  {-# INLINE listen #-}
  pass (R m) = R $ \r s w -> do
    ((a, f), s', w') <- m r s mempty
    let wt = w <> f w'
    wt `seq` pure (a, s', wt)
  {-# INLINE pass #-}

instance HasClashOpts (RewriteMonad extra) where
  askClashOpts = Lens.view clashOpts

instance CanWarn (RewriteMonad extra)

-- | Run the computation in the RewriteMonad
runR
  :: RewriteMonad extra a
  -> RewriteEnv
  -> RewriteState extra
  -> IO (a, RewriteState extra, Any)
runR m r s = unR m r s mempty

instance MonadUnique (RewriteMonad extra) where
  getUniqueM = do
    sup <- use uniqSupply
    let (a,sup') = freshId sup
    uniqSupply .= sup'
    a `seq` return a

censor :: (Any -> Any) -> RewriteMonad extra a -> RewriteMonad extra a
censor f (R m) = R $ \r s w -> do
  (a, s', w') <- m r s mempty
  let wt = w <> f w'
  wt `seq` pure (a, s', wt)
{-# INLINE censor #-}

data TransformContext
  = TransformContext
  { tfInScope :: !InScopeSet
  , tfContext :: Context
  }

-- | Monadic action that transforms a term given a certain context
type Transform m = TransformContext -> Term -> m Term

-- | A 'Transform' action in the context of the 'RewriteMonad'
type Rewrite extra = Transform (RewriteMonad extra)

-- Moved into Clash.Rewrite.WorkFree

-- TODO:
-- {-# SPECIALIZE isWorkFree
--       :: Lens' (RewriteState extra) (VarEnv Bool)
--       -> BindingMap
--       -> Term
--       -> RewriteMonad extra Bool
--   #-}
