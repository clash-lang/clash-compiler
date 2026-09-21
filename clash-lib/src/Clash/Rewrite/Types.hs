{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

{-|
  Copyright  :  (C) 2012-2016, University of Twente,
                    2016     , Myrtle Software Ltd,
                    2017     , Google Inc.,
                    2021-2026, QBayLogic B.V.
  License    :  BSD2 (see the file LICENSE)
  Maintainer :  QBayLogic B.V. <devops@qbaylogic.com>

  Type and instance definitions for Rewrite modules
-}
module Clash.Rewrite.Types where

import Clash.Annotations.BitRepresentation.Internal (CustomReprs)
import Clash.Core.Evaluator.Types as WHNF (Evaluator, PrimHeap)
import Clash.Core.PartialEval as PE (Evaluator)
import Clash.Core.Term (Context, Term)
import Clash.Core.TyCon (TyConMap, TyConName)
import Clash.Core.Type (Type)
import Clash.Core.Var (Id)
import Clash.Core.VarEnv (InScopeSet, VarEnv, VarSet)
import Clash.Driver.Types
  ( BindingMap,
    ClashEnv (..),
    ClashOpts (..),
    DebugOpts,
    HasClashOpts (..),
  )
import Clash.Driver.Warning (CanWarn)
import Clash.Netlist.Types (FilteredHWType, HWMap)
import Clash.Primitives.Types (CompiledPrimMap)
import Clash.Util
import Clash.Util.Supply (Supply, freshId)
import Control.DeepSeq (NFData)
import Control.Lens (use, (.=))
import qualified Control.Lens as Lens
import Control.Monad.Fix (MonadFix)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Reader (MonadReader (..))
import Control.Monad.State (MonadState (..))
import Control.Monad.State.Strict (State)
import Control.Monad.Trans.RWS.CPS (RWST)
import qualified Control.Monad.Trans.RWS.CPS as RWS
import Control.Monad.Writer (MonadWriter (..))
import Data.Binary (Binary)
import Data.HashMap.Strict (HashMap)
import Data.IntMap.Strict (IntMap)
import Data.Monoid (Any)
import Data.Text (Text)
import GHC.Generics

-- | State used by the inspection mechanism for recording rewrite steps.
data RewriteStep
  = RewriteStep
  { -- | current context
    t_ctx :: Context,
    -- | Name of the transformation
    t_name :: String,
    -- | Name of the current binder
    t_bndrS :: String,
    -- | Term before `apply`
    t_before :: Term,
    -- | Term after `apply`
    t_after :: Term
  }
  deriving (Show, Generic, NFData, Binary)

-- | State of a rewriting session
data RewriteState extra
  = RewriteState
  -- TODO Given we now keep transformCounter, this should just be 'fold'
  -- over that map, otherwise the two counts could fall out of sync.
  { -- | Total number of applied transformations
    _transformCounter :: {-# UNPACK #-} !Word,
    -- | Map that tracks how many times each transformation is applied
    _transformAppliedCounters :: HashMap Text Word,
    -- | Map that tracks how many times each transformation has been tried
    _transformTriedCounters :: HashMap Text Word,
    -- | Global binders
    _bindings :: !BindingMap,
    -- | Supply of unique numbers
    _uniqSupply :: !Supply,
    -- | Function which is currently normalized
    _curFun :: (Id, SrcSpan), -- Initially set to undefined: no strictness annotation
    -- | Used for 'Fresh'
    _nameCounter :: {-# UNPACK #-} !Int,
    -- | Used as a heap for compile-time evaluation of primitives that live in I/O
    _globalHeap :: PrimHeap,
    -- | Map telling whether a binder's definition is work-free
    _workFreeBinders :: VarEnv Bool,
    -- | Cache for the Core-type to HWType translation. The translation only
    -- depends on environment that is constant for the whole rewrite session
    -- (the type translator, custom representations, and the TyConMap), so the
    -- cache never has to be invalidated.
    _hwTypeCache :: HWMap,
    -- | Additional state
    _extra :: !extra
  }

Lens.makeLenses ''RewriteState

-- | Read-only environment of a rewriting session
data RewriteEnv
  = RewriteEnv
  { -- | The global environment of the compiler
    _clashEnv :: ClashEnv,
    -- | Hardcode Type -> FilteredHWType translator
    _typeTranslator ::
      CustomReprs ->
      TyConMap ->
      Type ->
      State HWMap (Maybe (Either String FilteredHWType)),
    -- | Hardcoded evaluator for partial evaluation
    _peEvaluator :: PE.Evaluator,
    -- | Hardcoded evaluator for WHNF (old evaluator)
    _evaluator :: WHNF.Evaluator,
    -- | Functions that are considered TopEntities
    _topEntities :: VarSet
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
newtype RewriteMonad extra a = R
  {unR :: RWST RewriteEnv Any (RewriteState extra) IO a}
  deriving newtype
    ( Applicative,
      Functor,
      Monad,
      MonadFix,
      MonadIO,
      MonadState (RewriteState extra),
      MonadWriter Any,
      MonadReader RewriteEnv
    )

instance HasClashOpts (RewriteMonad extra) where
  askClashOpts = Lens.view clashOpts

instance CanWarn (RewriteMonad extra)

-- | Run the computation in the RewriteMonad
runR ::
  RewriteMonad extra a ->
  RewriteEnv ->
  RewriteState extra ->
  IO (a, RewriteState extra, Any)
runR m = RWS.runRWST (unR m)

instance MonadUnique (RewriteMonad extra) where
  getUniqueM = do
    sup <- use uniqSupply
    let (a, sup') = freshId sup
    uniqSupply .= sup'
    a `seq` return a

censor :: (Any -> Any) -> RewriteMonad extra a -> RewriteMonad extra a
censor f = R . RWS.censor f . unR
{-# INLINE censor #-}

data TransformContext
  = TransformContext
  { tfInScope :: !InScopeSet,
    tfContext :: Context
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
