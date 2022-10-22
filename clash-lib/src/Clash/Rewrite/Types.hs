{-|
  Copyright  :  (C) 2012-2016, University of Twente,
                    2016     , Myrtle Software Ltd,
                    2017     , Google Inc.,
                    2021-2022, QBayLogic B.V.
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
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}

module Clash.Rewrite.Types where

import Control.Concurrent.Supply             (Supply, freshId)
import Control.DeepSeq                       (NFData)
import Control.Lens                          (Lens', use, (.=))
import qualified Control.Lens as Lens
import Control.Monad.Fix                     (MonadFix)
import Control.Monad.State.Strict            (State)
#if MIN_VERSION_transformers(0,5,6)
import Control.Monad.Reader                  (MonadReader (..))
import Control.Monad.State                   (MonadState (..))
import Control.Monad.Trans.RWS.CPS           (RWST)
import qualified Control.Monad.Trans.RWS.CPS as RWS
import Control.Monad.Writer                  (MonadWriter (..))
#else
import Control.Monad.Trans.RWS.Strict        (RWST)
import qualified Control.Monad.Trans.RWS.Strict as RWS
#endif
import Data.Binary                           (Binary)
import Data.HashMap.Strict                   (HashMap)
import Data.IntMap.Strict                    (IntMap)
import Data.Monoid                           (Any)
import Data.Text                             (Text)
import GHC.Generics

#if MIN_VERSION_ghc(9,0,0)
import GHC.Types.SrcLoc               (SrcSpan)
#else
import SrcLoc                         (SrcSpan)
#endif

import Clash.Core.PartialEval as PE          (Evaluator)
import Clash.Core.Evaluator.Types as WHNF    (Evaluator, PrimHeap)

import Clash.Core.Binding        (BindingMap)
import Clash.Core.InScopeSet     (InScopeSet)
import Clash.Core.Term           (Term, Context)
import Clash.Core.Type           (Type)
import Clash.Core.TyCon          (TyConMap, TyConName)
import Clash.Core.Var            (Id, VarEnv)
import Clash.Core.VarSet         (VarSet)
import Clash.Driver.Types        (ClashEnv(..), ClashOpts(..), DebugOpts)
import Clash.Netlist.Types       (FilteredHWType, HWMap)
import Clash.Primitives.Types    (CompiledPrimMap)
import Clash.Rewrite.WorkFree    (isWorkFree)
import Clash.Unique              (MonadUnique(..))

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
    -- TODO Given we now keep transformCounters, this should just be 'fold'
    -- over that map, otherwise the two counts could fall out of sync.
  { _transformCounter :: {-# UNPACK #-} !Word
  -- ^ Total number of applied transformations
  , _transformCounters :: HashMap Text Word
  -- ^ Map that tracks how many times each transformation is applied
  , _bindings         :: !(BindingMap Term)
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
  { unR :: RWST RewriteEnv Any (RewriteState extra) IO a }
  deriving newtype
    ( Applicative
    , Functor
    , Monad
    , MonadFix
    )

-- | Run the computation in the RewriteMonad
runR
  :: RewriteMonad extra a
  -> RewriteEnv
  -> RewriteState extra
  -> IO (a, RewriteState extra, Any)
runR m = RWS.runRWST (unR m)

#if MIN_VERSION_transformers(0,5,6) && !MIN_VERSION_mtl(2,3,0)
-- For Control.Monad.Trans.RWS.Strict these are already defined, however the
-- CPS version of RWS is not included in `mtl` yet.

instance MonadState (RewriteState extra) (RewriteMonad extra) where
  get = R RWS.get
  {-# INLINE get #-}
  put = R . RWS.put
  {-# INLINE put #-}
  state = R . RWS.state
  {-# INLINE state #-}

instance MonadWriter Any (RewriteMonad extra) where
  writer = R . RWS.writer
  {-# INLINE writer #-}
  tell = R . RWS.tell
  {-# INLINE tell #-}
  listen = R . RWS.listen . unR
  {-# INLINE listen #-}
  pass = R . RWS.pass . unR
  {-# INLINE pass #-}

instance MonadReader RewriteEnv (RewriteMonad extra) where
   ask = R RWS.ask
   {-# INLINE ask #-}
   local f = R . RWS.local f . unR
   {-# INLINE local #-}
   reader = R . RWS.reader
   {-# INLINE reader #-}
#endif

instance MonadUnique (RewriteMonad extra) where
  getUniqueM = do
    sup <- use uniqSupply
    let (a,sup') = freshId sup
    uniqSupply .= sup'
    a `seq` return a

censor :: (Any -> Any) -> RewriteMonad extra a -> RewriteMonad extra a
censor f = R . RWS.censor f . unR
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
{-# SPECIALIZE isWorkFree
      :: Lens' (RewriteState extra) (VarEnv Bool)
      -> BindingMap Term
      -> Term
      -> RewriteMonad extra Bool
  #-}
