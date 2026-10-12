{-|
  Copyright  :  (C) 2012-2016, University of Twente,
                         2017, Google Inc.
                         2022, QBayLogic B.V.
  License    :  BSD2 (see the file LICENSE)
  Maintainer :  QBayLogic B.V. <devops@qbaylogic.com>

  Types used in Normalize modules
-}

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}

module Clash.Normalize.Types where

import qualified Control.Lens as Lens
import Control.Monad.State.Strict (State)
import Data.HashMap.Strict        (HashMap)
import Data.Map                   (Map)
import Data.Set                   (Set)
import Data.Text                  (Text)

import Clash.Core.Term        (Term)
import Clash.Core.Type        (Type)
import Clash.Core.Var         (Id)
import Clash.Core.VarEnv      (VarEnv)
import Clash.Driver.Types     (Binding, BindingMap)
import Clash.Rewrite.Types    (Rewrite, RewriteMonad)
import Clash.Rewrite.Scoped.Types (ScopedRewrite)
import Clash.Core.Scoped.Alpha (AlphaTerm, AlphaType)
import Clash.Core.Scoped.NameMap (GlobalEnv)
import qualified Clash.Core.Scoped.Syntax as S

-- | State of the 'NormalizeMonad'
data NormalizeState
  = NormalizeState
  { _normalized          :: BindingMap
  -- ^ Global binders
  , _specialisationCache :: Map (Id,Int,Either Term Type) Id
  -- ^ Cache of previously specialized functions:
  --
  -- * Key: (name of the original function, argument position, specialized term/type)
  --
  -- * Elem: (name of specialized function,type of specialized function)
  , _specialisationHistory :: VarEnv Int
  -- ^ Cache of how many times a function was specialized
  , _inlineHistory   :: VarEnv (VarEnv Int)
  -- ^ Cache of function where inlining took place:
  --
  -- * Key: function where inlining took place
  --
  -- * Elem: (functions which were inlined, number of times inlined)
  , _primitiveArgs :: Map Text (Set Int)
  -- ^ Cache for looking up constantness of blackbox arguments
  , _recursiveComponents :: VarEnv Bool
  -- ^ Map telling whether a components is recursively defined.
  --
  -- NB: there are only no mutually-recursive component, only self-recursive
  -- ones.
  , _normalizationStrategy :: Rewrite NormalizeState
  -- ^ The normalization strategy as a rewrite of Clash Core, for
  -- 'Clash.Normalize.Util.normalizeTopLvlBndr'. See
  -- '_scopedNormalizationStrategy'.
  , _constantPropagationStrategy :: Rewrite NormalizeState
  -- ^ The constant propagation strategy as a rewrite of Clash Core, for
  -- "Clash.Normalize.PrimitiveReductions". See
  -- '_scopedNormalizationStrategy'.
  , _scopedNormalizationStrategy :: ScopedRewrite NormalizeState
  -- ^ The compiled 'Clash.Normalize.Strategy.normalization' strategy. Carried
  -- in the state so transformations can normalize other binders
  -- ('Clash.Normalize.Scoped.Util.normalizeTopLvlBndr') without importing the
  -- strategy module. Breaking that import cycle would need an hs-boot file,
  -- and GHC rejects Template Haskell splices in a module cycle.
  , _scopedConstantPropagationStrategy :: ScopedRewrite NormalizeState
  -- ^ The compiled 'Clash.Normalize.Strategy.constantPropagation' strategy,
  -- for "Clash.Normalize.Scoped.PrimitiveReductions". See
  -- '_scopedNormalizationStrategy'.
  , _workFreeAppCache :: HashMap Term Term
  -- ^ Cache of normalized work-free applications, i.e. global functions
  -- applied to closed arguments:
  --
  -- * Key: the application, e.g. @f \@7 dict@
  --
  -- * Elem: the normalized body of @f@ applied to the arguments
  --
  -- See 'Clash.Normalize.Util.normalizeWorkFreeApp'.
  , _scopedNormalized :: GlobalEnv (Binding (S.Term 'S.VoidS))
  -- ^ The normalized global binders. The fields from here on are the
  -- counterparts of the ones above for scoped Clash Core, which the
  -- normalizer uses. The ones above are those of the transformations of Clash
  -- Core, until Phase 7 of the port deletes them.
  , _scopedSpecialisationCache
      :: Map (S.GlobalId, Int, Either AlphaTerm AlphaType) S.GlobalId
  -- ^ See '_specialisationCache'. The specialized-on argument is a closed
  -- term or type, compared up to alpha-equivalence.
  , _scopedSpecialisationHistory :: GlobalEnv Int
  -- ^ See '_specialisationHistory'
  , _scopedInlineHistory :: GlobalEnv (GlobalEnv Int)
  -- ^ See '_inlineHistory'
  , _scopedRecursiveComponents :: GlobalEnv Bool
  -- ^ See '_recursiveComponents'
  , _scopedWorkFreeAppCache
      :: Map (S.GlobalId, [Either AlphaTerm AlphaType]) (S.Term 'S.VoidS)
  -- ^ See '_workFreeAppCache': keyed by the global function and its closed
  -- arguments, compared up to alpha-equivalence
  }

Lens.makeLenses ''NormalizeState

-- | State monad that stores specialisation and inlining information
type NormalizeMonad = State NormalizeState

-- | RewriteSession with extra Normalisation information
type NormalizeSession = RewriteMonad NormalizeState

-- | A 'Transform' action in the context of the 'RewriteMonad' and 'NormalizeMonad'
type NormRewrite = Rewrite NormalizeState

-- | Description of a @Term@ in terms of the type "components" the @Term@ has.
--
-- Is used as a performance/size metric.
data TermClassification
  = TermClassification
  { _function   :: !Int -- ^ Number of functions
  , _primitive  :: !Int -- ^ Number of primitives
  , _selection  :: !Int -- ^ Number of selections/multiplexers
  }
  deriving Show

Lens.makeLenses ''TermClassification
