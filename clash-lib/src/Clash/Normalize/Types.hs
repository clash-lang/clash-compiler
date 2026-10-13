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
import Data.Map                   (Map)
import Data.Set                   (Set)
import Data.Text                  (Text)

import Clash.Driver.Types (Binding)
import Clash.Rewrite.Types (RewriteMonad, Rewrite)
import Clash.Core.Scoped.Alpha (AlphaTerm, AlphaType)
import Clash.Core.Scoped.NameMap (GlobalEnv)
import qualified Clash.Core.Scoped.Syntax as S

-- | State of the 'NormalizeMonad'
data NormalizeState
  = NormalizeState
  { _primitiveArgs :: Map Text (Set Int)
  -- ^ Cache for looking up constantness of blackbox arguments
  , _normalizationStrategy :: Rewrite NormalizeState
  -- ^ The compiled 'Clash.Normalize.Strategy.normalization' strategy. Carried
  -- in the state so transformations can normalize other binders
  -- ('Clash.Normalize.Scoped.Util.normalizeTopLvlBndr') without importing the
  -- strategy module. Breaking that import cycle would need an hs-boot file,
  -- and GHC rejects Template Haskell splices in a module cycle.
  , _constantPropagationStrategy :: Rewrite NormalizeState
  -- ^ The compiled 'Clash.Normalize.Strategy.constantPropagation' strategy,
  -- for "Clash.Normalize.Scoped.PrimitiveReductions". See
  -- '_normalizationStrategy'.
  , _normalized :: GlobalEnv (Binding (S.Term 'S.VoidS))
  -- ^ The normalized global binders
  , _specialisationCache
      :: Map (S.GlobalId, Int, Either AlphaTerm AlphaType) S.GlobalId
  -- ^ Cache of previously specialized functions:
  --
  -- * Key: (the original function, argument position, specialized term/type).
  --   The specialized-on argument is a closed term or type, compared up to
  --   alpha-equivalence.
  --
  -- * Elem: the specialized function
  , _specialisationHistory :: GlobalEnv Int
  -- ^ Cache of how many times a function was specialized
  , _inlineHistory :: GlobalEnv (GlobalEnv Int)
  -- ^ Cache of function where inlining took place:
  --
  -- * Key: function where inlining took place
  --
  -- * Elem: (functions which were inlined, number of times inlined)
  , _recursiveComponents :: GlobalEnv Bool
  -- ^ Map telling whether a components is recursively defined.
  --
  -- NB: there are only no mutually-recursive component, only self-recursive
  -- ones.
  , _workFreeAppCache
      :: Map (S.GlobalId, [Either AlphaTerm AlphaType]) (S.Term 'S.VoidS)
  -- ^ Cache of normalized work-free applications, i.e. global functions
  -- applied to closed arguments:
  --
  -- * Key: the global function and its arguments, e.g. @f \@7 dict@,
  --   compared up to alpha-equivalence
  --
  -- * Elem: the normalized body of @f@ applied to the arguments
  --
  -- See 'Clash.Normalize.Scoped.Util.normalizeWorkFreeApp'.
  }

Lens.makeLenses ''NormalizeState

-- | State monad that stores specialisation and inlining information
type NormalizeMonad = State NormalizeState

-- | RewriteSession with extra Normalisation information
type NormalizeSession = RewriteMonad NormalizeState

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
