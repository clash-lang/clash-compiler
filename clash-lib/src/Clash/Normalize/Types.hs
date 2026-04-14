{-|
  Copyright  :  (C) 2012-2016, University of Twente,
                         2017, Google Inc.
                         2022, QBayLogic B.V.
  License    :  BSD2 (see the file LICENSE)
  Maintainer :  QBayLogic B.V. <devops@qbaylogic.com>

  Types used in Normalize modules
-}

{-# LANGUAGE TemplateHaskell #-}

module Clash.Normalize.Types where

import Control.Concurrent.MVar    (MVar)
import qualified Control.Lens as Lens
import Control.Monad.State.Strict (StateT)
import Data.Map                   (Map)
import Data.Set                   (Set)
import Data.Text                  (Text)

import Clash.Core.Term        (Term)
import Clash.Core.Type        (Type)
import Clash.Core.Var         (Id)
import Clash.Core.VarEnv      (VarEnv)
import Clash.Driver.Types     (Binding)
import Clash.Rewrite.Types    (Rewrite, RewriteMonad)

-- | State of the 'NormalizeMonad'
data NormalizeState
  = NormalizeState
  { _normalized          :: MVar (VarEnv (MVar (Binding Term)))
  -- ^ Global binders
  , _specialisationCache :: MVar (Map (Id,Int,Either Term Type) Id)
  -- ^ Cache of previously specialized functions:
  --
  -- * Key: (name of the original function, argument position, specialized term/type)
  --
  -- * Elem: (name of specialized function,type of specialized function)
  , _specialisationHistory :: MVar (VarEnv Int)
  -- ^ Cache of how many times a function was specialized
  , _inlineHistory   :: MVar (VarEnv (VarEnv Int))
  -- ^ Cache of function where inlining took place:
  --
  -- * Key: function where inlining took place
  --
  -- * Elem: (functions which were inlined, number of times inlined)
  , _primitiveArgs :: MVar (Map Text (Set Int))
  -- ^ Cache for looking up constantness of blackbox arguments
  , _recursiveComponents :: MVar (VarEnv Bool)
  -- ^ Map telling whether a components is recursively defined.
  --
  -- NB: there are only no mutually-recursive component, only self-recursive
  -- ones.
  }

Lens.makeLenses ''NormalizeState

-- | State monad that stores specialisation and inlining information
type NormalizeMonad = StateT NormalizeState IO

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
