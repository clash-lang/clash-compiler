{-|
  Copyright  :  (C) 2012-2016, University of Twente,
                         2017, Google Inc.
  License    :  BSD2 (see the file LICENSE)
  Maintainer :  Christiaan Baaij <christiaan.baaij@gmail.com>

  Types used in Normalize modules
-}

{-# LANGUAGE TemplateHaskell #-}

module CLaSH.Normalize.Types where

import Control.Monad.State.Strict (State)
import Data.HashMap.Strict (HashMap)
import Data.Map            (Map)

import CLaSH.Core.Term        (Term, TmName, TmOccName)
import CLaSH.Core.Type        (Type)
import CLaSH.Driver.Types     (BindingMap)
import CLaSH.Netlist.BlackBox.Types (BlackBoxTemplate)
import CLaSH.Primitives.Types (PrimMap)
import CLaSH.Rewrite.Types    (Rewrite, RewriteMonad)
import CLaSH.Util

-- | State of the 'NormalizeMonad'
data NormalizeState
  = NormalizeState
  { _normalized          :: BindingMap
  -- ^ Global binders
  , _specialisationCache :: Map (TmOccName,Int,Either Term Type) (TmName,Type)
  -- ^ Cache of previously specialised functions:
  --
  -- * Key: (name of the original function, argument position, specialised term/type)
  --
  -- * Elem: (name of specialised function,type of specialised function)
  , _specialisationHistory :: HashMap TmOccName Int
  -- ^ Cache of how many times a function was specialized
  , _specialisationLimit :: !Int
  -- ^ Number of time a function 'f' can be specialized
  , _inlineHistory   :: HashMap TmOccName (HashMap TmOccName Int)
  -- ^ Cache of function where inlining took place:
  --
  -- * Key: function where inlining took place
  --
  -- * Elem: (functions which were inlined, number of times inlined)
  , _inlineLimit     :: !Int
  -- ^ Number of times a function 'f' can be inlined in a function 'g'
  , _inlineBelow     :: !Int
  -- ^ Size of a function below which it is always inlined if it is not
  -- recursive
  , _primitives :: PrimMap BlackBoxTemplate -- ^ Primitive Definitions
  , _recursiveComponents :: HashMap TmOccName Bool
  -- ^ Map telling whether a components is recursively defined.
  --
  -- NB: there are only no mutually-recursive component, only self-recursive
  -- ones.
  }

makeLenses ''NormalizeState

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

makeLenses ''TermClassification
