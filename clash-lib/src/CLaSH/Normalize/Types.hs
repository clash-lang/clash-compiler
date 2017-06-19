{-|
  Copyright  :  (C) 2012-2016, University of Twente
  License    :  BSD2 (see the file LICENSE)
  Maintainer :  Christiaan Baaij <christiaan.baaij@gmail.com>

  Types used in Normalize modules
-}

{-# LANGUAGE TemplateHaskell #-}

module CLaSH.Normalize.Types where

import Control.Monad.State.Strict (State)
import Data.HashMap.Strict (HashMap)
import Data.Map            (Map)

import SrcLoc (SrcSpan)

import CLaSH.Core.Term        (Term, TmName)
import CLaSH.Core.Type        (Type)
import CLaSH.Netlist.BlackBox.Types (BlackBoxTemplate)
import CLaSH.Primitives.Types (PrimMap)
import CLaSH.Rewrite.Types    (Rewrite, RewriteMonad)
import CLaSH.Util

-- | State of the 'NormalizeMonad'
data NormalizeState
  = NormalizeState
  { _normalized          :: HashMap TmName (Type,SrcSpan,Term)
  -- ^ Global binders
  , _specialisationCache :: Map (TmName,Int,Either Term Type) (TmName,Type)
  -- ^ Cache of previously specialised functions:
  --
  -- * Key: (name of the original function, argument position, specialised term/type)
  --
  -- * Elem: (name of specialised function,type of specialised function)
  , _specialisationHistory :: HashMap TmName Int
  -- ^ Cache of how many times a function was specialized
  , _specialisationLimit :: !Int
  -- ^ Number of time a function 'f' can be specialized
  , _inlineHistory   :: HashMap TmName (HashMap TmName Int)
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
  , _recursiveComponents :: HashMap TmName Bool
  -- ^ Map telling whether a components is part of a recursive group
  }

makeLenses ''NormalizeState

-- | State monad that stores specialisation and inlining information
type NormalizeMonad = State NormalizeState

-- | RewriteSession with extra Normalisation information
type NormalizeSession = RewriteMonad NormalizeState

-- | A 'Transform' action in the context of the 'RewriteMonad' and 'NormalizeMonad'
type NormRewrite = Rewrite NormalizeState
