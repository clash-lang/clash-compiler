{-# LANGUAGE TemplateHaskell #-}
-- | Types used in Normalize modules
module CLaSH.Normalize.Types where

import Control.Monad.State (State)
import Control.Termination (TestResult)
import Data.HashMap.Strict (HashMap)
import Data.Map            (Map)

import CLaSH.Core.Term     (Term, TmName)
import CLaSH.Core.Type     (Type)
import CLaSH.Rewrite.Types (Rewrite, RewriteSession)
import CLaSH.Util

-- | State of the 'NormalizeMonad'
data NormalizeState
  = NormalizeState
  { _normalized      :: HashMap TmName Term -- ^ Global binders
  , _specialisations :: Map (TmName,Int,Either Term Type) (TmName,Type)
  -- ^ Cache of previously specialised functions:
  --
  -- * Key: (name of the original function, argument position, specialised term/type)
  --
  -- * Elem: (name of specialised function,type of specialised function)
  , _specHist        :: HashMap TmName (TestResult Term)
  , _inlined         :: HashMap TmName (HashMap TmName Int)
  -- ^ Cache of function where inlining took place:
  --
  -- * Key: function where inlining took place
  --
  -- * Elem: (functions which were inlined, number of times inlined)
  , _newInlined      :: [TmName]
  -- ^ Inlined functions in the current traversal
  , _inlineLimit     :: Int
  -- ^ Number of times a function 'f' can be inlined in a function 'g'
  , _curFun          :: TmName
  -- ^ Function which is currently normalized
  }

makeLenses ''NormalizeState

-- | State monad that stores specialisation and inlining information
type NormalizeMonad = State NormalizeState

-- | RewriteSession with extra Normalisation information
type NormalizeSession = RewriteSession NormalizeMonad

-- | A 'Transform' action in the context of the 'RewriteMonad' and 'NormalizeMonad'
type NormRewrite = Rewrite NormalizeMonad
