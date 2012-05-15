module CLaSH.Normalize.Types where

import Control.Monad.State (State)
import Data.HashMap.Lazy (HashMap)
import Data.Map          (Map)

import CLaSH.Core.Term (Term,TmName)
import CLaSH.Core.Type (Type)
import CLaSH.Rewrite.Types (Rewrite,RewriteSession)
import CLaSH.Util

data NormalizeState
  = NormalizeState
  { _normalized          :: HashMap TmName Term
  , _typeSpecializations :: Map (TmName,Int,Type) TmName
  }

mkLabels [''NormalizeState]

type NormalizeMonad = State NormalizeState

type NormalizeSession = RewriteSession NormalizeMonad

type NormRewrite = Rewrite NormalizeMonad
