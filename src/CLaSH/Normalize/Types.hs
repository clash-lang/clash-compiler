module CLaSH.Normalize.Types where

import Control.Monad.State (State)
import Data.HashMap.Lazy (HashMap)
import Data.Map          (Map)

import CLaSH.Core.Term (Term,TmName)
import CLaSH.Core.Type (Type)
import CLaSH.Rewrite.Types (Rewrite)
import CLaSH.Util

data NormalizeState
  = RewriteState
  { _normalized          :: HashMap TmName Term
  , _typeSpecializations :: Map (TmName,Type) TmName
  }

mkLabels [''NormalizeState]

type NormalizeMonad = State NormalizeState

type NormRewrite = Rewrite NormalizeMonad
