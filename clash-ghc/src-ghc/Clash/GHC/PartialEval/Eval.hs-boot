module Clash.GHC.PartialEval.Eval
  ( eval
  , forceEval
  , apply
  , applyTy
  ) where

import GHC.Stack (HasCallStack)

import Clash.Core.PartialEval.Monad (Eval)
import Clash.Core.PartialEval.NormalForm (Value)
import Clash.Core.Term (Term)
import Clash.Core.Type (Type)

eval :: (HasCallStack) => Term -> Eval Value
forceEval :: (HasCallStack) => Value -> Eval Value
apply :: (HasCallStack) => Value -> Value -> Eval Value
applyTy :: (HasCallStack) => Value -> Type -> Eval Value
