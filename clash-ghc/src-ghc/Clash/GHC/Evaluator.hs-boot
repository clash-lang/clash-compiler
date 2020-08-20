module Clash.GHC.Evaluator where

import Clash.Core.Term (Term)
import Clash.Core.Evaluator.Types (Step, Unwind, Machine)
import Clash.Core.Var (Id)

ghcStep :: Step
ghcUnwind :: Unwind

newLetBinding :: Machine -> Term -> (Machine, Id)

