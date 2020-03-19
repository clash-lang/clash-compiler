module Clash.GHC.Evaluator where

import Clash.Core.Term (Term)
import Clash.Core.TyCon (TyConMap)
import Clash.Core.Evaluator.Types (Step, Unwind, Machine)
import Clash.Core.Var (Id)

ghcStep :: Step
ghcUnwind :: Unwind

newLetBinding ::  TyConMap -> Machine -> Term -> (Machine, Id)

