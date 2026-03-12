{- |
Copyright   : (C) 2020 QBayLogic B.V.
License     : BSD2 (see the file LICENSE)
Maintainer  : QBayLogic B.V. <devops@qbaylogic.com>

The main API of the partial evaluator. This exposes the main functions needed
to call the evaluator, and the type of evaluators. A concrete implementation
of an evaluator is required to use this module: this can be imported from the
library for the compiler front-end, e.g. Clash.GHC.PartialEval in clash-ghc.
-}
module Clash.Core.PartialEval where

import Data.IntMap.Strict (IntMap)

import Clash.Core.PartialEval.AsTerm
import Clash.Core.PartialEval.Monad
import Clash.Core.PartialEval.NormalForm
import Clash.Core.Term (Term)
import Clash.Core.TyCon (TyConMap)
import Clash.Core.Var (Id)
import Clash.Core.VarEnv (InScopeSet)
import Clash.Driver.Types (Binding (..), BindingMap)
import Clash.Util.Supply (Supply)

{- | An evaluator for Clash core. This consists of two functions: one to
evaluate a term to weak-head normal form (WHNF) and another to recursively
evaluate sub-terms to obtain beta-normal eta-long form (NF).
-}
data Evaluator = Evaluator
  { evalWhnf :: Term -> Eval Value
  , quoteNf :: Value -> Eval Normal
  }

{- | Evaluate a term to WHNF, converting the result back to a Term.
The global environment at the end of evaluation is also returned, callers
should preserve any parts of the global environment needed for later calls.
-}
whnf ::
  -- | The evaluator implementation to use.
  Evaluator ->
  -- | The initial global environment.
  GlobalEnv ->
  {- | Whether evaluation should keep lifted data constructors.
  See NOTE [Lifted Constructors] in Clash.Core.PartialEval.NormalForm.
  -}
  Bool ->
  -- | The id of the term under evaluation.
  Id ->
  -- | The term under evaluation.
  Term ->
  -- | The term evalated to WHNF, and the final global environment.
  IO (Term, GlobalEnv)
whnf e g isSubj i x =
  let l = LocalEnv i mempty mempty (genvFuel g) isSubj
   in runEval g l (asTerm <$> evalWhnf e x)

{- | Evaluate a term to NF, converting the result back to a Term.
See `whnf` for more details.
-}
nf ::
  -- | The evaluator implementation to use.
  Evaluator ->
  -- | The initial global environment.
  GlobalEnv ->
  {- | Whether evaluation should keep lifted data constructors.
  See NOTE [Lifted Constructors] in Clash.Core.PartialEval.NormalForm.
  -}
  Bool ->
  -- | The id of the term under evaluation.
  Id ->
  -- | The term under evaluation.
  Term ->
  -- | The term evalated to NF, and the final global environment.
  IO (Term, GlobalEnv)
nf e g isSubj i x =
  let l = LocalEnv i mempty mempty (genvFuel g) isSubj
   in runEval g l (asTerm <$> (evalWhnf e x >>= quoteNf e))

mkGlobalEnv ::
  -- | Global bindings available to the evaluator.
  BindingMap ->
  -- | The type constructors known by Clash.
  TyConMap ->
  -- | The set of variables in scope during evaluation.
  InScopeSet ->
  -- | The supply of fresh names for variables.
  Supply ->
  -- | The initial supply of fuel.
  Word ->
  -- | The initial IO heap.
  IntMap Value ->
  -- | The address of the next heap element.
  Int ->
  GlobalEnv
mkGlobalEnv bm tcm iss ids fuel heap addr =
  GlobalEnv (fmap asThunk bm) tcm iss ids fuel heap addr mempty
 where
  asThunk b@Binding{bindingId = i, bindingTerm = t} =
    b{bindingTerm = VThunk t (LocalEnv i mempty mempty fuel False)}
