{-|
  Copyright   :  (C) 2026, QBayLogic B.V.
  License     :  BSD2 (see the file LICENSE)
  Maintainer  :  QBayLogic B.V. <devops@qbaylogic.com>

  Tests for "Clash.Rewrite.Scoped.Whnf": evaluating subterms with the
  let-bindings of their scoped context.

  The cases of "whnfRW uses the context's let-bindings (#2571)" in
  "Clash.Tests.Rewrite.Util", on scoped Core: a traversal goes under the
  binders itself, so the context is built by 'Clash.Rewrite.Scoped.pushUnder',
  and evaluates every variable. In Clash Core, a binder that shadows a
  let-binder, or a free variable of its right-hand side, makes the binding
  unusable. In scoped Core, the shadowing binder has another name, so the
  binding is used where it is valid, and nowhere else.
-}

{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Clash.Tests.Rewrite.Scoped.Whnf (tests) where

import Data.Default (def)
import qualified Data.IntMap.Strict as IntMap
import Test.Tasty
import Test.Tasty.HUnit

import Clash.Core.Name (NameSort (..))
import qualified Clash.Core.Term as C
import qualified Clash.Core.Type as CT
import Clash.Normalize.Types (NormalizeState)
import Clash.Rewrite.Types (RewriteEnv (..), RewriteState (..))

import Clash.Core.Scoped.Syntax
import Clash.Rewrite.Scoped (ScopedRewrite, ScopedTransform (..), changed)
import qualified Clash.Rewrite.Scoped as S

import Test.Clash.Rewrite
  ( assertAlphaEq, heapLookupEvaluator, intId, intLit, intTy, localId
  , pairDataCon, tyVar )
import Test.Clash.Scoped (runScopedOpen)

-- | Replace every variable that evaluates to a literal by that literal
evalVars :: ScopedRewrite NormalizeState
evalVars = S.topdownR $ ScopedTransform $ \ctx -> \case
  e@(Var {}) -> S.whnfRW False ctx e $ \_ r -> case r of
    Literal _ -> changed r
    _ -> pure (sink e)
  e -> pure e

-- | Run 'evalVars' on a term, in the scope of its free variables
runEvalVars :: C.Term -> IO C.Term
runEvalVars tm = do
  (res, _, _) <- runScopedOpen env st evalVars tm
  pure res
 where
  env = def{_evaluator = heapLookupEvaluator}
  st :: RewriteState NormalizeState
  st = def{_globalHeap = (IntMap.empty, 0)}

tests :: TestTree
tests = testGroup "Clash.Tests.Rewrite.Scoped.Whnf"
  [ testCase "let x = 3 in \\y -> x" $
      runEvalVars (C.Let (C.NonRec x (intLit 3)) (C.Lam y (C.Var x)))
        >>= assertAlphaEq (C.Let (C.NonRec x (intLit 3)) (C.Lam y (intLit 3)))

    -- The lambda binds another variable than the let
  , testCase "let x = 3 in \\x -> x" $
      let tm = C.Let (C.NonRec x (intLit 3)) (C.Lam x (C.Var x))
      in runEvalVars tm >>= assertAlphaEq tm

    -- The y of the right-hand side is the free one, not the lambda's
  , testCase "let x = y in \\y -> x" $
      let tm = C.Let (C.NonRec x (C.Var y)) (C.Lam y (C.Var x))
      in runEvalVars tm >>= assertAlphaEq tm

  , testCase "let y = 5 in let x = y in \\y -> x" $
      runEvalVars (C.Let (C.NonRec y (intLit 5)) (C.Let (C.NonRec x (C.Var y)) (C.Lam y (C.Var x))))
        >>= assertAlphaEq
              (C.Let (C.NonRec y (intLit 5)) (C.Let (C.NonRec x (intLit 5)) (C.Lam y (intLit 5))))

  , testCase "let y = 5 in let x = y in case s of MkPair y z -> x" $
      runEvalVars
        (C.Let (C.NonRec y (intLit 5)) (C.Let (C.NonRec x (C.Var y))
           (C.Case (C.Var s) intTy [(C.DataPat pairDataCon [] [y, z], C.Var x)])))
        >>= assertAlphaEq
              (C.Let (C.NonRec y (intLit 5)) (C.Let (C.NonRec x (intLit 5))
                 (C.Case (C.Var s) intTy [(C.DataPat pairDataCon [] [y, z], intLit 5)])))

    -- The right-hand side of w refers to the inner x, which is not on the
    -- heap while rewriting that right-hand side; the body sees both
  , testCase "let x = 3 in letrec { x = 4; w = x } in w" $
      runEvalVars
        (C.Let (C.NonRec x (intLit 3)) (C.Letrec [(x, intLit 4), (w, C.Var x)] (C.Var w)))
        >>= assertAlphaEq
              (C.Let (C.NonRec x (intLit 3)) (C.Letrec [(x, intLit 4), (w, C.Var x)] (intLit 4)))

  , testCase "let x = y in let y = 5 in x" $
      let tm = C.Let (C.NonRec x (C.Var y)) (C.Let (C.NonRec y (intLit 5)) (C.Var x))
      in runEvalVars tm >>= assertAlphaEq
           (C.Let (C.NonRec x (C.Var y)) (C.Let (C.NonRec y (intLit 5)) (C.Var x)))

  , testCase "let x = (v :: a) in /\\a -> x" $
      let tm = C.Let (C.NonRec x (C.Var v)) (C.TyLam a (C.Var x))
      in runEvalVars tm >>= assertAlphaEq tm
  ]
 where
  x = intId User "x" 1
  y = intId User "y" 2
  z = intId User "z" 3
  s = intId User "s" 4
  w = intId User "w" 5
  a = tyVar User "a" 6
  v = localId User "v" 7 (CT.VarTy a)
