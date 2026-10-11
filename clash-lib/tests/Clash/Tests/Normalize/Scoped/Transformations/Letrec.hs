{-|
  Copyright   :  (C) 2026, QBayLogic B.V.
  License     :  BSD2 (see the file LICENSE)
  Maintainer  :  QBayLogic B.V. <devops@qbaylogic.com>

  Tests for "Clash.Normalize.Scoped.Transformations.Letrec"
-}

{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}

module Clash.Tests.Normalize.Scoped.Transformations.Letrec (tests) where

import Data.Default (def)
import Test.Tasty
import Test.Tasty.HUnit

import Clash.Core.Name (NameSort (User))
import qualified Clash.Core.Term as C
import Clash.Core.Var (Id)
import Clash.Core.VarEnv (emptyInScopeSet)
import Clash.Normalize.Scoped.Transformations.Letrec (deadCode)
import Clash.Normalize.Types (NormRewrite, NormalizeState)
import Clash.Rewrite.Combinators (bottomupR)
import Clash.Rewrite.StrategyDSL.TH (asRewriteQ, asScopedRewriteQ)
import Clash.Rewrite.Types (RewriteState, TransformContext (..))
import Clash.Rewrite.Util (apply)

import Test.Clash.Rewrite
  (assertAlphaEq, globalIntVar, intId, intLit, runRewriteTest)
import Test.Clash.Scoped (runViaFoil)

-- | Run 'deadCode' on a closed term, at its root. Gives the result and
-- whether a change was signaled.
deadCodeAt :: C.Term -> IO (C.Term, Bool)
deadCodeAt e = do
  (t, _, ch) <- runViaFoil def st (TransformContext emptyInScopeSet [])
                  $(asScopedRewriteQ deadCode) e
  pure (t, ch)
 where
  st :: RewriteState NormalizeState
  st = def

-- | 'deadCode' in a bottom-up traversal of Clash Core, as the normalizer runs
-- it: through 'Clash.Rewrite.Scoped.ViaFoil.viaFoil'
deadCodeBottomup :: C.Term -> IO C.Term
deadCodeBottomup e = do
  (t, _, _) <- runRewriteTest def st (rw (TransformContext emptyInScopeSet []) e)
  pure t
 where
  rw :: NormRewrite
  rw = bottomupR (apply "deadCode" $(asRewriteQ deadCode))
  st :: RewriteState NormalizeState
  st = def

a, b, c, x :: Id
a = intId User "a" 1
b = intId User "b" 2
c = intId User "c" 3
x = intId User "x" 4

f :: C.Term
f = globalIntVar User "f" 5

tests :: TestTree
tests = testGroup "Clash.Tests.Normalize.Scoped.Transformations.Letrec"
  [ testGroup "deadCode"
      [ testCase "removes an unused non-recursive binding" $ do
          (t, ch) <- deadCodeAt (C.Let (C.NonRec x (intLit 1)) (intLit 2))
          assertAlphaEq (intLit 2) t
          ch @?= True
      , testCase "keeps a used non-recursive binding" $ do
          let e = C.Let (C.NonRec x (intLit 1)) (C.Var x)
          (t, ch) <- deadCodeAt e
          show t @?= show e
          ch @?= False
      , testCase "removes a recursive let-binding without used bindings" $ do
          (t, ch) <- deadCodeAt (C.Letrec [(a, C.Var b), (b, C.Var a)] (intLit 2))
          assertAlphaEq (intLit 2) t
          ch @?= True
      , testCase "keeps the bindings the used ones refer to, in order" $ do
          (t, ch) <- deadCodeAt
            (C.Letrec [(a, C.App f (C.Var b)), (c, intLit 3), (b, intLit 1)] (C.Var a))
          assertAlphaEq (C.Letrec [(a, C.App f (C.Var b)), (b, intLit 1)] (C.Var a)) t
          ch @?= True
      , testCase "keeps a recursive let-binding whose bindings are all used" $ do
          let e = C.Letrec [(a, C.App f (C.Var b)), (b, C.Var a)] (C.Var a)
          (t, ch) <- deadCodeAt e
          show t @?= show e
          ch @?= False
      , testCase "a NonRec binder doesn't keep the binding it shadows alive" $ do
          -- let x = 1 in let x = x in x: the outer binding is used by the
          -- right-hand side of the inner one
          let e = C.Let (C.NonRec x (intLit 1)) (C.Let (C.NonRec x (C.Var x)) (C.Var x))
          deadCodeBottomup e >>= assertAlphaEq e
      , testCase "in a bottom-up traversal of Clash Core" $
          deadCodeBottomup
            (C.Lam x (C.Let (C.NonRec a (C.Var x))
              (C.Letrec [(b, intLit 1), (c, C.Var a)] (C.App f (C.Var x)))))
            >>= assertAlphaEq (C.Lam x (C.App f (C.Var x)))
      ]
  ]
