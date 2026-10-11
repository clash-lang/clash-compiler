{-|
  Copyright   :  (C) 2026, QBayLogic B.V.
  License     :  BSD2 (see the file LICENSE)
  Maintainer  :  QBayLogic B.V. <devops@qbaylogic.com>

  Tests for "Clash.Rewrite.Combinators"
-}

module Clash.Tests.Rewrite.Combinators (tests) where

import Control.Monad.Trans.Writer.Strict (Writer, runWriter, tell)
import Data.Functor.Identity (runIdentity)

import Test.Tasty
import Test.Tasty.HUnit

import Clash.Core.Name (NameSort (User))
import Clash.Core.Term (Bind (..), Term (..))
import Clash.Core.VarEnv (elemInScopeSet, emptyInScopeSet)
import Clash.Rewrite.Combinators (allR)
import Clash.Rewrite.Types (TransformContext (..))

import Test.Clash.Rewrite (assertAlphaEq, globalIntVar, intId, intVar)

-- | @\\x -> let x = g x in x@, where the right-hand side refers to the lambda's
-- @x@: the let's binder is not in scope of its own right-hand side.
shadowingNonRec :: Term
shadowingNonRec =
  Lam x (Let (NonRec x (App (globalIntVar User "g" 1) (Var x))) (Var x))
 where
  x = intId User "x" 0

-- | 'allR' must rebuild a 'NonRec' let as a 'NonRec' let. It used to rebuild
-- it as a 'Rec' let, turning @let x = g x@ (with the outer @x@ on the
-- right-hand side) into the loop @letrec x = g x@.
allRKeepsNonRec :: Assertion
allRKeepsNonRec =
  assertAlphaEq shadowingNonRec
    (runIdentity (allR (allR (\_ -> pure)) emptyContext shadowingNonRec))
 where
  emptyContext = TransformContext emptyInScopeSet []

-- | 'allR' must bring the binder of a 'NonRec' let into scope of the body,
-- but not of the right-hand side.
allRNonRecScope :: Assertion
allRNonRecScope =
  [("rhs", False), ("body", True)] @=? snd (runWriter (allR record ctx term))
 where
  x = intId User "x" 0
  term = Let (NonRec x (intVar User "y" 1)) (Var x)
  ctx = TransformContext emptyInScopeSet []

  record :: TransformContext -> Term -> Writer [(String, Bool)] Term
  record (TransformContext is _) e = do
    tell [(if e == Var x then "body" else "rhs", x `elemInScopeSet` is)]
    pure e

tests :: TestTree
tests = testGroup "Clash.Tests.Rewrite.Combinators"
  [ testCase "allR keeps a NonRec let non-recursive" allRKeepsNonRec
  , testCase "allR scopes a NonRec binder over the body only" allRNonRecScope
  ]
