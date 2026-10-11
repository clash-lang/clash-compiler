{-|
  Copyright   :  (C) 2026, QBayLogic B.V.
  License     :  BSD2 (see the file LICENSE)
  Maintainer  :  QBayLogic B.V. <devops@qbaylogic.com>

  Tests for "Clash.Normalize.Transformations.Cast"
-}

{-# LANGUAGE TemplateHaskell #-}

module Clash.Tests.Normalize.Transformations.Cast (tests) where

import Data.Default (def)

import Test.Tasty
import Test.Tasty.HUnit

import Clash.Core.Name (NameSort (User))
import Clash.Core.Term (Bind (..), Term (..))
import Clash.Normalize.Transformations.Cast (splitCastWork)
import Clash.Normalize.Types (NormRewrite)
import Clash.Rewrite.StrategyDSL.TH (asRewriteQ)

import Test.Clash.Rewrite
  ( assertAlphaEq, globalId, inScopeOf, intFunTy, intId, intTy
  , runSingleTransformation )

splitCastWorkR :: NormRewrite
splitCastWorkR = $(asRewriteQ splitCastWork)

-- | @let x = cast (g x) in x@, with the outer @x@ on the right-hand side.
-- 'splitCastWork' must keep the binding non-recursive:
--
-- @
-- let x' = g x in let x = cast x' in x
-- @
--
-- It used to rebuild it as @letrec x' = g x; x = cast x' in x@, where @g x@
-- refers to the cast itself.
splitCastWorkNonRec :: Assertion
splitCastWorkNonRec = do
  result <- runSingleTransformation def def (inScopeOf [x]) splitCastWorkR term
  assertAlphaEq expected result
 where
  x = intId User "x" 0
  x' = intId User "x'" 1
  g = Var (globalId User "g" 2 (intFunTy 1))
  term = Let (NonRec x (Cast (App g (Var x)) intTy intTy)) (Var x)
  expected =
    Let (NonRec x' (App g (Var x)))
      (Let (NonRec x (Cast (Var x') intTy intTy)) (Var x))

tests :: TestTree
tests = testGroup "Clash.Tests.Normalize.Transformations.Cast"
  [ testCase "splitCastWork keeps a NonRec let non-recursive"
      splitCastWorkNonRec
  ]
