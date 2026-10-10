{-|
  Copyright   :  (C) 2026, QBayLogic B.V.
  License     :  BSD2 (see the file LICENSE)
  Maintainer  :  QBayLogic B.V. <devops@qbaylogic.com>

  Tests for "Clash.Normalize.Transformations.Case"
-}

{-# LANGUAGE TemplateHaskell #-}

module Clash.Tests.Normalize.Transformations.Case (tests) where

import Data.Default (def)

import Test.Tasty
import Test.Tasty.HUnit

import Clash.Core.Name (NameSort (User))
import Clash.Core.Term (Pat (..), Term (..), mkApps)
import Clash.Core.Var (Id)
import Clash.Normalize.Transformations.Case (caseCon)
import Clash.Normalize.Types (NormRewrite)
import Clash.Rewrite.StrategyDSL.TH (asRewriteQ)

import Test.Clash.Rewrite
  ( assertStructurallyEqual, inScopeOf, intId, intTy, pairDataCon
  , runSingleTransformation )

caseConR :: NormRewrite
caseConR = $(asRewriteQ caseCon)

-- | @case MkPair c a of MkPair a' b -> b@, where the pattern binder @a'@ has
-- the same unique as the free variable @a@ in the subject (it shadows it), but
-- a different name. 'caseCon' should reduce this to exactly the free variable
-- @a@.
--
-- 'caseCon' used to first substitute the fields for the pattern binders, and
-- then apply the substitution for the existential type variables to the result
-- using an in-scope set extended with the pattern binders. That second
-- substitution replaced the free variable @a@, coming from the subject, by the
-- pattern binder @a'@ that shadowed it.
--
-- https://github.com/clash-lang/clash-compiler/pull/1040
-- https://github.com/clash-lang/clash-compiler/commit/e087036b76b65900dad4bbbff8bd291f42250a64
caseConShadowedFreeVar :: IO Term
caseConShadowedFreeVar =
  runSingleTransformation def def (inScopeOf [c, aOuter]) caseConR term
 where
  c = intId User "c" 3
  b = intId User "b" 2
  aPat = intId User "pat" 1
  term =
    Case (mkApps (Data pairDataCon) [Left (Var c), Left (Var aOuter)]) intTy
      [(DataPat pairDataCon [] [aPat, b], Var b)]

-- | See 'caseConShadowedFreeVar'
aOuter :: Id
aOuter = intId User "a" 1

tests :: TestTree
tests =
  testGroup
    "Clash.Tests.Normalize.Transformations.Case"
    [ testCase "caseCon keeps free variables shadowed by the pattern (#1040)" $
        caseConShadowedFreeVar >>= assertStructurallyEqual (Var aOuter)
    ]
