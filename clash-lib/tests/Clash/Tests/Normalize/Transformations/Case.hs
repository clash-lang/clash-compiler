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

import Clash.Core.DataCon (DataCon)
import Clash.Core.Name (NameSort (User))
import Clash.Core.Term (Pat (..), Term (..), mkApps)
import Clash.Core.Type (Type)
import Clash.Core.Var (Id)
import Clash.Normalize.Transformations.Case (caseCase, caseCon)
import Clash.Normalize.Types (NormRewrite)
import Clash.Rewrite.StrategyDSL.TH (asRewriteQ)

import Test.Clash.Rewrite
  ( assertAlphaEq, assertStructurallyEqual, inScopeOf, intFunTy, intId, intTy
  , localId, mkDataCon, pairDataCon, parseTyConTy, runSingleTransformation )

caseCaseR, caseConR :: NormRewrite
caseCaseR = $(asRewriteQ caseCase)
caseConR = $(asRewriteQ caseCon)

-- | @T@, a data type with a single constructor 'mkT'
tTy :: Type
tTy = parseTyConTy "T"

-- | @MkT :: Int -> T@
mkT :: DataCon
mkT = mkDataCon "MkT" 1000 [] [] [intTy] tTy

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

-- | @case (case a of {MkT x -> f}) of {_ -> x}@, with @x@ free in the outer
-- alternative. 'caseCase' must not move the outer alternative under the
-- binder @x@ of the inner one.
caseCaseCapture :: IO Term
caseCaseCapture =
  runSingleTransformation def def (inScopeOf [a, f, x]) caseCaseR $
    Case
      (Case (Var a) (intFunTy 1) [(DataPat mkT [] [x], Var f)])
      intTy
      [(DefaultPat, Var x)]
 where
  a = localId User "a" 1 tTy
  f = localId User "f" 2 (intFunTy 1)
  x = intId User "x" 3

-- | The expected result of 'caseCaseCapture'
caseCaseCaptureExpected :: Term
caseCaseCaptureExpected =
  Case (Var a) intTy
    [(DataPat mkT [] [x1], Case (Var f) intTy [(DefaultPat, Var x)])]
 where
  a = localId User "a" 1 tTy
  f = localId User "f" 2 (intFunTy 1)
  x = intId User "x" 3
  x1 = intId User "x" 4

tests :: TestTree
tests =
  testGroup
    "Clash.Tests.Normalize.Transformations.Case"
    [ testCase "caseCon keeps free variables shadowed by the pattern (#1040)" $
        caseConShadowedFreeVar >>= assertStructurallyEqual (Var aOuter)

    -- 'caseCase' pushed the outer case-expression into the alternatives of
    -- the inner one, without renaming the binders of the inner alternatives.
    -- Those could then capture free variables of the outer alternatives.
    -- https://github.com/clash-lang/clash-compiler/pull/1067
    , testCase "caseCase deshadows alternatives (#1067)" $
        caseCaseCapture >>= assertAlphaEq caseCaseCaptureExpected
    ]
