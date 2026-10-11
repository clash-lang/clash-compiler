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
import Clash.Core.Subst (aeqType)
import Clash.Core.Term (Bind (..), Pat (..), Term (..), mkApps)
import Clash.Core.Type (Type (VarTy))
import Clash.Core.Var (Id, TyVar)
import Clash.Core.VarEnv (extendInScopeSet)
import Clash.Normalize.Transformations.Case (caseCase, caseCon, caseLet)
import Clash.Normalize.Transformations.Cast (caseCast)
import Clash.Normalize.Types (NormRewrite)
import Clash.Rewrite.StrategyDSL.TH (asRewriteQ)

import Test.Clash.Rewrite
  ( assertAlphaEq, assertStructurallyEqual, inScopeOf, intFunTy, intId, intTy
  , localId, mkDataCon, pairDataCon, parseTyConTy, runSingleTransformation
  , showPprU, tyVar )

caseCaseR, caseCastR, caseConR, caseLetR :: NormRewrite
caseCaseR = $(asRewriteQ caseCase)
caseCastR = $(asRewriteQ caseCast)
caseConR = $(asRewriteQ caseCon)
caseLetR = $(asRewriteQ caseLet)

-- | @T@, a data type with a single constructor 'mkT'
tTy :: Type
tTy = parseTyConTy "T"

-- | @MkT :: Int -> T@
mkT :: DataCon
mkT = mkDataCon "MkT" 1000 [] [] [intTy] tTy

-- | @cast (case s of {MkE \@a' x -> y}) :: a -> Int@, where the existential
-- type variable @a'@ bound by the pattern has the same unique as the free type
-- variable @a@ of the cast (it shadows it). 'caseCast' pushes the cast into
-- the alternative, which must not capture @a@:
--
-- @
-- case s of {MkE \@b x -> cast y :: a -> Int} :: Int
-- @
caseCastExistential :: IO Term
caseCastExistential =
  runSingleTransformation def def is caseCastR $
    Cast
      (Case (Var s) (VarTy a) [(DataPat mkE [a'] [mkEField a'], Var y)])
      (VarTy a) intTy
 where
  is = extendInScopeSet (inScopeOf [s, y]) a
  a = tyVar User "a" 10
  a' = tyVar User "a'" 10
  s = localId User "s" 12 eTy
  y = localId User "y" 13 (VarTy a)

-- | The expected result of 'caseCastExistential'
caseCastExistentialExpected :: Term
caseCastExistentialExpected =
  Case (Var s) intTy
    [(DataPat mkE [b] [mkEField b], Cast (Var y) (VarTy a) intTy)]
 where
  a = tyVar User "a" 10
  b = tyVar User "b" 14
  s = localId User "s" 12 eTy
  y = localId User "y" 13 (VarTy a)

-- | @E@, a data type with a single constructor 'mkE'
eTy :: Type
eTy = parseTyConTy "E"

-- | @MkE :: forall b. b -> E@, a data constructor with an existential type
-- variable
mkE :: DataCon
mkE = mkDataCon "MkE" 1001 [] [b] [VarTy b] eTy
 where
  b = tyVar User "b" 1002

-- | The field of 'mkE', of the given type
mkEField :: TyVar -> Id
mkEField a = localId User "x" 11 (VarTy a)

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

-- | @case (let x = u in x) of {_ -> x}@, with @x@ free in the alternative.
-- 'caseLet' must not move the alternative under the let-binder @x@.
caseLetCapture :: IO Term
caseLetCapture =
  runSingleTransformation def def (inScopeOf [u, x]) caseLetR $
    Case (Let (NonRec x (Var u)) (Var x)) intTy [(DefaultPat, Var x)]
 where
  u = intId User "u" 1
  x = intId User "x" 2

-- | The expected result of 'caseLetCapture'
caseLetCaptureExpected :: Term
caseLetCaptureExpected =
  Let (NonRec x1 (Var u)) (Case (Var x1) intTy [(DefaultPat, Var x)])
 where
  u = intId User "u" 1
  x = intId User "x" 2
  x1 = intId User "x" 3

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

    -- 'caseCast' pushed a cast into the alternatives of a case-expression
    -- without renaming the type variables bound by their patterns, and kept
    -- the type of the alternatives as it was before the cast
    , testCase "caseCast deshadows existentials and sets the type" $ do
        result <- caseCastExistential
        assertAlphaEq caseCastExistentialExpected result
        case result of
          Case _ ty _ -> assertBool ("type: " <> showPprU ty) (ty `aeqType` intTy)
          _ -> pure ()

    -- 'caseLet' moved the let-bindings out of the subject of a case-expression
    -- without renaming them. They could then capture free variables of the
    -- alternatives. https://github.com/clash-lang/clash-compiler/pull/1071
    , testCase "caseLet deshadows let-bindings (#1071)" $
        caseLetCapture >>= assertAlphaEq caseLetCaptureExpected
    ]
