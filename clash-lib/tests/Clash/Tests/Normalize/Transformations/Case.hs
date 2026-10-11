{-|
  Copyright   :  (C) 2026, QBayLogic B.V.
  License     :  BSD2 (see the file LICENSE)
  Maintainer  :  QBayLogic B.V. <devops@qbaylogic.com>

  Tests for "Clash.Normalize.Transformations.Case". The tests of the
  transformations of that module are parameterised over their implementation
  ('caseTests'), so that they also run on their ports to scoped Clash Core,
  see "Clash.Tests.Normalize.Scoped.Transformations.Case".
-}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Clash.Tests.Normalize.Transformations.Case
  ( tests
  , CaseRewrites (..)
  , caseTests
  ) where

import Data.Default (def)

import Test.Tasty
import Test.Tasty.HUnit

import Clash.Core.DataCon (DataCon (dcTag))
import Clash.Core.Literal (Literal (IntLiteral, IntegerLiteral))
import Clash.Core.Name (NameSort (User), noSrcSpan)
import Clash.Core.Subst (aeqType)
import Clash.Core.Term
  ( Bind (..), Pat (..), PrimInfo (..), PrimUnfolding (..), Term (..)
  , TickInfo (..), IsMultiPrim (..), WorkInfo (..), mkApps )
import Clash.Core.Type (Type (VarTy), mkFunTy)
import Clash.Core.TysPrim (intPrimTy, integerIsDc, integerPrimTy)
import Clash.Core.Var (Id, TyVar)
import Clash.Core.VarEnv (extendInScopeSet)
import Clash.Normalize.Transformations.Case
  ( caseCaseWorker, caseConWorker, caseFlatWorker, caseLetWorker
  , elimCaseBigNumInternalsWorker )
import Clash.Normalize.Transformations.Cast (caseCast)
import qualified Clash.Normalize.Primitives as NP (undefined)
import Clash.Normalize.Types (NormRewrite)
import Clash.Rewrite.StrategyDSL (onCase, toTransformation)
import Clash.Rewrite.StrategyDSL.TH (asRewriteQ)

import Test.Clash.Rewrite
  ( assertAlphaEq, assertStructurallyEqual, ghcBoolTy, ghcFalseDataCon
  , ghcTrueDataCon, inScopeOf, intFunTy, intId, intTy, localId, mkDataCon
  , pairDataCon, parseTyConTy, runSingleTransformation, showPprU, tyVar )

-- | The transformations of "Clash.Normalize.Transformations.Case" that
-- 'caseTests' tests, as rewrites of Clash Core
data CaseRewrites = CaseRewrites
  { crCaseCase :: NormRewrite
  , crCaseCon :: NormRewrite
  , crCaseFlat :: NormRewrite
  , crCaseLet :: NormRewrite
  , crElimCaseBigNum :: NormRewrite
  }

-- | The transformations of Clash Core. They are made from their workers: the
-- transformations of "Clash.Normalize.Transformations.Case" are the ports to
-- scoped Clash Core once the normalizer runs those.
currentRewrites :: CaseRewrites
currentRewrites = CaseRewrites
  { crCaseCase = $(asRewriteQ (toTransformation "caseCase" (onCase 'caseCaseWorker)))
  , crCaseCon = $(asRewriteQ (toTransformation "caseCon" (onCase 'caseConWorker)))
  , crCaseFlat = $(asRewriteQ (toTransformation "caseFlat" (onCase 'caseFlatWorker)))
  , crCaseLet = $(asRewriteQ (toTransformation "caseLet" (onCase 'caseLetWorker)))
  , crElimCaseBigNum =
      $(asRewriteQ (toTransformation "elimCaseBigNum" (onCase 'elimCaseBigNumInternalsWorker)))
  }

caseCastR :: NormRewrite
caseCastR = $(asRewriteQ caseCast)

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
caseConShadowedFreeVar :: NormRewrite -> IO Term
caseConShadowedFreeVar caseConR =
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
caseCaseCapture :: NormRewrite -> IO Term
caseCaseCapture caseCaseR =
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
caseLetCapture :: NormRewrite -> IO Term
caseLetCapture caseLetR =
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

-- | @case MkU w of {MkT y -> z; MkU z' -> z'}@, where the pattern binder @z'@
-- has the same unique as the free variable @z@ (it shadows it), but a different
-- name. The alternatives are alpha equivalent, but only the first returns @z@.
-- 'caseCon' should reduce this to @w@.
caseConOneAltShadowed :: NormRewrite -> IO Term
caseConOneAltShadowed caseConR =
  runSingleTransformation def def (inScopeOf [w, z]) caseConR $
    Case (App (Data mkU) (Var w)) intTy
      [ (DataPat mkT [] [intId User "y" 6], Var z)
      , (DataPat mkU [] [z'], Var z') ]
 where
  w = intId User "w" 7
  z = intId User "z" 5
  z' = intId User "z'" 5

-- | @MkU :: Int -> T@, the second constructor of @T@
mkU :: DataCon
mkU = (mkDataCon "MkU" 1003 [] [] [intTy] tTy) { dcTag = 2 }

-- | A primitive @work :: Int -> Int@ that is not work free
workPrim :: Term
workPrim = Prim (PrimInfo "work" (intFunTy 1) WorkAlways SingleResult NoUnfolding)

-- | @case MkPair (work c) d of MkPair x y -> f x y@: 'caseCon' let-binds the
-- field that is not work free, and substitutes the other one
caseConLetBindsWork :: NormRewrite -> IO Term
caseConLetBindsWork caseConR =
  runSingleTransformation def def (inScopeOf [c, d, f]) caseConR $
    Case (mkApps (Data pairDataCon) [Left (App workPrim (Var c)), Left (Var d)]) intTy
      [(DataPat pairDataCon [] [x, y], mkApps (Var f) [Left (Var x), Left (Var y)])]
 where
  c = intId User "c" 1
  d = intId User "d" 2
  f = localId User "f" 3 (intFunTy 2)
  x = intId User "x" 4
  y = intId User "y" 5

-- | The expected result of 'caseConLetBindsWork'
caseConLetBindsWorkExpected :: Term
caseConLetBindsWorkExpected =
  Let (NonRec x1 (App workPrim (Var c))) (mkApps (Var f) [Left (Var x1), Left (Var d)])
 where
  c = intId User "c" 1
  d = intId User "d" 2
  f = localId User "f" 3 (intFunTy 2)
  x1 = intId User "x" 6

-- | @case MkPair (work c) (work d) of MkPair x y -> y@: 'caseCon' drops the
-- unused field, even though it is not work free
caseConDropsUnused :: NormRewrite -> IO Term
caseConDropsUnused caseConR =
  runSingleTransformation def def (inScopeOf [c, d]) caseConR $
    Case (mkApps (Data pairDataCon) [Left (App workPrim (Var c)), Left (App workPrim (Var d))]) intTy
      [(DataPat pairDataCon [] [x, y], Var y)]
 where
  c = intId User "c" 1
  d = intId User "d" 2
  x = intId User "x" 4
  y = intId User "y" 5

-- | The expected result of 'caseConDropsUnused'
caseConDropsUnusedExpected :: Term
caseConDropsUnusedExpected = Let (NonRec y1 (App workPrim (Var d))) (Var y1)
 where
  d = intId User "d" 2
  y1 = intId User "y" 6

-- | @case MkU w of {MkT y -> y}@: no alternative matches, so the
-- case-expression is undefined
caseConNoMatch :: NormRewrite -> IO Term
caseConNoMatch caseConR =
  runSingleTransformation def def (inScopeOf [w]) caseConR $
    Case (App (Data mkU) (Var w)) intTy [(DataPat mkT [] [y], Var y)]
 where
  w = intId User "w" 7
  y = intId User "y" 6

-- | @case 5 of {_ -> a; IS x -> x}@, for a literal of type @Integer@: 'caseCon'
-- matches the constructor of small integers, and binds its field to an
-- @Int#@ literal
caseConIntegerLiteral :: NormRewrite -> IO Term
caseConIntegerLiteral caseConR =
  runSingleTransformation def def (inScopeOf [a]) caseConR $
    Case (Literal (IntegerLiteral 5)) integerPrimTy
      [(DefaultPat, Var a), (DataPat integerIsDc [] [x], Var x)]
 where
  a = localId User "a" 1 intPrimTy
  x = localId User "x" 2 intPrimTy

-- | The expected result of 'caseConIntegerLiteral'
caseConIntegerLiteralExpected :: Term
caseConIntegerLiteralExpected = Let (NonRec x (Literal (IntLiteral 5))) (Var x)
 where
  x = localId User "x" 2 intPrimTy

-- | @case (tick (letrec x = f x in x)) of {_ -> x}@, with @x@ free in the
-- alternative: 'caseLet' moves the tick into the right-hand side and the body,
-- and renames the binder
caseLetRecTicks :: NormRewrite -> IO Term
caseLetRecTicks caseLetR =
  runSingleTransformation def def (inScopeOf [f, x]) caseLetR $
    Case (Tick tick (Let (Rec [(x, App (Var f) (Var x))]) (Var x))) intTy [(DefaultPat, Var x)]
 where
  f = localId User "f" 1 (intFunTy 1)
  x = intId User "x" 2

-- | The expected result of 'caseLetRecTicks'
caseLetRecTicksExpected :: Term
caseLetRecTicksExpected =
  Let (Rec [(x1, Tick tick (App (Var f) (Var x1)))])
    (Case (Tick tick (Var x1)) intTy [(DefaultPat, Var x)])
 where
  f = localId User "f" 1 (intFunTy 1)
  x = intId User "x" 2
  x1 = intId User "x" 3

-- | The tick of 'caseLetRecTicks'
tick :: TickInfo
tick = SrcSpan noSrcSpan

-- | @I# :: Int# -> Int@
intDataCon :: DataCon
intDataCon = mkDataCon "GHC.Types.I#" 1010 [] [] [intPrimTy] intTy

-- | @GHC.Classes.eqInt :: Int -> Int -> Bool@
eqIntPrim :: Term
eqIntPrim =
  Prim (PrimInfo "GHC.Classes.eqInt" (mkFunTy intTy (mkFunTy intTy ghcBoolTy))
                 WorkVariable SingleResult NoUnfolding)

-- | @eqInt x (I# n)@
eqIntLit :: Id -> Integer -> Term
eqIntLit x n = mkApps eqIntPrim [Left (Var x), Left (App (Data intDataCon) (Literal (IntLiteral n)))]

-- | A chain of comparisons, as GHC makes for a case-expression on an @Int@:
--
-- @
-- case eqInt x (I# 3) of
--   False -> case eqInt x (I# 2) of
--     False -> d
--     True -> b
--   True -> a
-- @
caseFlatEqInt :: NormRewrite -> IO Term
caseFlatEqInt caseFlatR =
  runSingleTransformation def def (inScopeOf [x, a, b, d]) caseFlatR $
    Case (eqIntLit x 3) intTy
      [ (DataPat ghcFalseDataCon [] [],
          Case (eqIntLit x 2) intTy
            [ (DataPat ghcFalseDataCon [] [], Var d)
            , (DataPat ghcTrueDataCon [] [], Var b) ])
      , (DataPat ghcTrueDataCon [] [], Var a) ]
 where
  x = intId User "x" 1
  a = intId User "a" 2
  b = intId User "b" 3
  d = intId User "d" 4

-- | The expected result of 'caseFlatEqInt': a case-expression on the @Int#@
-- of @x@
caseFlatEqIntExpected :: Term
caseFlatEqIntExpected =
  Case (Var x) intTy
    [ (DataPat intDataCon [] [wild],
        Case (Var wild) intTy
          [ (DefaultPat, Var d)
          , (LitPat (IntLiteral 3), Var a)
          , (LitPat (IntLiteral 2), Var b) ]) ]
 where
  x = intId User "x" 1
  a = intId User "a" 2
  b = intId User "b" 3
  d = intId User "d" 4
  wild = localId User "wild" 5 intPrimTy

-- | @case i of {IS x -> a; IP y -> b}@: 'elimCaseBigNumInternals' keeps the
-- alternative of small integers, which doesn't use its field
elimCaseBigNumUnused :: NormRewrite -> IO Term
elimCaseBigNumUnused elimR =
  runSingleTransformation def def (inScopeOf [i, a, b]) elimR $
    Case (Var i) intTy
      [ (DataPat integerIsDc [] [localId User "x" 4 intPrimTy], Var a)
      , (DataPat integerIpDc [] [localId User "y" 5 intPrimTy], Var b) ]
 where
  i = localId User "i" 1 integerPrimTy
  a = intId User "a" 2
  b = intId User "b" 3

-- | @case i of {IS x -> f x; IP y -> b}@: 'elimCaseBigNumInternals' keeps the
-- alternative of small integers, which uses its field, as a projection
elimCaseBigNumUsed :: NormRewrite -> IO Term
elimCaseBigNumUsed elimR =
  runSingleTransformation def def (inScopeOf [i, f, b]) elimR $
    Case (Var i) intTy
      [ (DataPat integerIsDc [] [x], App (Var f) (Var x))
      , (DataPat integerIpDc [] [localId User "y" 5 intPrimTy], Var b) ]
 where
  i = localId User "i" 1 integerPrimTy
  f = localId User "f" 2 (mkFunTy intPrimTy intTy)
  b = intId User "b" 3
  x = localId User "x" 4 intPrimTy

-- | The expected result of 'elimCaseBigNumUsed'
elimCaseBigNumUsedExpected :: Term
elimCaseBigNumUsedExpected =
  Case (Var i) intTy [(DataPat integerIsDc [] [x], App (Var f) (Var x))]
 where
  i = localId User "i" 1 integerPrimTy
  f = localId User "f" 2 (mkFunTy intPrimTy intTy)
  x = localId User "x" 4 intPrimTy

-- | A second constructor of @Integer@, standing in for @IP@
integerIpDc :: DataCon
integerIpDc = (mkDataCon "IP" 1011 [] [] [intPrimTy] integerPrimTy) { dcTag = 2 }

tests :: TestTree
tests =
  testGroup
    "Clash.Tests.Normalize.Transformations.Case"
    ( caseTests currentRewrites <>
      -- 'caseCast' pushed a cast into the alternatives of a case-expression
      -- without renaming the type variables bound by their patterns, and kept
      -- the type of the alternatives as it was before the cast
      [ testCase "caseCast deshadows existentials and sets the type" $ do
          result <- caseCastExistential
          assertAlphaEq caseCastExistentialExpected result
          case result of
            Case _ ty _ -> assertBool ("type: " <> showPprU ty) (ty `aeqType` intTy)
            _ -> pure ()
      ] )

-- | The tests of the transformations of
-- "Clash.Normalize.Transformations.Case", for the given implementations
caseTests :: CaseRewrites -> [TestTree]
caseTests rws =
  [ testCase "caseCon keeps free variables shadowed by the pattern (#1040)" $
      caseConShadowedFreeVar (crCaseCon rws) >>= assertStructurallyEqual (Var aOuter)

  -- 'caseCase' pushed the outer case-expression into the alternatives of
  -- the inner one, without renaming the binders of the inner alternatives.
  -- Those could then capture free variables of the outer alternatives.
  -- https://github.com/clash-lang/clash-compiler/pull/1067
  , testCase "caseCase deshadows alternatives (#1067)" $
      caseCaseCapture (crCaseCase rws) >>= assertAlphaEq caseCaseCaptureExpected

  -- 'caseLet' moved the let-bindings out of the subject of a case-expression
  -- without renaming them. They could then capture free variables of the
  -- alternatives. https://github.com/clash-lang/clash-compiler/pull/1071
  , testCase "caseLet deshadows let-bindings (#1071)" $
      caseLetCapture (crCaseLet rws) >>= assertAlphaEq caseLetCaptureExpected

  -- 'caseOneAlt' replaced a case-expression whose alternatives are all
  -- equal by its first alternative, only checking whether the variables
  -- bound by the first pattern occur in it
  , testCase "caseOneAlt checks the pattern of every alternative" $
      caseConOneAltShadowed (crCaseCon rws) >>= assertAlphaEq (Var (intId User "w" 7))

  , testCase "caseCon let-binds fields that are not work free" $
      caseConLetBindsWork (crCaseCon rws) >>= assertAlphaEq caseConLetBindsWorkExpected
  , testCase "caseCon drops unused fields" $
      caseConDropsUnused (crCaseCon rws) >>= assertAlphaEq caseConDropsUnusedExpected
  , testCase "caseCon gives undefined if no alternative matches" $
      caseConNoMatch (crCaseCon rws) >>= assertAlphaEq (TyApp (Prim NP.undefined) intTy)
  , testCase "caseCon matches the constructors of an Integer literal" $
      caseConIntegerLiteral (crCaseCon rws) >>= assertAlphaEq caseConIntegerLiteralExpected
  , testCase "caseLet moves ticks into recursive let-bindings" $
      caseLetRecTicks (crCaseLet rws) >>= assertAlphaEq caseLetRecTicksExpected
  , testCase "caseFlat flattens comparisons of an Int" $
      caseFlatEqInt (crCaseFlat rws) >>= assertAlphaEq caseFlatEqIntExpected
  , testCase "elimCaseBigNumInternals drops the alternatives of big integers" $
      elimCaseBigNumUnused (crElimCaseBigNum rws) >>= assertAlphaEq (Var (intId User "a" 2))
  , testCase "elimCaseBigNumInternals keeps a used field" $
      elimCaseBigNumUsed (crElimCaseBigNum rws) >>= assertAlphaEq elimCaseBigNumUsedExpected
  ]
