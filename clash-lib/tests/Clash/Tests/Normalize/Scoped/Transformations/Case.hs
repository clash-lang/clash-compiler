{-|
  Copyright   :  (C) 2026, QBayLogic B.V.
  License     :  BSD2 (see the file LICENSE)
  Maintainer  :  QBayLogic B.V. <devops@qbaylogic.com>

  Tests for "Clash.Normalize.Scoped.Transformations.Case".

  The regression tests guard against bugs that the case transformations had.
  The capture bugs among them can't be written in scoped Core: they are type
  errors. Instead, 'assertRewritesTo' also runs each test in a scope
  whose variables clash with the binders of its input, where a transformation
  that moves a term under one of them has to rename it. The tests in a sunk
  scope ('sunkD') check that the new binders are fresh for that scope.
-}

{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}

module Clash.Tests.Normalize.Scoped.Transformations.Case (tests) where

import Data.Default (def)

import Test.Tasty
import Test.Tasty.HUnit

import qualified Clash.Core.DataCon as C
import qualified Clash.Core.Basic as C (WorkInfo (WorkAlways), IsMultiPrim (SingleResult))
import Clash.Core.Scoped.Syntax (mkPrimInfo)
import Clash.Core.Scoped.Syntax
  (Alt (..), Distinct, Pat (DefaultPat), PrimUnfolding (NoUnfolding), PrimInfo, Term (..), sink)
import qualified Clash.Normalize.Scoped.Transformations.Case as S
import Clash.Normalize.Types (NormalizeState)
import Clash.Rewrite.Scoped (Rewrite, Transform (..))
import Clash.Rewrite.StrategyDSL.TH (asRewriteQ)
import Clash.Rewrite.Types (RewriteState)

import Test.Clash.Rewrite
  ( TestDefs (..), TestTerm, parseTestTermWith, strQQ, testDataCon, varD )
import Test.Clash.Scoped
  ( ScopeDecl, TestScope, assertCaptureRewritesTo, assertLintClean, assertRewritesTo, binderRaws
  , closedType, extendTestScope, runRewriteIn, sunkD, termIn, typeIn, withTestScope, assertTermIs )

-- * Definitions

-- | @MkT :: Int -> T@ and @MkU :: Int -> T@
mkT, mkU :: C.DataCon
mkT = testDataCon "MkT" 1000 1 [] [] ["Int"] "T"
mkU = testDataCon "MkU" 1003 2 [] [] ["Int"] "T"

-- | @I# :: Int# -> Int@
intDataCon :: C.DataCon
intDataCon = testDataCon "GHC.Types.I#" 1010 1 [] [] ["GHC.Prim.Int#"] "Int"

-- | A second constructor of @Integer@, standing in for @IP@
integerIpDc :: C.DataCon
integerIpDc = testDataCon "IP" 1011 2 [] [] ["GHC.Prim.Int#"] "GHC.Num.Integer.Integer"

-- | A primitive @work :: Int -> Int@ that is not work free
workPrim :: PrimInfo
workPrim = mkPrimInfo "work" (closedType "Int -> Int") C.WorkAlways C.SingleResult NoUnfolding

defs :: TestDefs
defs = TestDefs [mkT, mkU, intDataCon, integerIpDc] [workPrim] []

-- | A term that mentions the definitions of the tests
tm :: String -> TestTerm
tm = parseTestTermWith defs

st :: RewriteState NormalizeState
st = def

caseCaseS, caseConS, caseFlatS, caseLetS, elimCaseBigNumS :: Rewrite NormalizeState
caseCaseS = $(asRewriteQ S.caseCase)
caseConS = $(asRewriteQ S.caseCon)
caseFlatS = $(asRewriteQ S.caseFlat)
caseLetS = $(asRewriteQ S.caseLet)
elimCaseBigNumS = $(asRewriteQ S.elimCaseBigNumInternals)

-- | Assert that a transformation rewrites a term to the expected one, see
-- 'assertRewritesTo'
rewrites :: Rewrite NormalizeState -> [ScopeDecl] -> String -> String -> Assertion
rewrites rw decls input expected = assertRewritesTo def st rw decls (tm input) (tm expected)

-- | Assert that a transformation gives the expected term on a capture
-- scenario, see 'assertCaptureRewritesTo'
capture
  :: Rewrite NormalizeState -> [ScopeDecl] -> String -> [ScopeDecl]
  -> (forall l. Distinct l => TestScope l -> Term l -> Term l) -> String -> Assertion
capture rw decls part new mk expected =
  assertCaptureRewritesTo def st rw decls (tm part) new mk (tm expected)

-- | Assert that a transformation, run on a term sunk into a scope with
-- 'sunkD', whose name is above those of all binders of the term, gives the
-- expected term, with binders that are fresh for that scope
assertSunk :: Rewrite NormalizeState -> [ScopeDecl] -> String -> String -> Assertion
assertSunk rw decls input expected =
  withTestScope decls $ \ts0 ->
    extendTestScope ts0 [sunkD] $ \ts -> do
      (e, _, _) <- runRewriteIn def st ts rw (sink (termIn ts0 (tm input)))
      assertLintClean ts e
      assertTermIs ts (tm expected) e
      assertBool ("binders that are not fresh: " <> show (binderRaws e))
        (all (> 1000000) (binderRaws e))

tests :: TestTree
tests = testGroup "Clash.Tests.Normalize.Scoped.Transformations.Case"
  [ testGroup "regression tests" regressionTests
  , testGroup "in a scope that the binders clash with"
      [ testCase "caseCase renames the binders of the inner alternatives" $
          assertSunk caseCaseS [varD "a_1" "T", varD "f_9" "Int -> Int -> Int", varD "x_6" "Int"]
            "(case (case a_1 of { MkT (x :: Int) -> f_9 }) :: Int -> Int of { _ -> x_6 }) :: Int"
            "(case a_1 of { MkT (x :: Int) -> (case f_9 of { _ -> x_6 }) :: Int }) :: Int"
      , testCase "caseLet renames the let-binders" $
          assertSunk caseLetS [varD "u_5" "Int", varD "x_6" "Int"]
            "(case let { !(x :: Int) = u_5 } in x of { _ -> x_6 }) :: Int"
            "let { !(x :: Int) = u_5 } in (case x of { _ -> x_6 }) :: Int"
      , testCase "caseCon let-binds fields with fresh binders" $
          assertSunk caseConS [varD "c_3" "Int", varD "d_4" "Int", varD "f_9" "Int -> Int -> Int"]
            "(case MkPair (Prim.work c_3) d_4 of { MkPair (x :: Int) (y :: Int) -> f_9 x y }) :: Int"
            "let { !(x :: Int) = Prim.work c_3 } in f_9 x d_4"
      ]
  , testGroup "caseCon"
      [ testCase "let-binds fields in the order of the fields" $
          assertSunk caseConS [varD "b_2" "Int", varD "c_3" "Int", varD "f_9" "Int -> Int -> Int"]
            "(case MkPair (Prim.work c_3) (Prim.work b_2) of { MkPair (x :: Int) (y :: Int) -> f_9 x y }) :: Int"
            "let { !(x :: Int) = Prim.work c_3; !(y :: Int) = Prim.work b_2 } in f_9 x y"
      , testCase "caseOneAlt moves equal alternatives out of the scope of their patterns" $
          assertSunk (Transform (\_ -> S.caseOneAlt)) [varD "a_1" "T", varD "c_3" "Int"]
            "(case a_1 of { _ -> c_3; MkT (x :: Int) -> c_3 }) :: Int"
            "c_3"
      ]
  ]

-- | Regression tests of the transformations on case-expressions
regressionTests :: [TestTree]
regressionTests =
  [ -- 'caseCon' used to first substitute the fields for the pattern binders,
    -- and then apply the substitution for the existential type variables to
    -- the result using an in-scope set extended with the pattern binders. The
    -- free variable @a@ of the subject was shadowed by the pattern binder
    -- @pat@, and that second substitution replaced it by @pat@.
    -- https://github.com/clash-lang/clash-compiler/pull/1040
    testCase "caseCon keeps free variables shadowed by the pattern (#1040)" $ do
      rewrites caseConS [varD "a_1" "Int", varD "c_3" "Int"]
        "(case MkPair c_3 a_1 of { MkPair (pat :: Int) (b :: Int) -> b }) :: Int"
        "a_1"
      -- The pattern binder @pat@ has the name of @a@
      capture caseConS [varD "c_3" "Int"]
        "(case c_3 of { MkPair (pat :: Int) (b :: Int) -> b }) :: Int"
        [varD "a_1" "Int"]
        (\ts -> \case
          Case _ ty alts -> Case (termIn ts (tm "MkPair c_3 a_1")) ty alts
          e -> e)
        "a_1"

    -- 'caseCase' pushed the outer case-expression into the alternatives of
    -- the inner one, without renaming the binders of the inner alternatives.
    -- Those could then capture free variables of the outer alternatives.
    -- https://github.com/clash-lang/clash-compiler/pull/1067
  , testCase "caseCase deshadows alternatives (#1067)" $ do
      rewrites caseCaseS [varD "a_1" "T", varD "f_2" "Int -> Int", varD "x_3" "Int"]
        "(case (case a_1 of { MkT (x :: Int) -> f_2 }) :: Int -> Int of { _ -> x_3 }) :: Int"
        "(case a_1 of { MkT (x :: Int) -> (case f_2 of { _ -> x_3 }) :: Int }) :: Int"
      -- The pattern binder @x@ has the name of the free @x@
      capture caseCaseS [varD "a_1" "T", varD "f_2" "Int -> Int"]
        "(case a_1 of { MkT (x :: Int) -> f_2 }) :: Int -> Int"
        [varD "x_3" "Int"]
        (\ts inner -> Case inner (typeIn ts "Int") [Alt DefaultPat (termIn ts (tm "x_3"))])
        "(case a_1 of { MkT (x :: Int) -> (case f_2 of { _ -> x_3 }) :: Int }) :: Int"

    -- 'caseLet' moved the let-bindings out of the subject of a case-expression
    -- without renaming them. They could then capture free variables of the
    -- alternatives. https://github.com/clash-lang/clash-compiler/pull/1071
  , testCase "caseLet deshadows let-bindings (#1071)" $ do
      rewrites caseLetS [varD "u_1" "Int", varD "x_2" "Int"]
        "(case let { !(x :: Int) = u_1 } in x of { _ -> x_2 }) :: Int"
        "let { !(x :: Int) = u_1 } in (case x of { _ -> x_2 }) :: Int"
      -- The let-binder @x@ has the name of the free @x@
      capture caseLetS [varD "u_1" "Int"]
        "let { !(x :: Int) = u_1 } in x"
        [varD "x_2" "Int"]
        (\ts subj -> Case subj (typeIn ts "Int") [Alt DefaultPat (termIn ts (tm "x_2"))])
        "let { !(x :: Int) = u_1 } in (case x of { _ -> x_2 }) :: Int"

    -- 'caseOneAlt' replaced a case-expression whose alternatives are all
    -- equal by its first alternative, only checking whether the variables
    -- bound by the first pattern occur in it. Here the alternatives are alpha
    -- equivalent, but only the first returns the free variable @z@: the
    -- pattern binder @z'@ used to shadow it.
  , testCase "caseOneAlt checks the pattern of every alternative" $ do
      rewrites caseConS [varD "z_5" "Int", varD "w_7" "Int"]
        "(case MkU w_7 of { MkT (y :: Int) -> z_5; MkU (z' :: Int) -> z' }) :: Int"
        "w_7"
      -- The pattern binder @z'@ has the name of the free @z@
      capture caseConS [varD "w_7" "Int"]
        "(case w_7 of { MkU (z' :: Int) -> z' }) :: Int"
        [varD "z_5" "Int"]
        (\ts -> \case
          Case _ ty [alt2] | Case _ _ [alt1] <- termIn ts (tm "(case w_7 of { MkT (y :: Int) -> z_5 }) :: Int")
            -> Case (termIn ts (tm "MkU w_7")) ty [alt1, alt2]
          e -> e)
        "w_7"

  , testCase "caseCon let-binds fields that are not work free" $
      rewrites caseConS [varD "c_1" "Int", varD "d_2" "Int", varD "f_3" "Int -> Int -> Int"]
        "(case MkPair (Prim.work c_1) d_2 of { MkPair (x :: Int) (y :: Int) -> f_3 x y }) :: Int"
        "let { !(x :: Int) = Prim.work c_1 } in f_3 x d_2"
  , testCase "caseCon drops unused fields" $
      rewrites caseConS [varD "c_1" "Int", varD "d_2" "Int"]
        "(case MkPair (Prim.work c_1) (Prim.work d_2) of { MkPair (x :: Int) (y :: Int) -> y }) :: Int"
        "let { !(y :: Int) = Prim.work d_2 } in y"
  , testCase "caseCon gives undefined if no alternative matches" $
      rewrites caseConS [varD "w_7" "Int"]
        "(case MkU w_7 of { MkT (y :: Int) -> y }) :: Int"
        "Clash.Normalize.Primitives.undefined @Int"
  , testCase "caseCon matches the constructors of an Integer literal" $
      rewrites caseConS [varD "a_1" "GHC.Prim.Int#"]
        "(case (5 :: Integer) of { _ -> a_1; GHC.Num.Integer.IS (x :: GHC.Prim.Int#) -> x }) :: GHC.Num.Integer.Integer"
        "let { !(x :: GHC.Prim.Int#) = 5 } in x"
  , testCase "caseLet moves ticks into recursive let-bindings" $
      rewrites caseLetS [varD "f_1" "Int -> Int", varD "x_2" "Int"]
        "(case Core.srcSpan (let { (x :: Int) = f_1 x } in x) of { _ -> x_2 }) :: Int"
        "let { (x :: Int) = Core.srcSpan (f_1 x) } in (case Core.srcSpan x of { _ -> x_2 }) :: Int"
    -- A chain of comparisons, as GHC makes for a case-expression on an @Int@,
    -- becomes a case-expression on the @Int#@ of @x@
  , testCase "caseFlat flattens comparisons of an Int" $
      rewrites caseFlatS [varD "x_1" "Int", varD "a_2" "Int", varD "b_3" "Int", varD "d_4" "Int"]
        [strQQ|
          (case GHC.Classes.eqInt x_1 (GHC.Types.I# 3) of
            { GHC.Types.False ->
                (case GHC.Classes.eqInt x_1 (GHC.Types.I# 2) of
                  { GHC.Types.False -> d_4; GHC.Types.True -> b_3 }) :: Int
            ; GHC.Types.True -> a_2 }) :: Int
        |]
        "(case x_1 of { GHC.Types.I# (wild :: GHC.Prim.Int#) -> (case wild of { _ -> d_4; 3 -> a_2; 2 -> b_3 }) :: Int }) :: Int"
  , testCase "elimCaseBigNumInternals drops the alternatives of big integers" $
      rewrites elimCaseBigNumS
        [varD "i_1" "GHC.Num.Integer.Integer", varD "a_2" "Int", varD "b_3" "Int"]
        "(case i_1 of { GHC.Num.Integer.IS (x :: GHC.Prim.Int#) -> a_2; IP (y :: GHC.Prim.Int#) -> b_3 }) :: Int"
        "a_2"
  , testCase "elimCaseBigNumInternals keeps a used field" $
      rewrites elimCaseBigNumS
        [varD "i_1" "GHC.Num.Integer.Integer", varD "f_2" "GHC.Prim.Int# -> Int", varD "b_3" "Int"]
        "(case i_1 of { GHC.Num.Integer.IS (x :: GHC.Prim.Int#) -> f_2 x; IP (y :: GHC.Prim.Int#) -> b_3 }) :: Int"
        "(case i_1 of { GHC.Num.Integer.IS (x :: GHC.Prim.Int#) -> f_2 x }) :: Int"
  ]
