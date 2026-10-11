{-|
  Copyright   :  (C) 2026, QBayLogic B.V.
  License     :  BSD2 (see the file LICENSE)
  Maintainer  :  QBayLogic B.V. <devops@qbaylogic.com>

  Tests for "Clash.Normalize.Scoped.Transformations.Case": the tests of
  "Clash.Normalize.Transformations.Case", run on the ports through
  'Clash.Rewrite.Scoped.ViaFoil.viaFoil', and tests of the ports on terms
  whose binders clash with the scope.

  The capture bugs that the regression tests of
  "Clash.Normalize.Transformations.Case" guard against can't be written in
  scoped Core: they are type errors. Through
  'Clash.Rewrite.Scoped.ViaFoil.viaFoil', conversion gives the binders of
  their inputs fresh names, so the ports don't even see binders that shadow
  free variables. The tests in a sunk scope ('runSunk') give the ports such
  binders: they have to rename them.
-}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}

module Clash.Tests.Normalize.Scoped.Transformations.Case (tests) where

import Data.Default (def)

import Test.Tasty
import Test.Tasty.HUnit

import Clash.Core.Name (NameSort (User))
import qualified Clash.Core.DataCon as C
import qualified Clash.Core.Term as C
import qualified Clash.Core.Type as C
import qualified Clash.Core.Var as C
import qualified Clash.Normalize.Scoped.Transformations.Case as S
import Clash.Normalize.Types (NormalizeState)
import Clash.Rewrite.Scoped (ScopedRewrite, ScopedTransform (..), rootContext)
import Clash.Rewrite.StrategyDSL.TH (asRewriteQ, asScopedRewriteQ)
import Clash.Rewrite.Types (RewriteState)

import Clash.Core.Scoped.Convert (convScope, fromClashTerm, toClashTerm)
import Clash.Core.Scoped.Syntax (sink)

import Clash.Tests.Normalize.Transformations.Case (CaseRewrites (..), caseTests)
import Test.Clash.Rewrite
  ( assertAlphaEq, intFunTy, intId, intTy, localId, mkDataCon, pairDataCon
  , parseTyConTy, runRewriteTest )
import Test.Clash.Scoped (binderRaws, extendSunk, sunkVar, withFreeVarsOf)

-- | The ports, as rewrites of Clash Core: they run through
-- 'Clash.Rewrite.Scoped.ViaFoil.viaFoil'
scopedRewrites :: CaseRewrites
scopedRewrites = CaseRewrites
  { crCaseCase = $(asRewriteQ S.caseCase)
  , crCaseCon = $(asRewriteQ S.caseCon)
  , crCaseFlat = $(asRewriteQ S.caseFlat)
  , crCaseLet = $(asRewriteQ S.caseLet)
  , crElimCaseBigNum = $(asRewriteQ S.elimCaseBigNumInternals)
  }

-- | Run a scoped rewrite at the root of a term, converted to scoped Core and
-- sunk into a larger scope ('extendSunk'): all of its binders clash with that
-- scope, so a rewrite that moves a term under them has to rename them. Gives
-- the result, converted back, and the raw names of its binders.
runSunk :: ScopedRewrite NormalizeState -> C.Term -> IO (C.Term, [Int])
runSunk rw tm = do
  (r, _, _) <- runRewriteTest def st $
    withFreeVarsOf [tm] $ \env -> extendSunk env [] [] $ \env1 -> do
      e <- runScopedTransform rw (rootContext (convScope env1)) (sink (fromClashTerm env tm))
      pure (toClashTerm e, binderRaws e)
  pure r
 where
  st :: RewriteState NormalizeState
  st = def

-- | Assert that a rewrite in a sunk scope ('runSunk') gives the expected
-- term, with binders that are fresh for the larger scope
assertSunk :: ScopedRewrite NormalizeState -> C.Term -> C.Term -> Assertion
assertSunk rw tm expected = do
  (r, raws) <- runSunk rw tm
  assertAlphaEq expected r
  assertBool ("binders that are not fresh: " <> show raws)
    (all (> fromIntegral (C.varUniq sunkVar)) raws)

-- | @T@, a data type with a single constructor 'mkT'
tTy :: C.Type
tTy = parseTyConTy "T"

-- | @MkT :: Int -> T@
mkT :: C.DataCon
mkT = mkDataCon "MkT" 1000 [] [] [intTy] tTy

-- | A primitive @work :: Int -> Int@ that is not work free
workPrim :: C.Term
workPrim = C.Prim (C.PrimInfo "work" (intFunTy 1) C.WorkAlways C.SingleResult C.NoUnfolding)

a, b, c, d, u, x, x1, y1 :: C.Id
a = localId User "a" 1 tTy
b = intId User "b" 2
c = intId User "c" 3
d = intId User "d" 4
u = intId User "u" 5
x = intId User "x" 6
x1 = intId User "x" 7
y1 = intId User "y" 8

f :: C.Id
f = localId User "f" 9 (intFunTy 2)

tests :: TestTree
tests = testGroup "Clash.Tests.Normalize.Scoped.Transformations.Case"
  [ testGroup "through viaFoil" (caseTests scopedRewrites)
  , testGroup "in a scope that the binders clash with"
      [ testCase "caseCase renames the binders of the inner alternatives" $
          assertSunk $(asScopedRewriteQ S.caseCase)
            (C.Case (C.Case (C.Var a) (intFunTy 1) [(C.DataPat mkT [] [x], C.Var f)])
                    intTy [(C.DefaultPat, C.Var x)])
            (C.Case (C.Var a) intTy
               [(C.DataPat mkT [] [x1], C.Case (C.Var f) intTy [(C.DefaultPat, C.Var x)])])
      , testCase "caseLet renames the let-binders" $
          assertSunk $(asScopedRewriteQ S.caseLet)
            (C.Case (C.Let (C.NonRec x (C.Var u)) (C.Var x)) intTy [(C.DefaultPat, C.Var x)])
            (C.Let (C.NonRec x1 (C.Var u)) (C.Case (C.Var x1) intTy [(C.DefaultPat, C.Var x)]))
      , testCase "caseCon let-binds fields with fresh binders" $
          assertSunk $(asScopedRewriteQ S.caseCon)
            (C.Case (C.mkApps (C.Data pairDataCon) [Left (C.App workPrim (C.Var c)), Left (C.Var d)])
                    intTy
                    [(C.DataPat pairDataCon [] [x, y1], C.mkApps (C.Var f) [Left (C.Var x), Left (C.Var y1)])])
            (C.Let (C.NonRec x1 (C.App workPrim (C.Var c)))
               (C.mkApps (C.Var f) [Left (C.Var x1), Left (C.Var d)]))
      ]
  , testGroup "caseCon"
      [ -- The current 'caseCon' orders the let-bindings with
        -- 'Clash.Core.Util.listToLets', so the order of its result depends on
        -- the uniques it gives the binders
        testCase "let-binds fields in the order of the fields" $
          assertSunk $(asScopedRewriteQ S.caseCon)
            (C.Case (C.mkApps (C.Data pairDataCon) [Left (C.App workPrim (C.Var c)), Left (C.App workPrim (C.Var b))])
                    intTy
                    [(C.DataPat pairDataCon [] [x, y1], C.mkApps (C.Var f) [Left (C.Var x), Left (C.Var y1)])])
            (C.Let (C.NonRec x1 (C.App workPrim (C.Var c)))
               (C.Let (C.NonRec u (C.App workPrim (C.Var b)))
                  (C.mkApps (C.Var f) [Left (C.Var x1), Left (C.Var u)])))
      , testCase "caseOneAlt moves equal alternatives out of the scope of their patterns" $
          assertSunk (ScopedTransform (\_ -> S.caseOneAlt))
            (C.Case (C.Var a) intTy
               [(C.DefaultPat, C.Var c), (C.DataPat mkT [] [x], C.Var c)])
            (C.Var c)
      ]
  ]
