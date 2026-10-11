{-|
  Copyright   :  (C) 2026, QBayLogic B.V.
  License     :  BSD2 (see the file LICENSE)
  Maintainer  :  QBayLogic B.V. <devops@qbaylogic.com>

  Tests for "Clash.Normalize.Scoped.Transformations.Letrec". Most compare a
  port with its Clash Core counterpart in
  "Clash.Normalize.Transformations.Letrec", running the port through
  'Clash.Rewrite.Scoped.ViaFoil.viaFoil', as the normalizer does.
-}

{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Clash.Tests.Normalize.Scoped.Transformations.Letrec (tests) where

import Data.Default (def)
import Test.Tasty
import Test.Tasty.HUnit

import Clash.Core.Name (NameSort (User), nameOcc, noSrcSpan)
import qualified Clash.Core.Term as C
import qualified Clash.Core.Type as C
import Clash.Core.DataCon (DataCon (..))
import Clash.Core.Var (Id)
import Clash.Core.VarEnv (emptyInScopeSet)
import qualified Clash.Normalize.Scoped.Transformations.Letrec as S
import qualified Clash.Normalize.Transformations.Letrec as Current
import Clash.Normalize.Types (NormRewrite, NormalizeState)
import Clash.Rewrite.Combinators (bottomupR)
import Clash.Rewrite.StrategyDSL.TH (asRewriteQ, asScopedRewriteQ)
import Clash.Rewrite.Types (RewriteState (..), TransformContext (..))
import Clash.Rewrite.Util (apply)

import Clash.Tests.Normalize.Transformations.Letrec (letrecTests)
import Test.Clash.Rewrite
  ( assertAlphaEq, consDc, globalId, globalIntVar, inScopeOf, intFunTy, intId
  , intLit, intTy, localId, nilDc, pairDataCon, pairTy, rewriteEnvWithTyCons
  , runRewriteTest, showPprU, vecTy )
import Test.Clash.Scoped (assertRewritesAgree, runViaFoil)

-- * Running the transformations

-- | Run a rewrite on a term in a context, with the type constructors of the
-- tests. Gives the result and whether a change was signaled.
runIn :: RewriteState NormalizeState -> TransformContext -> NormRewrite -> C.Term -> IO (C.Term, Bool)
runIn st ctx rw e = do
  (t, _, ch) <- runRewriteTest rewriteEnvWithTyCons st (rw ctx e)
  pure (t, ch)

-- | The root context with the given variables in scope
rootCtx :: [Id] -> TransformContext
rootCtx vs = TransformContext (inScopeOf vs) []

-- | Assert that a port agrees with the current transformation, in the given
-- context and state. The current transformations run at a single node, and
-- their ports through 'viaFoil'.
agrees
  :: RewriteState NormalizeState -> TransformContext -> NormRewrite -> NormRewrite
  -> C.Term -> Assertion
agrees = assertRewritesAgree rewriteEnvWithTyCons

-- * Fixtures

a, b, c, x :: Id
a = intId User "a" 1
b = intId User "b" 2
c = intId User "c" 3
x = intId User "x" 4

f :: C.Term
f = globalIntVar User "f" 5

tests :: TestTree
tests = testGroup "Clash.Tests.Normalize.Scoped.Transformations.Letrec"
  [ deadCodeTests
  , removeUnusedExprTests
  , flattenLetTests
  , topLetTests
  , regressionTests
  ]

-- * deadCode

-- | Run 'deadCode' on a closed term, at its root. Gives the result and
-- whether a change was signaled.
deadCodeAt :: C.Term -> IO (C.Term, Bool)
deadCodeAt e = do
  (t, _, ch) <- runViaFoil def st (TransformContext emptyInScopeSet [])
                  $(asScopedRewriteQ S.deadCode) e
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
  rw = bottomupR (apply "deadCode" $(asRewriteQ S.deadCode))
  st :: RewriteState NormalizeState
  st = def

deadCodeTests :: TestTree
deadCodeTests = testGroup "deadCode"
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

-- * removeUnusedExpr

removeUnusedExprC, removeUnusedExprS :: NormRewrite
removeUnusedExprC = apply "removeUnusedExpr" $ \ctx -> \case
  e@(C.Case subj ty alts) -> Current.removeUnusedExprCase ctx e subj ty alts
  e -> Current.removeUnusedExprSpine ctx e
removeUnusedExprS = $(asRewriteQ S.removeUnusedExpr)

removeUnusedExprTests :: TestTree
removeUnusedExprTests = testGroup "removeUnusedExpr"
      [ testCase "removes a case-expression whose pattern binds no used variables" $
          agrees def (rootCtx [s]) removeUnusedExprC removeUnusedExprS
            (C.Case (C.Var s) intTy [(C.DataPat pairDataCon [] [a, b], intLit 1)])
      , testCase "keeps a case-expression whose pattern binds a used variable" $
          agrees def (rootCtx [s]) removeUnusedExprC removeUnusedExprS
            (C.Case (C.Var s) intTy [(C.DataPat pairDataCon [] [a, b], C.Var b)])
      , testCase "replaces a vector of length zero in a Cons by Nil" $ do
          -- Cons @1 @Int @0 co x v, where v :: Vec 0 Int
          let v = localId User "v" 20 (vecTy (C.LitTy (C.NumTy 0)) intTy)
              co = localId User "co" 21 intTy
              e = C.mkApps (C.Data vecCons)
                    [ Right (C.LitTy (C.NumTy 1)), Right intTy, Right (C.LitTy (C.NumTy 0))
                    , Left (C.Var co), Left (C.Var x), Left (C.Var v) ]
          (t, ch) <- runIn def (rootCtx [v, co, x]) removeUnusedExprS e
          ch @?= True
          case C.collectArgs t of
            (C.Data dc, [_, _, _, _, Left x1, Left nil])
              | dc == vecCons, x1 == C.Var x, (C.Data nilDc1, _) <- C.collectArgs nil
              , nilDc1 == nilDc -> pure ()
            _ -> assertFailure ("Unexpected result:\n" <> showPprU t)
      ]
 where
  s = localId User "s" 40 pairTy
  -- The constructor 'Clash.Normalize.Transformations.Letrec.removeUnusedExpr'
  -- recognizes by its name
  vecCons = consDc {dcName = (dcName consDc) {nameOcc = "Clash.Sized.Vector.Cons"}}

-- * flattenLet

d, r, y :: Id
d = intId User "d" 6
r = intId User "r" 7
y = intId User "y" 8

-- | An application of a global function of type @Int -> ... -> Int@
app :: String -> Int -> [C.Term] -> C.Term
app g u args = C.mkApps (C.Var (globalId User g (fromIntegral u) (intFunTy (length args)))) (map Left args)

-- | A tick
srcTick :: C.TickInfo
srcTick = C.SrcSpan noSrcSpan

flattenLetC, flattenLetS :: NormRewrite
flattenLetC = apply "flattenLet" Current.flattenLetWorker
flattenLetS = $(asRewriteQ S.flattenLet)

flattenLetTests :: TestTree
flattenLetTests = testGroup "flattenLet"
      [ testCase "merges nested let-expressions" $
          agrees def (rootCtx [x]) flattenLetC flattenLetS
            (C.Letrec [(a, app "g" 10 [C.Var x])]
              (C.Let (C.NonRec b (app "h" 11 [C.Var a, C.Var x])) (app "k" 12 [C.Var a, C.Var b])))
      , testCase "inlines a work-free binding" $
          agrees def (rootCtx [x]) flattenLetC flattenLetS
            (C.Let (C.NonRec a (C.Var x)) (app "k" 12 [C.Var a, C.Var a]))
      , testCase "inlines a binding used once" $
          agrees def (rootCtx [x]) flattenLetC flattenLetS
            (C.Let (C.NonRec a (app "g" 10 [C.Var x])) (app "k" 12 [C.Var a, C.Var x]))
      , testCase "keeps a binding used twice that performs work" $
          agrees def (rootCtx [x]) flattenLetC flattenLetS
            (C.Let (C.NonRec a (app "g" 10 [C.Var x])) (app "k" 12 [C.Var a, C.Var a]))
      , testCase "keeps a recursive binding" $
          agrees def (rootCtx [x]) flattenLetC flattenLetS
            (C.Letrec [(a, app "g" 10 [C.Var a])] (C.Var a))
      , testCase "merges let-expressions in right-hand sides, before their binding" $
          -- letrec a = 1; b = (letrec c = g a; d = h c in tick (k c d)); r = f b
          -- in r
          agrees def (rootCtx []) flattenLetC flattenLetS
            (C.Letrec
              [ (a, intLit 1)
              , (b, C.Tick srcTick
                      (C.Letrec [(c, app "g" 10 [C.Var a]), (d, app "h" 11 [C.Var c])]
                        (app "k" 12 [C.Var c, C.Var d])))
              , (r, C.App f (C.Var b)) ]
              (C.Var r))
      , testCase "inlines the single binding of a let-expression in a right-hand side" $
          agrees def (rootCtx [x]) flattenLetC flattenLetS
            (C.Letrec
              [ (a, C.Tick srcTick (C.Let (C.NonRec c (app "g" 10 [C.Var x])) (app "k" 12 [C.Var c, C.Var x])))
              , (b, app "h" 11 [C.Var a]) ]
              (app "k" 12 [C.Var a, C.Var b]))
      , testCase "merges a recursive single binding in a right-hand side" $
          agrees def (rootCtx [x]) flattenLetC flattenLetS
            (C.Letrec
              [ (a, C.Letrec [(c, app "h" 11 [C.Var c])] (app "k" 12 [C.Var c, C.Var x]))
              , (b, app "h" 11 [C.Var a]) ]
              (app "k" 12 [C.Var a, C.Var b]))
        -- The Clash Core version deshadows the let-expression in the
        -- right-hand side as a recursive one, which turns @x = g x@ into the
        -- loop @x' = g x'@. In scoped Core, the right-hand side of a 'NonRec'
        -- binding can't refer to its binder.
      , testCase "doesn't make a shadowing NonRec binding in a right-hand side recursive" $ do
          (t, ch) <- runIn def (rootCtx [x]) flattenLetS
            (C.Let (C.NonRec y (C.Let (C.NonRec x (app "g" 10 [C.Var x])) (C.Var x)))
              (C.Var y))
          assertAlphaEq (app "g" 10 [C.Var x]) t
          ch @?= True
      , testCase "doesn't fire on a let-expression without let-expressions" $
          agrees def (rootCtx [x]) flattenLetC flattenLetS
            (C.Letrec [(a, app "g" 10 [C.Var x]), (b, app "h" 11 [C.Var a])]
              (app "k" 12 [C.Var a, C.Var b]))
      ]

-- * topLet

-- | The context of the body of lambdas binding the given variables
lamBodyCtx :: [Id] -> TransformContext
lamBodyCtx vs = TransformContext (inScopeOf vs) (map C.LamBody (reverse vs))

topLetC, topLetS :: NormRewrite
topLetC = apply "topLet" Current.topLetWorker
topLetS = $(asRewriteQ S.topLet)

topLetTests :: TestTree
topLetTests = testGroup "topLet"
      [ testCase "binds the body of a function" $
          agrees def (lamBodyCtx [x]) topLetC topLetS (app "g" 10 [C.Var x])
      , testCase "adds a binding for the body of a let-expression" $
          agrees def (lamBodyCtx [x]) topLetC topLetS
            (C.Letrec [(a, app "g" 10 [C.Var x])] (app "k" 12 [C.Var a, C.Var x]))
      , testCase "adds a binding for the body of a non-recursive let-expression" $
          agrees def (lamBodyCtx [x]) topLetC topLetS
            (C.Let (C.NonRec a (app "g" 10 [C.Var x])) (app "k" 12 [C.Var a, C.Var x]))
      , testCase "doesn't fire on a let-expression whose body is a variable" $
          agrees def (lamBodyCtx [x]) topLetC topLetS
            (C.Letrec [(a, app "g" 10 [C.Var x])] (C.Var a))
      , testCase "doesn't fire on a tick" $
          agrees def (lamBodyCtx [x]) topLetC topLetS (C.Tick srcTick (app "g" 10 [C.Var x]))
      , testCase "doesn't fire on an untranslatable term" $
          agrees def (lamBodyCtx [x]) topLetC topLetS (C.Var (globalId User "g" 10 (intFunTy 1)))
      , testCase "doesn't fire outside the body of a function" $
          agrees def (TransformContext (inScopeOf [x]) [C.AppArg Nothing, C.LamBody x])
            topLetC topLetS (app "g" 10 [C.Var x])
      ]

-- | The regression tests of the Clash Core versions
regressionTests :: TestTree
regressionTests = testGroup "regression tests" (letrecTests flattenLetS topLetS)
