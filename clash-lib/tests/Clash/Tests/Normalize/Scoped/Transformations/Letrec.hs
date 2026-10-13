{-|
  Copyright   :  (C) 2026, QBayLogic B.V.
  License     :  BSD2 (see the file LICENSE)
  Maintainer  :  QBayLogic B.V. <devops@qbaylogic.com>

  Tests for "Clash.Normalize.Scoped.Transformations.Letrec". Most compare a
  port with its Clash Core counterpart in
  "Clash.Normalize.Transformations.Letrec", running the port through
  'Test.Clash.Scoped.ViaFoil.viaFoil', as the normalizer does.
-}

{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
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
import qualified Clash.Normalize.Scoped.Transformations.Letrec as S
import qualified Clash.Normalize.Transformations.Letrec as Current
import Clash.Normalize.Types (NormRewrite, NormalizeState)
import Clash.Core.Scoped.Syntax (Distinct, Term)
import qualified Clash.Rewrite.Scoped.Combinators as SC (bottomupR)
import Clash.Rewrite.Scoped (ScopedRewrite, ScopedTransform (..))
import Test.Clash.Scoped.ViaFoil (viaFoil)
import Clash.Rewrite.StrategyDSL.TH (asScopedRewriteQ)
import Clash.Rewrite.Types (RewriteState (..), TransformContext (..))
import Clash.Rewrite.Util (apply)

import Clash.Tests.Normalize.Transformations.Letrec (letrecTests)
import Test.Clash.Rewrite
  ( assertAlphaEq, consDc, globalId, globalIntVar, inScopeOf, intFunTy, intId
  , intLit, intTy, localId, nilDc, pairDataCon, pairTy, rewriteEnvWithTyCons
  , runRewriteTest, showPprU, vecTy )
import Test.Clash.Scoped
  ( TestScope, assertClashes, assertLintClean, assertRewritesAgree, assertTermAlphaEq
  , assertTermIs, runScopedIn, termQQ, varD, withClashing, withTerm )
import Clash.Core.Scoped.Convert (mkGlobalIdFromClash)

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
  , simpleCSETests
  , recToLetRecTests
  ]

-- * deadCode

-- | Run 'deadCode' at the root of a term, in its scope. Gives the result and
-- whether a change was signaled.
deadCodeAt :: Distinct n => TestScope n -> Term n -> IO (Term n, Bool)
deadCodeAt ts e = do
  (t, _, ch) <- runScopedIn def st ts $(asScopedRewriteQ S.deadCode) e
  pure (t, ch)
 where
  st :: RewriteState NormalizeState
  st = def

-- | 'deadCode' in a bottom-up traversal
deadCodeBottomup :: Distinct n => TestScope n -> Term n -> IO (Term n)
deadCodeBottomup ts e = do
  (t, _, _) <- runScopedIn def st ts (SC.bottomupR $(asScopedRewriteQ S.deadCode)) e
  pure t
 where
  st :: RewriteState NormalizeState
  st = def

deadCodeTests :: TestTree
deadCodeTests = testGroup "deadCode"
      [ testCase "removes an unused non-recursive binding" $
          withTerm "let { !(x :: Int) = 1 } in 2" $ \ts e -> do
            (t, ch) <- deadCodeAt ts e
            assertTermIs ts "2" t
            ch @?= True
      , testCase "keeps a used non-recursive binding" $
          withTerm "let { !(x :: Int) = 1 } in x" $ \ts e -> do
            (t, ch) <- deadCodeAt ts e
            assertTermAlphaEq e t
            ch @?= False
      , testCase "removes a recursive let-binding without used bindings" $
          withTerm "let { (a :: Int) = b; (b :: Int) = a } in 2" $ \ts e -> do
            (t, ch) <- deadCodeAt ts e
            assertTermIs ts "2" t
            ch @?= True
      , testCase "keeps the bindings the used ones refer to, in order" $
          withTerm "let { (a :: Int) = f_G5 b; (c :: Int) = 3; (b :: Int) = 1 } in a" $ \ts e -> do
            (t, ch) <- deadCodeAt ts e
            assertTermIs ts "let { (a :: Int) = f_G5 b; (b :: Int) = 1 } in a" t
            ch @?= True
      , testCase "keeps a recursive let-binding whose bindings are all used" $
          withTerm "let { (a :: Int) = f_G5 b; (b :: Int) = a } in a" $ \ts e -> do
            (t, ch) <- deadCodeAt ts e
            assertTermAlphaEq e t
            ch @?= False
        -- The outer binding is used by the right-hand side of the inner one,
        -- which shadowed it in Clash Core. Here the binders clash with a
        -- variable of the scope instead.
      , testCase "a NonRec binder doesn't keep the binding it shadows alive" $
          withTerm "let { !(x :: Int) = 1 } in let { !(x :: Int) = x } in x" $ \ts0 e0 ->
            withClashing ts0 [varD "z" "Int"] e0 $ \ts e -> do
              assertClashes ts e
              t <- deadCodeBottomup ts e
              assertLintClean ts t
              assertTermAlphaEq e t
      , testCase "in a bottom-up traversal" $
          withTerm [termQQ|
            \(x :: Int) ->
              let { !(a :: Int) = x } in
              let { (b :: Int) = 1; (c :: Int) = a } in
              f_G5 x
          |] $ \ts e -> do
            t <- deadCodeBottomup ts e
            assertTermIs ts "\\(x :: Int) -> f_G5 x" t
      ]

-- * removeUnusedExpr

removeUnusedExprC, removeUnusedExprS :: NormRewrite
removeUnusedExprC = apply "removeUnusedExpr" $ \ctx -> \case
  e@(C.Case subj ty alts) -> Current.removeUnusedExprCase ctx e subj ty alts
  e -> Current.removeUnusedExprSpine ctx e
removeUnusedExprS = (viaFoil "removeUnusedExpr" $(asScopedRewriteQ S.removeUnusedExpr))

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
flattenLetS = (viaFoil "flattenLet" flattenLetSc)

-- | The scoped 'S.flattenLet'
flattenLetSc :: ScopedRewrite NormalizeState
flattenLetSc = $(asScopedRewriteQ S.flattenLet)

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
        -- binding can't refer to its binder. The binder of @x@ also clashes
        -- with a variable of the scope.
      , testCase "doesn't make a shadowing NonRec binding in a right-hand side recursive" $
          withTerm [termQQ|
            let { !(y :: Int) =
              let { !(x_1 :: Int) = (g_G10 :: Int -> Int) (x_1 :: Int) } in x_1 }
            in y
          |] $ \ts0 e0 -> withClashing ts0 [varD "z" "Int"] e0 $ \ts e -> do
            assertClashes ts e
            (t, _, ch) <- runScopedIn rewriteEnvWithTyCons def ts flattenLetSc e
            assertLintClean ts t
            assertTermIs ts "g_G10 x_1" t
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
topLetS = (viaFoil "topLet" $(asScopedRewriteQ S.topLet))

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

-- * simpleCSE

cseC, cseS :: NormRewrite
cseC = apply "CSE" $ \ctx -> \case
  e@(C.Let bind body) -> Current.simpleCSEWorker ctx e bind body
  e -> pure e
cseS = (viaFoil "simpleCSE" $(asScopedRewriteQ S.simpleCSE))

simpleCSETests :: TestTree
simpleCSETests = testGroup "simpleCSE"
      [ testCase "replaces a binding by an equal one" $ do
          let e = C.Letrec [ (a, app "g" 10 [C.Var x]), (b, app "g" 10 [C.Var x])
                           , (c, app "k" 12 [C.Var a, C.Var b]) ]
                    (C.Var c)
          agrees def (rootCtx [x]) cseC cseS e
          (t, _) <- runIn def (rootCtx [x]) cseS e
          assertAlphaEq
            (C.Letrec [(c, app "k" 12 [C.Var a, C.Var a]), (a, app "g" 10 [C.Var x])] (C.Var c))
            t
      , testCase "replaces bindings in a recursive group" $
          agrees def (rootCtx [x]) cseC cseS
            (C.Letrec [ (a, app "k" 12 [C.Var x, C.Var b]), (b, app "k" 12 [C.Var x, C.Var a])
                      , (c, app "k" 12 [C.Var x, C.Var a]), (d, app "k" 12 [C.Var c, C.Var b]) ]
              (C.Var d))
      , testCase "doesn't replace a binding with a NoDeDup tick" $
          agrees def (rootCtx [x]) cseC cseS
            (C.Letrec [ (a, C.Tick C.NoDeDup (app "g" 10 [C.Var x]))
                      , (b, C.Tick C.NoDeDup (app "g" 10 [C.Var x]))
                      , (c, app "k" 12 [C.Var a, C.Var b]) ]
              (C.Var c))
      , testCase "doesn't fire on a non-recursive let-expression" $
          agrees def (rootCtx [x]) cseC cseS
            (C.Let (C.NonRec a (app "g" 10 [C.Var x])) (C.Var a))
      ]

-- * recToLetRec

-- | The function being normalized in the 'recToLetRec' tests,
-- @rec :: Int -> Int -> Int@, applied
recApp :: [C.Term] -> C.Term
recApp = app "rec" 300

-- | A state in which the function being normalized is @rec@, see 'recApp'
recState :: C.Type -> RewriteState NormalizeState
recState ty = def { _curFun = (mkGlobalIdFromClash (globalId User "rec" 300 ty), noSrcSpan) }

recToLetRecC, recToLetRecS :: NormRewrite
recToLetRecC = apply "recToLetRec" Current.recToLetRec
recToLetRecS = apply "recToLetRec" (viaFoil "recToLetRec" (ScopedTransform S.recToLetRec))

recToLetRecTests :: TestTree
recToLetRecTests = testGroup "recToLetRec"
      [ testCase "replaces recursive calls with the original arguments by the result" $ do
          -- \x y -> letrec a = rec x y; b = g a; r = k b a in r
          let e = C.Lam x (C.Lam y
                    (C.Letrec [ (a, recApp [C.Var x, C.Var y]), (b, app "g" 10 [C.Var a])
                              , (r, app "k" 12 [C.Var b, C.Var a]) ]
                      (C.Var r)))
          agrees (recState (intFunTy 2)) (rootCtx []) recToLetRecC recToLetRecS e
          (t, _) <- runIn (recState (intFunTy 2)) (rootCtx []) recToLetRecS e
          assertAlphaEq
            (C.Lam x (C.Lam y
              (C.Letrec [(b, app "g" 10 [C.Var r]), (r, app "k" 12 [C.Var b, C.Var r])] (C.Var r))))
            t
      , testCase "keeps recursive calls with other arguments" $
          agrees (recState (intFunTy 2)) (rootCtx []) recToLetRecC recToLetRecS
            (C.Lam x (C.Lam y
              (C.Letrec [ (a, recApp [C.Var y, C.Var x]), (r, app "k" 12 [C.Var a, C.Var a]) ]
                (C.Var r))))
      , testCase "recognizes an argument rebuilt from its fields" $
          -- \p -> letrec a = rec (MkPair (case p of MkPair c d -> c)
          --                              (case p of MkPair c d -> d));
          --              r = k a a
          --       in r
          let p = localId User "p" 30 pairTy
              field i = C.Case (C.Var p) intTy [(C.DataPat pairDataCon [] [c, d], C.Var ([c, d] !! i))]
              recP = C.App (C.Var (globalId User "rec" 300 (C.mkFunTy pairTy intTy)))
              e = C.Lam p
                    (C.Letrec [ (a, recP (C.mkApps (C.Data pairDataCon) [Left (field 0), Left (field 1)]))
                              , (r, app "k" 12 [C.Var a, C.Var a]) ]
                      (C.Var r))
          in  agrees (recState (C.mkFunTy pairTy intTy)) (rootCtx []) recToLetRecC recToLetRecS e
      , testCase "merges nested let-expressions, and moves their ticks to the bindings" $
          agrees (recState (intFunTy 2)) (rootCtx []) recToLetRecC recToLetRecS
            (C.Lam x (C.Lam y (C.Tick srcTick
              (C.Let (C.NonRec a (recApp [C.Var x, C.Var y]))
                (C.Letrec [(b, app "g" 10 [C.Var a]), (r, app "k" 12 [C.Var b, C.Var a])]
                  (C.Var r))))))
      , testCase "doesn't fire without other bindings" $
          agrees (recState (intFunTy 2)) (rootCtx []) recToLetRecC recToLetRecS
            (C.Lam x (C.Lam y (C.Letrec [(r, recApp [C.Var x, C.Var y])] (C.Var r))))
      , testCase "doesn't fire on a term that is not in normal form" $
          agrees (recState (intFunTy 2)) (rootCtx []) recToLetRecC recToLetRecS
            (C.Lam x (C.Lam y (app "k" 12 [recApp [C.Var x, C.Var y], C.Var x])))
      , testCase "doesn't fire outside the root" $
          agrees (recState (intFunTy 2)) (TransformContext (inScopeOf [x, y]) [C.LamBody y])
            recToLetRecC recToLetRecS
            (C.Letrec [ (a, recApp [C.Var x, C.Var y]), (b, app "g" 10 [C.Var a])
                      , (r, app "k" 12 [C.Var b, C.Var a]) ]
              (C.Var r))
        -- The Clash Core version would replace @r@ by itself, leaving it
        -- unbound
      , testCase "doesn't fire when the result is a recursive call" $ do
          let e = C.Lam x (C.Lam y
                    (C.Letrec [(r, recApp [C.Var x, C.Var y]), (b, app "g" 10 [C.Var r])]
                      (C.Var r)))
          (t, ch) <- runIn (recState (intFunTy 2)) (rootCtx []) recToLetRecS e
          assertAlphaEq e t
          ch @?= False
      ]
