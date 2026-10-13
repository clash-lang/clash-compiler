{-|
  Copyright   :  (C) 2026, QBayLogic B.V.
  License     :  BSD2 (see the file LICENSE)
  Maintainer  :  QBayLogic B.V. <devops@qbaylogic.com>

  Tests for "Clash.Normalize.Scoped.Transformations.Letrec".
  'assertRewritesTo' runs every test in the scope of the free variables of
  its input, and in a scope that the binders of the input clash with.
-}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Clash.Tests.Normalize.Scoped.Transformations.Letrec (tests) where

import Data.Default (def)
import Test.Tasty
import Test.Tasty.HUnit

import Clash.Core.Name (nameOcc)
import Clash.Core.DataCon (DataCon (..))
import qualified Clash.Normalize.Scoped.Transformations.Letrec as S
import Clash.Normalize.Types (NormalizeState)
import Clash.Core.Syntax (Distinct, GlobalId, Term)
import qualified Clash.Rewrite.Combinators as SC (bottomupR)
import Clash.Driver.Types (DebugOpts (..), debugSilent)
import Clash.Rewrite (Rewrite, Transform (..))
import Clash.Rewrite.Apply (applyDebug)
import Clash.Rewrite.StrategyDSL.TH (asRewriteQ)
import Clash.Rewrite.Types (RewriteEnv, RewriteState (..))

import Test.Clash.Rewrite
  ( TestDefs (..), TestTerm, TestType, assertErrorContainsIO, consDc, parseTestTermWith
  , rewriteEnvWithDebug, rewriteEnvWithTyCons, runRewriteTest, strQQ, testGlobalId )
import Test.Clash.Scoped
  ( ScopeDecl, Step (..), TestScope, assertClashes, assertLintClean, assertNoRewrite
  , assertNoRewriteAt, assertRewritesAt, assertRewritesTo, assertTermAlphaEq, assertTermIs, runRewriteIn
  , termIn, termQQ, varD, withClashing, withCurFun, withTerm )

-- * Definitions

-- | The global functions of the tests
globals :: [GlobalId]
globals =
  [ testGlobalId "f_G5" "Int -> Int"
  , testGlobalId "g_G10" "Int -> Int"
  , testGlobalId "h_G11" "Int -> Int"
  , testGlobalId "k_G12" "Int -> Int -> Int"
  , testGlobalId "f_G102" "Int -> Int"
  , testGlobalId "g_G103" "Int -> Int -> Int"
  , testGlobalId "h_G104" "Int -> Int -> Int -> Int"
  , testGlobalId "c_G99" "Int" ]

-- | The constructor 'Clash.Normalize.Scoped.Transformations.Letrec.removeUnusedExpr'
-- recognizes by its name
vecCons :: DataCon
vecCons = consDc {dcName = (dcName consDc) {nameOcc = "Clash.Sized.Vector.Cons"}}

tm :: String -> TestTerm
tm = parseTestTermWith (TestDefs [vecCons] [] globals)

env :: RewriteEnv
env = rewriteEnvWithTyCons

st :: RewriteState NormalizeState
st = def

-- | Assert that a transformation rewrites a term to the expected one, see
-- 'assertRewritesTo'
rewrites :: Rewrite NormalizeState -> [ScopeDecl] -> TestTerm -> TestTerm -> Assertion
rewrites = assertRewritesTo env st

-- | Assert that a transformation leaves a term as it is
keeps :: Rewrite NormalizeState -> [ScopeDecl] -> TestTerm -> Assertion
keeps = assertNoRewrite env st

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
  (t, _, ch) <- runRewriteIn def st ts $(asRewriteQ S.deadCode) e
  pure (t, ch)

-- | 'deadCode' in a bottom-up traversal
deadCodeBottomup :: Distinct n => TestScope n -> Term n -> IO (Term n)
deadCodeBottomup ts e = do
  (t, _, _) <- runRewriteIn def st ts (SC.bottomupR $(asRewriteQ S.deadCode)) e
  pure t

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
        -- whose binder has the same name. The binders also clash with a
        -- variable of the scope.
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

removeUnusedExprS :: Rewrite NormalizeState
removeUnusedExprS = $(asRewriteQ S.removeUnusedExpr)

removeUnusedExprTests :: TestTree
removeUnusedExprTests = testGroup "removeUnusedExpr"
  [ testCase "removes a case-expression whose pattern binds no used variables" $
      rewrites removeUnusedExprS [varD "s_40" "Pair"]
        (tm "(case s_40 of { MkPair (a :: Int) (b :: Int) -> 1 }) :: Int")
        (tm "1")
  , testCase "keeps a case-expression whose pattern binds a used variable" $
      keeps removeUnusedExprS [varD "s_40" "Pair"]
        (tm "(case s_40 of { MkPair (a :: Int) (b :: Int) -> b }) :: Int")
  , testCase "replaces a vector of length zero in a Cons by Nil" $
      rewrites removeUnusedExprS
        [varD "x_4" "Int", varD "v_20" "Clash.Sized.Vector.Vec 0 Int", varD "co_21" "Int"]
        (tm "Clash.Sized.Vector.Cons @1 @Int @0 co_21 x_4 v_20")
        (tm [strQQ|
          Cons @1 @Int @0 (Prim._CO_ @((GHC.Prim.~#) Nat Nat 1 ((GHC.TypeNats.+) 0 1))) x_4
            (Nil @0 @Int (Prim._CO_ @((GHC.Prim.~#) Nat Nat 0 0)))
        |])
  ]

-- * flattenLet

-- | The scoped 'S.flattenLet'
flattenLetSc :: Rewrite NormalizeState
flattenLetSc = $(asRewriteQ S.flattenLet)

flattenLetTests :: TestTree
flattenLetTests = testGroup "flattenLet"
  [ testCase "merges nested let-expressions" $
      rewrites flattenLetSc [varD "x_4" "Int"]
        (tm "let { (a :: Int) = g_G10 x_4 } in let { !(b :: Int) = (h_G11 :: Int -> Int -> Int) a x_4 } in k_G12 a b")
        (tm "let { (a :: Int) = g_G10 x_4; (b :: Int) = (h_G11 :: Int -> Int -> Int) a x_4 } in k_G12 a b")
  , testCase "inlines a work-free binding" $
      rewrites flattenLetSc [varD "x_4" "Int"]
        (tm "let { !(a :: Int) = x_4 } in k_G12 a a")
        (tm "k_G12 x_4 x_4")
  , testCase "inlines a binding used once" $
      rewrites flattenLetSc [varD "x_4" "Int"]
        (tm "let { !(a :: Int) = g_G10 x_4 } in k_G12 a x_4")
        (tm "k_G12 (g_G10 x_4) x_4")
  , testCase "keeps a binding used twice that performs work" $
      keeps flattenLetSc [varD "x_4" "Int"]
        (tm "let { !(a :: Int) = g_G10 x_4 } in k_G12 a a")
  , testCase "keeps a recursive binding" $
      keeps flattenLetSc [] (tm "let { (a :: Int) = g_G10 a } in a")
  , testCase "merges let-expressions in right-hand sides, before their binding" $
      rewrites flattenLetSc []
        (tm [strQQ|
          let
            { (a :: Int) = 1
            ; (b :: Int) = Core.srcSpan (let { (c :: Int) = g_G10 a; (d :: Int) = h_G11 c } in k_G12 c d)
            ; (r :: Int) = f_G5 b
            } in r
        |])
        (tm [strQQ|
          let
            { (a :: Int) = 1
            ; (c :: Int) = g_G10 a
            ; (d :: Int) = h_G11 c
            ; (b :: Int) = Core.srcSpan (k_G12 c d)
            ; (r :: Int) = f_G5 b
            } in r
        |])
  , testCase "inlines the single binding of a let-expression in a right-hand side" $
      rewrites flattenLetSc [varD "x_4" "Int"]
        (tm [strQQ|
          let
            { (a :: Int) = Core.srcSpan (let { !(c :: Int) = g_G10 x_4 } in k_G12 c x_4)
            ; (b :: Int) = h_G11 a
            } in k_G12 a b
        |])
        (tm "let { (a :: Int) = Core.srcSpan (k_G12 (g_G10 x_4) x_4); (b :: Int) = h_G11 a } in k_G12 a b")
  , testCase "merges a recursive single binding in a right-hand side" $
      rewrites flattenLetSc [varD "x_4" "Int"]
        (tm "let { (a :: Int) = let { (c :: Int) = h_G11 c } in k_G12 c x_4; (b :: Int) = h_G11 a } in k_G12 a b")
        (tm "let { (c :: Int) = h_G11 c; (a :: Int) = k_G12 c x_4; (b :: Int) = h_G11 a } in k_G12 a b")
    -- Deshadowing the let-expression in the right-hand side as if it were
    -- recursive would turn @x = g x@ into the loop @x' = g x'@. In scoped
    -- Core, the right-hand side of a 'NonRec' binding can't refer to its
    -- binder. The binder of @x@ also clashes with a variable of the scope.
  , testCase "doesn't make a shadowing NonRec binding in a right-hand side recursive" $
      withTerm [termQQ|
        let { !(y :: Int) =
          let { !(x_1 :: Int) = (g_G10 :: Int -> Int) (x_1 :: Int) } in x_1 }
        in y
      |] $ \ts0 e0 -> withClashing ts0 [varD "z" "Int"] e0 $ \ts e -> do
        assertClashes ts e
        (t, _, ch) <- runRewriteIn rewriteEnvWithTyCons def ts flattenLetSc e
        assertLintClean ts t
        assertTermIs ts "g_G10 x_1" t
        ch @?= True
  , testCase "doesn't fire on a let-expression without let-expressions" $
      keeps flattenLetSc [varD "x_4" "Int"]
        (tm "let { (a :: Int) = g_G10 x_4; (b :: Int) = h_G11 a } in k_G12 a b")
  ]

-- * topLet

topLetS :: Rewrite NormalizeState
topLetS = $(asRewriteQ S.topLet)

-- | Assert that 'S.topLet', in the body of the outer lambda of a term, gives
-- the expected term
topLetTo :: [ScopeDecl] -> TestTerm -> TestTerm -> Assertion
topLetTo = assertRewritesAt env st [LamStep] topLetS

-- | Assert that 'S.topLet', at the given path, leaves a term as it is
topLetKeeps :: [Step] -> [ScopeDecl] -> TestTerm -> Assertion
topLetKeeps path = assertNoRewriteAt env st path topLetS

topLetTests :: TestTree
topLetTests = testGroup "topLet"
  [ testCase "binds the body of a function" $
      topLetTo []
        (tm "\\(x :: Int) -> g_G10 x")
        (tm "\\(x :: Int) -> let { !(r :: Int) = g_G10 x } in r")
  , testCase "adds a binding for the body of a let-expression" $
      topLetTo []
        (tm "\\(x :: Int) -> let { (a :: Int) = g_G10 x } in k_G12 a x")
        (tm "\\(x :: Int) -> let { (a :: Int) = g_G10 x; (r :: Int) = k_G12 a x } in r")
  , testCase "adds a binding for the body of a non-recursive let-expression" $
      topLetTo []
        (tm "\\(x :: Int) -> let { !(a :: Int) = g_G10 x } in k_G12 a x")
        (tm "\\(x :: Int) -> let { (a :: Int) = g_G10 x; (r :: Int) = k_G12 a x } in r")
  , testCase "doesn't fire on a let-expression whose body is a variable" $
      topLetKeeps [LamStep] [] (tm "\\(x :: Int) -> let { (a :: Int) = g_G10 x } in a")
  , testCase "doesn't fire on a tick" $
      topLetKeeps [LamStep] [] (tm "\\(x :: Int) -> Core.srcSpan (g_G10 x)")
  , testCase "doesn't fire on an untranslatable term" $
      topLetKeeps [LamStep] [] (tm "\\(x :: Int) -> g_G10")
  , testCase "doesn't fire outside the body of a function" $
      topLetKeeps [LamStep, AppArgStep] [] (tm "\\(x :: Int) -> f_G5 (g_G10 x)")
  ]

-- | Regression tests for bugs in the transformations on let-expressions
regressionTests :: TestTree
regressionTests = testGroup "regression tests"
  [ -- 'topLet' turns @letrec x = 1 in 2@ into @letrec x = 1; result = 2 in
    -- result@. Its new binder must not clash with the binders of the let
    -- expression. Before #490, 'topLet' made a new binder using an in-scope
    -- set that did not contain them.
    -- https://github.com/clash-lang/clash-compiler/pull/490
    testCase "topLet does not reuse the unique of a let-binder (#490)" $
      rewrites topLetS []
        (tm "let { (x :: Int) = 1 } in 2")
        (tm "let { (x :: Int) = 1; (r :: GHC.Prim.Int#) = 2 } in r")

    -- #1766 / #1837: 'flattenLet' did not flatten a let-expression whose body
    -- is another let-expression. Doing so requires deshadowing the inner
    -- bindings, as they may reuse uniques of the outer ones: here the inner
    -- @x@ shadows the outer one. Unflattened, and with shadowing binders,
    -- Clash generated duplicate signal names.
    -- https://github.com/clash-lang/clash-compiler/pull/1837
  , testCase "flattenLet deshadows when flattening nested letrec (#1837)" $
      rewrites flattenLetSc []
        (tm [strQQ|
          let { (x :: Int) = f_G102 y; (y :: Int) = f_G102 x } in
            let { (x :: Int) = g_G103 y z; (z :: Int) = g_G103 x x } in
              h_G104 x z y
        |])
        (tm [strQQ|
          let
            { (x :: Int) = f_G102 y
            ; (y :: Int) = f_G102 x
            ; (x2 :: Int) = g_G103 y z
            ; (z :: Int) = g_G103 x2 x2
            } in h_G104 x2 z y
        |])

    -- #1837 also made flattening a nested letrec signal a change (it merely
    -- merges two let-expressions, so no other code path in 'flattenLet' does).
    -- Without it, the strategy might not run to a fixed point.
  , testCase "flattenLet signals change when flattening nested letrec (#1837)" $
      rewrites flattenLetSc []
        (tm "let { (a :: Int) = f_G102 b; (b :: Int) = f_G102 a } in let { (c :: Int) = g_G103 a b } in h_G104 c c")
        (tm "let { (a :: Int) = f_G102 b; (b :: Int) = f_G102 a; (c :: Int) = g_G103 a b } in h_G104 c c")

    -- #1837 also made applying a transformation ('applyDebug') report a
    -- transformation that changes a term without signalling it, when checking
    -- invariants
  , testCase "apply reports an unsignalled change (#1837)" $
      withTerm (tm "let { (a :: Int) = f_G102 b; (b :: Int) = f_G102 a } in a") $ \ts e ->
        assertErrorContainsIO "Expression changed without notice" $ do
          -- 'applyDebug' skips its checks until the first transformation
          -- that signals a change, so pretend one already did
          (e', _, _) <- runRewriteTest (rewriteEnvWithDebug debugSilent{dbg_invariants = True})
            st{_transformCounter = 1} $
            applyDebug "flattenLet" e False (termIn ts (tm "3"))
          pure e'

    -- 'flattenLet' deshadowed a non-recursive let-expression in a right-hand
    -- side as if it were recursive. Its binder @x@ shadowed the variable
    -- @x_9@ its right-hand side refers to, so renaming both turned
    --
    --   let y = (let x_9 = g_5 x_9 in x_9) in y
    --
    -- into the loop @letrec x' = g_5 x'; y = x' in y@.
  , testCase "flattenLet keeps a shadowing NonRec let in a right-hand side non-recursive" $
      rewrites flattenLetSc [varD "g_5" "Int -> Int", varD "x_9" "Int"]
        (tm "let { !(y :: Int) = let { !(x :: Int) = g_5 x_9 } in x } in y")
        (tm "g_5 x_9")

    -- #1103 made 'flattenLet' deshadow a let-expression in the right-hand
    -- side of a let-binding (before merging it into the outer let-expression)
    -- only when one of its binders is already in scope. That set of in-scope
    -- variables must include the context of the let-expression, the outer
    -- binders, /and/ the binders merged in from earlier right-hand sides.
    -- Here:
    --
    --   * the binder @x@ in @a@ is also a free variable (bound in the
    --     context), referred to by @b@: not renaming it captures that
    --     reference;
    --   * the binder @u@ in @b@ was already merged in from @a@;
    --   * the binder @a@ in @b@ is also an outer binder.
    --
    -- https://github.com/clash-lang/clash-compiler/pull/1103
  , testCase "flattenLet deshadows nested let-bindings when needed (#1103)" $
      rewrites flattenLetSc [varD "x_9" "Int"]
        (tm [strQQ|
          let
            { (a :: Int) = let { (x :: Int) = f_G102 c_G99; (u :: Int) = g_G103 x x } in u
            ; (b :: Int) = let { (u :: Int) = f_G102 x_9; (a :: Int) = g_G103 u u } in a
            } in h_G104 a b x_9
        |])
        (tm [strQQ|
          let
            { (x :: Int) = f_G102 c_G99
            ; (u :: Int) = g_G103 x x
            ; (a :: Int) = u
            ; (u2 :: Int) = f_G102 x_9
            ; (a2 :: Int) = g_G103 u2 u2
            ; (b :: Int) = a2
            } in h_G104 a b x_9
        |])

    -- 'flattenLet' merges a let-expression into the non-recursive
    -- let-expression whose body it is: @let x = g x in let y = h x in y@,
    -- where the right-hand side @g x@ refers to the outer @x@, must not
    -- become @letrec x = g x; y = h x in y@.
  , testCase "flattenLet merges a let into a shadowing NonRec let" $
      rewrites flattenLetSc [varD "x_0" "Int"]
        (tm "let { !(x :: Int) = g_G10 x_0; !(y :: Int) = h_G11 x } in y")
        (tm "let { (x :: Int) = g_G10 x_0; (y :: Int) = h_G11 x } in y")

    -- 'flattenLet' merges a let-expression in the right-hand side of a
    -- non-recursive let-binding into it: @let x = (letrec y = h x; z = k y in
    -- z) in f x x@ must become @letrec y = h x; z = k y; x' = z in f x' x'@,
    -- where @h x@ still refers to the outer @x@.
  , testCase "flattenLet merges a let into a shadowing NonRec binding" $
      rewrites flattenLetSc [varD "x_0" "Int"]
        (tm "let { !(x :: Int) = let { (y :: Int) = h_G11 x_0; (z :: Int) = (k_G12 :: Int -> Int) y } in z } in (f_G10 :: Int -> Int -> Int) x x")
        (tm "let { (y :: Int) = h_G11 x_0; (z :: Int) = k_G12 y; (x :: Int) = z } in f_G10 x x")

    -- 'topLet' adds a binder for the body of a non-recursive let-expression
    -- to its bindings: @let x = g x in f x@ must become
    -- @letrec x' = g x; result = f x' in result@.
  , testCase "topLet adds its binder to a shadowing NonRec let" $
      rewrites topLetS [varD "x_0" "Int"]
        (tm "let { !(x :: Int) = g_G10 x_0 } in (f_G11 :: Int -> Int) x")
        (tm "let { (x :: Int) = g_G10 x_0; (r :: Int) = f_G11 x } in r")
  ]

-- * simpleCSE

cseS :: Rewrite NormalizeState
cseS = $(asRewriteQ S.simpleCSE)

simpleCSETests :: TestTree
simpleCSETests = testGroup "simpleCSE"
  [ testCase "replaces a binding by an equal one" $
      rewrites cseS [varD "x_4" "Int"]
        (tm "let { (a :: Int) = g_G10 x_4; (b :: Int) = g_G10 x_4; (c :: Int) = k_G12 a b } in c")
        (tm "let { (c :: Int) = k_G12 a a; (a :: Int) = g_G10 x_4 } in c")
  , testCase "replaces bindings in a recursive group" $
      rewrites cseS [varD "x_4" "Int"]
        (tm [strQQ|
          let
            { (a :: Int) = k_G12 x_4 b
            ; (b :: Int) = k_G12 x_4 a
            ; (c :: Int) = k_G12 x_4 a
            ; (d :: Int) = k_G12 c b
            } in d
        |])
        (tm "let { (d :: Int) = k_G12 b b; (a :: Int) = k_G12 x_4 b; (b :: Int) = k_G12 x_4 a } in d")
  , testCase "doesn't replace a binding with a NoDeDup tick" $
      keeps cseS [varD "x_4" "Int"]
        (tm [strQQ|
          let
            { (a :: Int) = Core.noDeDup (g_G10 x_4)
            ; (b :: Int) = Core.noDeDup (g_G10 x_4)
            ; (c :: Int) = k_G12 a b
            } in c
        |])
  , testCase "doesn't fire on a non-recursive let-expression" $
      keeps cseS [varD "x_4" "Int"] (tm "let { !(a :: Int) = g_G10 x_4 } in a")
  ]

-- * recToLetRec

recToLetRecS :: Rewrite NormalizeState
recToLetRecS = Transform S.recToLetRec

-- | A state in which the function being normalized is @rec@, of the given
-- type
recState :: TestType -> RewriteState NormalizeState
recState ty = withCurFun (testGlobalId "rec_G300" ty) st

-- | Assert that 'S.recToLetRec' rewrites a term to the expected one, in a
-- state in which the function being normalized is @rec :: Int -> Int ->
-- Int@
recTo :: TestTerm -> TestTerm -> Assertion
recTo = assertRewritesTo env (recState "Int -> Int -> Int") recToLetRecS []

-- | Assert that 'S.recToLetRec' leaves a term as it is, see 'recTo'
recKeeps :: [ScopeDecl] -> TestTerm -> Assertion
recKeeps = assertNoRewrite env (recState "Int -> Int -> Int") recToLetRecS

-- | @rec :: Int -> Int -> Int@, applied
recTm :: String -> TestTerm
recTm = parseTestTermWith (TestDefs [] [] (testGlobalId "rec_G300" "Int -> Int -> Int" : globals))

recToLetRecTests :: TestTree
recToLetRecTests = testGroup "recToLetRec"
  [ testCase "replaces recursive calls with the original arguments by the result" $
      recTo
        (recTm "\\(x :: Int) (y :: Int) -> let { (a :: Int) = rec_G300 x y; (b :: Int) = g_G10 a; (r :: Int) = k_G12 b a } in r")
        (recTm "\\(x :: Int) (y :: Int) -> let { (b :: Int) = g_G10 r; (r :: Int) = k_G12 b r } in r")
  , testCase "keeps recursive calls with other arguments" $
      recKeeps []
        (recTm "\\(x :: Int) (y :: Int) -> let { (a :: Int) = rec_G300 y x; (r :: Int) = k_G12 a a } in r")
  , testCase "recognizes an argument rebuilt from its fields" $
      assertRewritesTo env (recState "Pair -> Int") recToLetRecS []
        (tm [strQQ|
          \(p :: Pair) ->
            let
              { (a :: Int) = (rec_G300 :: Pair -> Int)
                  (MkPair
                    ((case p of { MkPair (c :: Int) (d :: Int) -> c }) :: Int)
                    ((case p of { MkPair (c :: Int) (d :: Int) -> d }) :: Int))
              ; (r :: Int) = k_G12 a a
              } in r
        |])
        (tm "\\(p :: Pair) -> let { (r :: Int) = k_G12 r r } in r")
  , testCase "merges nested let-expressions, and moves their ticks to the bindings" $
      recTo
        (recTm [strQQ|
          \(x :: Int) (y :: Int) -> Core.srcSpan
            (let { !(a :: Int) = rec_G300 x y } in
              let { (b :: Int) = g_G10 a; (r :: Int) = k_G12 b a } in r)
        |])
        (recTm "\\(x :: Int) (y :: Int) -> let { (b :: Int) = Core.srcSpan (g_G10 r); (r :: Int) = Core.srcSpan (k_G12 b r) } in r")
  , testCase "doesn't fire without other bindings" $
      recKeeps [] (recTm "\\(x :: Int) (y :: Int) -> let { (r :: Int) = rec_G300 x y } in r")
  , testCase "doesn't fire on a term that is not in normal form" $
      recKeeps [] (recTm "\\(x :: Int) (y :: Int) -> k_G12 (rec_G300 x y) x")
  , testCase "doesn't fire outside the root" $
      assertNoRewriteAt env (recState "Int -> Int -> Int") [LamStep] recToLetRecS [varD "x_4" "Int"]
        (recTm "\\(y :: Int) -> let { (a :: Int) = rec_G300 x_4 y; (b :: Int) = g_G10 a; (r :: Int) = k_G12 b a } in r")
    -- Replacing @r@ by itself would leave it unbound
  , testCase "doesn't fire when the result is a recursive call" $
      recKeeps [] (recTm "\\(x :: Int) (y :: Int) -> let { (r :: Int) = rec_G300 x y; (b :: Int) = g_G10 r } in r")
  ]
