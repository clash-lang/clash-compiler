{-|
  Copyright   :  (C) 2026, QBayLogic B.V.
  License     :  BSD2 (see the file LICENSE)
  Maintainer  :  QBayLogic B.V. <devops@qbaylogic.com>

  Tests for "Clash.Normalize.Scoped.Transformations.Specialize": regression
  tests of the specializations and of 'appProp', and tests of the cases of the
  specializations. 'assertRewritesTo' and 'assertSpecializesTo' run every test
  in the scope of the free variables of its input, and in a scope that the
  binders of the input clash with.
-}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module Clash.Tests.Normalize.Scoped.Transformations.Specialize (tests) where

import qualified Control.Lens as Lens
import Data.Default (def)
import qualified Data.Map.Strict as Map

import Test.Tasty
import Test.Tasty.HUnit

import Clash.Core.Scoped.NameMap (GlobalEnv, globalEnvFromList, lookupGlobalEnv)
import Clash.Core.Scoped.Syntax
import Clash.Driver.Types (Binding (..))
import qualified Clash.Normalize.Scoped.Transformations.Cast as Scoped
import qualified Clash.Normalize.Scoped.Transformations.Inline as InlineS
import qualified Clash.Normalize.Scoped.Transformations.Specialize as Scoped
import Clash.Normalize.Types (NormalizeState, scopedSpecialisationCache)
import Clash.Rewrite.Scoped (ScopedRewrite, ScopedTransform (..))
import Clash.Rewrite.StrategyDSL.TH (asScopedRewriteQ)
import Clash.Rewrite.Types (RewriteEnv, RewriteState (..), extra)
import Clash.Util.Supply (newSupply)

import Test.Clash.Rewrite
  ( TestDefs (..), TestTerm, firstUniques, kindedTyVarD, mkGlobalBinding, parseTestTerm
  , parseTestTermWith, parseTestType, strQQ, testGlobalId, testTyConMap, tyVarD, withTyConMap
  , zeroWidthTyConMap )
import Test.Clash.Scoped
  ( ScopeDecl, Step (..), TestScope (..), assertLintClean, assertNoRewrite, assertRewritesTo
  , assertCaptureRewritesTo, assertSpecializesTo, assertTermAlphaEq, assertTermIs, closedTerm
  , newBindings, runScopedAt
  , runScopedIn, termIn, varD, withBinderClashes, withCurFun, withTestScope )
import Test.Clash.Scoped.Print (showTestTerm)

-- * The transformations

appPropS, constantSpecS, nonRepSpecS, typeSpecS, zeroWidthSpecS, argCastSpecS
  , specializeS :: ScopedRewrite NormalizeState
appPropS = $(asScopedRewriteQ Scoped.appProp)
constantSpecS = $(asScopedRewriteQ Scoped.constantSpec)
nonRepSpecS = $(asScopedRewriteQ Scoped.nonRepSpec)
typeSpecS = $(asScopedRewriteQ Scoped.typeSpec)
zeroWidthSpecS = $(asScopedRewriteQ Scoped.zeroWidthSpec)
argCastSpecS = $(asScopedRewriteQ Scoped.argCastSpec)
specializeS = ScopedTransform Scoped.specialize

-- * Fixtures

-- | Global functions with bindings: @f :: Int -> Int@, @fPoly :: forall b. b
-- -> Int -> Int@, @fT :: forall b. b -> b@, @fHo :: (Int -> Int) -> Int@,
-- @fPair :: Pair -> Int@; and global functions @g, k :: Int -> Int -> Int@,
-- of which only @k@ has a binding, and @kInt@, which is like @k@ but is an
-- internal function (one Clash created)
f, fPoly, fT, fHo, fPair, g, k, kInt :: GlobalId
f = testGlobalId "f_G10" "Int -> Int"
fPoly = testGlobalId "fPoly_G14" "forall (b :: Type). b -> Int -> Int"
fT = testGlobalId "fT_G16" "forall (b :: Type). b -> b"
fHo = testGlobalId "fHo_G17" "(Int -> Int) -> Int"
fPair = testGlobalId "fPair_G24" "Pair -> Int"
g = testGlobalId "g_G11" "Int -> Int -> Int"
k = testGlobalId "k_G18" "Int -> Int -> Int"
kInt = testGlobalId "kInt_IG25" "Int -> Int -> Int"

tm :: String -> TestTerm
tm = parseTestTermWith (TestDefs [] [] [f, fPoly, fT, fHo, fPair, g, k, kInt])

-- | The bindings of the global functions
bindings :: GlobalEnv (Binding (Term 'VoidS))
bindings = globalEnvFromList
  [ (gl, mkGlobalBinding gl (closedTerm body))
  | (gl, body) <-
      [ (f, "\\(p :: Int) -> p")
      , (fPoly, "\\(b :: Type) (y :: b) (p :: Int) -> p")
      , (fT, "\\(b :: Type) (y :: b) -> y")
      , (fHo, "\\(h :: Int -> Int) -> h 1")
      , (fPair, "\\(pr :: Pair) -> (case pr of { MkPair (p :: Int) (q :: Int) -> q }) :: Int")
      , (k, "\\(p :: Int) (q :: Int) -> q")
      , (kInt, "\\(p :: Int) (q :: Int) -> q") ] ]

-- | A state with 'bindings', in which a function @top@ is being normalized
st :: RewriteState NormalizeState
st = withCurFun (testGlobalId "top_G42" "Int -> Int") def {_bindings = bindings}

-- | The environment of the test types, including the zero-width types (see
-- 'zeroWidthTyConMap')
tcEnv :: RewriteEnv
tcEnv = withTyConMap zeroWidthTyConMap def

-- | A placeholder of a new global function, see 'assertSpecializesTo'
newFun :: String -> TestTerm -> (GlobalId, TestTerm)
newFun nm body = (testGlobalId (nm <> "_G9001") "Int", body)

-- | Assert that a transformation rewrites a term to the expected one, see
-- 'assertRewritesTo'
rewrites :: ScopedRewrite NormalizeState -> [ScopeDecl] -> TestTerm -> TestTerm -> Assertion
rewrites = assertRewritesTo tcEnv st

-- | Assert that a transformation creates a global function, and rewrites a
-- term to the expected one, see 'assertSpecializesTo'
specializes
  :: ScopedRewrite NormalizeState -> [ScopeDecl] -> TestTerm -> TestTerm -> String -> TestTerm
  -> Assertion
specializes rw decls input expected nm body =
  assertSpecializesTo tcEnv st rw decls input expected [newFun nm body]

-- | Assert that 'appProp' gives the expected term on a capture scenario, see
-- 'assertCaptureRewritesTo'
capturing
  :: [ScopeDecl] -> String -> [ScopeDecl]
  -> (forall l. Distinct l => TestScope l -> Term l -> Term l) -> String -> Assertion
capturing decls part new mk expected =
  assertCaptureRewritesTo tcEnv st appPropS decls (tm part) new mk (tm expected)

-- | Assert that a transformation leaves a term as it is
keeps :: ScopedRewrite NormalizeState -> [ScopeDecl] -> TestTerm -> Assertion
keeps = assertNoRewrite tcEnv st

-- | @\\(x :: ty) -> h x@, with @h :: ty -> Int@ a free local function
zwDecls :: String -> [ScopeDecl]
zwDecls ty = [varD "h_61" (parseTestType (ty <> " -> Int"))]

zwLam :: String -> TestTerm
zwLam ty = tm ("\\(x :: " <> ty <> ") -> h_61 x")

-- | 'zeroWidthSpec' rewrites 'zwLam' to the expected term
zwFires :: String -> String -> Assertion
zwFires ty expected = rewrites zeroWidthSpecS (zwDecls ty) (zwLam ty) (tm expected)

-- | 'zeroWidthSpec' doesn't fire on 'zwLam'
zwDoesNotFire :: String -> Assertion
zwDoesNotFire ty = keeps zeroWidthSpecS (zwDecls ty) (zwLam ty)

tests :: TestTree
tests =
  testGroup "Clash.Tests.Normalize.Scoped.Transformations.Specialize"
    [ testGroup "regression tests"
        [ testGroup "appProp" appPropTests
        , testGroup "specialize" specializeTests
        , testGroup "nonRepSpec" nonRepSpecTests
        ]
    , testGroup "specialize"
        [ testCase "f (g x y)" $
            specializes specializeS [varD "x_12" "Int", varD "y_13" "Int"]
              (tm "f_G10 (g_G11 x_12 y_13)")
              (tm "f_G9001 x_12 y_13")
              "f" (tm "\\(x :: Int) (y :: Int) -> (\\(p :: Int) -> p) (g_G11 x y)")
        , testCase "fPoly @a (x :: a) (g u v)" $
            specializes specializeS
              [tyVarD "a_50", varD "x_12" "a_50", varD "u_15" "Int", varD "v_16" "Int"]
              (tm "fPoly_G14 @a_50 x_12 (g_G11 u_15 v_16)")
              (tm "fPoly_G9001 @a_50 x_12 u_15 v_16")
              "fPoly"
              (tm "\\(b :: Type) (y :: b) (u :: Int) (v :: Int) -> (\\(b1 :: Type) (y2 :: b1) (p :: Int) -> p) @b y (g_G11 u v)")
        , testCase "f (case (x :: a) of {}), a :: kk" $
            specializes specializeS
              [tyVarD "kk_52", kindedTyVarD "a_50" "kk_52", varD "x_12" "a_50"]
              (tm "f_G10 ((case x_12 of {}) :: Int)")
              (tm "f_G9001 @kk_52 @a_50 x_12")
              "f" (tm "\\(kk :: Type) (a :: Core.Kind kk) (x :: a) -> (\\(p :: Int) -> p) ((case x of {}) :: Int)")
        , testCase "fT @Int" $
            specializes specializeS [] (tm "fT_G16 @Int") (tm "fT_G9001")
              "fT" (tm "(\\(b :: Type) (y :: b) -> y) @Int")
        , testCase "fHo (k x)" $
            specializes specializeS [varD "x_12" "Int"]
              (tm "fHo_G17 (k_G18 x_12)")
              (tm "fHok_G9001 x_12")
              "fHok" (tm "\\(x :: Int) -> (\\(h :: Int -> Int) -> h 1) ((\\(p :: Int) (q :: Int) -> q) x)")
        , testCase "fHo (g x)" $
            specializes specializeS [varD "x_12" "Int"]
              (tm "fHo_G17 (g_G11 x_12)")
              (tm "fHog_G9001 x_12")
              "fHog" (tm "\\(x :: Int) -> (\\(h :: Int -> Int) -> h 1) (g_G11 x)")
        , testCase "(\\z -> z) (g x y)" $
            specializes specializeS [varD "x_12" "Int", varD "y_13" "Int"]
              (tm "(\\(z :: Int) -> z) (g_G11 x_12 y_13)")
              (tm "(\\(z :: Int) -> z) (ctopspecF_G9001 x_12 y_13)")
              "ctopspecF" (tm "\\(x :: Int) (y :: Int) -> g_G11 x y")
        ]
    , testGroup "appProp"
        [ testCase "lambdas, work-free and not work-free arguments" $
            rewrites appPropS [varD "x_12" "Int", varD "y_13" "Int", varD "fl_31" "Int -> Int"]
              (tm "(\\(z :: Int) (w :: Int) -> g_G11 z w) x_12 (fl_31 y_13)")
              (tm "let { !(w :: Int) = fl_31 y_13 } in g_G11 x_12 w")
        , testCase "type lambda" $
            rewrites appPropS [varD "x_12" "Int"]
              (tm "(\\(a :: Type) (x :: a) -> x) @Int x_12")
              (tm "x_12")
        , testCase "let-expression" $
            rewrites appPropS [varD "x_12" "Int", varD "y_13" "Int", varD "fl_31" "Int -> Int"]
              (tm "(let { !(z :: Int) = fl_31 x_12 } in \\(w :: Int) -> g_G11 z w) y_13")
              (tm "let { !(z :: Int) = fl_31 x_12 } in g_G11 z y_13")
        , testCase "ticks" $
            rewrites appPropS [varD "x_12" "Int"]
              (tm "Core.srcSpan (\\(z :: Int) -> z) x_12")
              (tm "Core.srcSpan x_12")
        , testCase "case-expression, an argument that isn't work free" $
            rewrites appPropS [varD "x_12" "Int", varD "s_28" "Pair", varD "fl_31" "Int -> Int"]
              (tm "((case s_28 of { MkPair (a :: Int) (b :: Int) -> \\(z :: Int) -> g_G11 a z }) :: Int -> Int) (fl_31 x_12)")
              (tm "let { !(arg :: Int) = fl_31 x_12 } in (case s_28 of { MkPair (a :: Int) (b :: Int) -> g_G11 a arg }) :: Int")
        , testCase "case-expression, a type argument and a work-free argument" $
            rewrites appPropS [varD "x_12" "Int", varD "s_28" "Pair"]
              (tm [strQQ|
                ((case s_28 of { MkPair (a :: Int) (b :: Int) -> \(c :: Type) (x :: c) -> x })
                  :: forall (c :: Type). c -> c) @Int x_12
              |])
              (tm "(case s_28 of { MkPair (a :: Int) (b :: Int) -> x_12 }) :: Int")
        , testCase "doesn't fire on a variable applied to arguments" $
            keeps appPropS [varD "x_12" "Int", varD "fl_31" "Int -> Int"] (tm "fl_31 x_12")
        ]
    , testGroup "constantSpec"
        [ testCase "f 3: a constant argument" $
            specializes constantSpecS [] (tm "f_G10 3") (tm "f_G9001")
              "f" (tm "(\\(p :: Int) -> p) 3")
        , testCase "fPair (MkPair x 2): a partially constant argument" $
            specializes constantSpecS [varD "x_12" "Int"]
              (tm "fPair_G24 (MkPair x_12 2)")
              (tm "let { !(csr :: Int) = x_12 } in fPair_G9001 csr")
              "fPair"
              (tm [strQQ|
                \(csr :: Int) ->
                  (\(pr :: Pair) -> (case pr of { MkPair (p :: Int) (q :: Int) -> q }) :: Int) (MkPair csr 2)
              |])
        , testCase "fPair (MkPair (fl x) (fl y)): no constant parts" $
            keeps constantSpecS [varD "x_12" "Int", varD "y_13" "Int", varD "fl_31" "Int -> Int"]
              (tm "fPair_G24 (MkPair (fl_31 x_12) (fl_31 y_13))")
        , testCase "fl 3: a local function" $
            keeps constantSpecS [varD "fl_31" "Int -> Int"] (tm "fl_31 3")
        , testCase "fPoly @Int 3 4: a type argument" $
            keeps constantSpecS [] (tm "fPoly_G14 @Int 3 4")
        ]
    , testGroup "typeSpec"
        [ testCase "fT @Int" $
            specializes typeSpecS [] (tm "fT_G16 @Int") (tm "fT_G9001")
              "fT" (tm "(\\(b :: Type) (y :: b) -> y) @Int")
        , testCase "fT @a: a free type variable" $
            keeps typeSpecS [tyVarD "a_50"] (tm "fT_G16 @a_50")
        , testCase "fT @Pair" $
            specializes typeSpecS [] (tm "fT_G16 @Pair") (tm "fT_G9001")
              "fT" (tm "(\\(b :: Type) (y :: b) -> y) @Pair")
        ]
    , testGroup "nonRepSpec"
        [ testCase "fHo (g x)" $
            specializes nonRepSpecS [varD "x_12" "Int"]
              (tm "fHo_G17 (g_G11 x_12)")
              (tm "fHog_G9001 x_12")
              "fHog" (tm "\\(x :: Int) -> (\\(h :: Int -> Int) -> h 1) (g_G11 x)")
        , testCase "fHo (kInt x): an internal function is inlined" $
            specializes nonRepSpecS [varD "x_12" "Int"]
              (tm "fHo_G17 (kInt_IG25 x_12)")
              (tm "fHo_G9001")
              "fHo" (tm "(\\(h :: Int -> Int) -> h 1) (\\(q :: Int) -> q)")
        , testCase "fHo h: a local variable" $
            keeps nonRepSpecS [varD "hl_32" "Int -> Int"] (tm "fHo_G17 hl_32")
        , testCase "f x: a representable argument" $
            keeps nonRepSpecS [varD "x_12" "Int"] (tm "f_G10 x_12")
        ]
    , testGroup "zeroWidthSpec"
        [ testCase "BitVector 0" $
            zwFires "Clash.Sized.Internal.BitVector.BitVector 0"
              "\\(x :: Clash.Sized.Internal.BitVector.BitVector 0) -> h_61 (Clash.Sized.Internal.BitVector.fromInteger# @0 (0 :: Natural) (0 :: Natural) (0 :: Integer))"
        , testCase "Unsigned 0" $
            zwFires "Clash.Sized.Internal.Unsigned.Unsigned 0"
              "\\(x :: Clash.Sized.Internal.Unsigned.Unsigned 0) -> h_61 (Clash.Sized.Internal.Unsigned.fromInteger# @0 (0 :: Natural) (0 :: Integer))"
        , testCase "Signed 0" $
            zwFires "Clash.Sized.Internal.Signed.Signed 0"
              "\\(x :: Clash.Sized.Internal.Signed.Signed 0) -> h_61 (Clash.Sized.Internal.Signed.fromInteger# @0 (0 :: Natural) (0 :: Integer))"
        , testCase "Index 1" $
            zwFires "Clash.Sized.Internal.Index.Index 1"
              "\\(x :: Clash.Sized.Internal.Index.Index 1) -> h_61 (Clash.Sized.Internal.Index.fromInteger# @1 (1 :: Natural) (0 :: Integer))"
        , testCase "a product of zero-width types" $
            zwFires "ZW_3104" $
              "\\(x :: ZW_3104) -> h_61 (MkZW (Clash.Sized.Internal.BitVector.fromInteger# @0 (0 :: Natural) (0 :: Natural) (0 :: Integer))"
                <> " (Clash.Sized.Internal.Unsigned.fromInteger# @0 (0 :: Natural) (0 :: Integer)))"
        , testCase "a field type that mentions a type argument" $
            zwFires "TProxy 3" "\\(x :: TProxy 3) -> h_61 (MkTProxy @3 (Proxy @3))"
        , testCase "a type argument" $
            zwFires "Proxy 3" "\\(x :: Proxy 3) -> h_61 (Proxy @3)"
        , testCase "BitVector 1" $ zwDoesNotFire "Clash.Sized.Internal.BitVector.BitVector 1"
        , testCase "Index 2" $ zwDoesNotFire "Clash.Sized.Internal.Index.Index 2"
        , testCase "Int" $ zwDoesNotFire "Int"
        , testCase "a constructor with an existential type variable" $ zwDoesNotFire "ExZW_3107"
        , testCase "a recursive type" $
            zwDoesNotFire "Stream (Clash.Sized.Internal.BitVector.BitVector 0)"
        ]
    , testGroup "argCastSpec"
        [ testCase "fPair (cast x)" $
            specializes argCastSpecS [varD "x_12" "Int"]
              (tm "fPair_G24 (Core.cast @Int @Pair x_12)")
              (tm "fPair_G9001 x_12")
              "fPair"
              (tm "\\(x :: Int) -> (\\(pr :: Pair) -> (case pr of { MkPair (p :: Int) (q :: Int) -> q }) :: Int) (Core.cast @Int @Pair x)")
        , testCase "fPair (cast (fl x)): a cast that isn't work free" $
            specializes argCastSpecS [varD "x_12" "Int", varD "fl_31" "Int -> Int"]
              (tm "fPair_G24 (Core.cast @Int @Pair (fl_31 x_12))")
              (tm "fPair_G9001 fl_31 x_12")
              "fPair"
              (tm [strQQ|
                \(fl :: Int -> Int) (x :: Int) ->
                  (\(pr :: Pair) -> (case pr of { MkPair (p :: Int) (q :: Int) -> q }) :: Int) (Core.cast @Int @Pair (fl x))
              |])
        , testCase "fPair <tick> (cast x)" $
            specializes argCastSpecS [varD "x_12" "Int"]
              (tm "fPair_G24 (Core.srcSpan (Core.cast @Int @Pair x_12))")
              (tm "fPair_G9001 x_12")
              "fPair"
              (tm [strQQ|
                \(x :: Int) ->
                  (\(pr :: Pair) -> (case pr of { MkPair (p :: Int) (q :: Int) -> q }) :: Int)
                    (Core.srcSpan (Core.cast @Int @Pair x))
              |])
        , testCase "fPoly @Pair y (cast x): type arguments" $
            specializes argCastSpecS [varD "x_12" "Int", varD "yp_34" "Pair"]
              (tm "fPoly_G14 @Pair yp_34 (Core.cast @Pair @Int x_12)")
              (tm "fPoly_G9001 @Pair yp_34 x_12")
              "fPoly"
              (tm "\\(b :: Type) (y :: b) (x :: Int) -> (\\(b1 :: Type) (y2 :: b1) (p :: Int) -> p) @b y (Core.cast @Pair @Int x)")
        , testCase "fPair (cast (cast x)): a cast of a cast" $
            keeps argCastSpecS [varD "x_12" "Int"]
              (tm "fPair_G24 (Core.cast @Int @Pair (Core.cast @Int @Int x_12))")
        , testCase "pl (cast x): a local function" $
            keeps argCastSpecS [varD "x_12" "Int", varD "pl_33" "Pair -> Int"]
              (tm "pl_33 (Core.cast @Int @Pair x_12)")
        ]
    ]

-- * The regression tests of appProp
--
-- Up to #1040, 'appProp' relied on its input being deshadowed. Several passes
-- broke that invariant: the flattening stage (fixed in #492 by deshadowing
-- there), and 'bindConstantVar' (#1035). #1040 made 'appProp' deshadow the
-- function part of the application itself, and removed the deshadowing from
-- the flattening stage and the inlining transformations again. Each test runs
-- 'appProp' on a term in which a binder in the function part has the name of
-- a free variable of an argument, and 'assertRewritesTo' also runs it in a
-- scope that the binders of the input clash with.

-- | Run the scoped 'Clash.Normalize.Scoped.Transformations.Inline.bindConstantVar'
-- on @let a = .. in \\x -> e@, and 'appProp' on the body of the resulting
-- lambda. Asserts that this gives the expected term, also in a scope that the
-- binders of the input clash with.
bindConstantVarThenAppProp :: [ScopeDecl] -> TestTerm -> TestTerm -> Assertion
bindConstantVarThenAppProp decls input expected =
  withTestScope decls $ \ts0 -> do
    let e0 = termIn ts0 input
    check ts0 e0
    withBinderClashes ts0 e0 check
 where
  check :: Distinct n => TestScope n -> Term n -> Assertion
  check ts e = do
    -- 'bindConstantVar' looks at the function being normalized
    let st0 = withCurFun (testGlobalId "top_G200" "Int") def
    (inlined, _, _) <- runScopedIn def st0 ts $(asScopedRewriteQ InlineS.bindConstantVar) e
    (e', _, _) <- runScopedAt def st0 ts [LamStep] appPropS inlined
    assertLintClean ts e'
    assertTermIs ts expected e'

appPropTests :: [TestTree]
appPropTests =
  [ -- The arguments are moved into the body of a let-expression in the
    -- function part, so its binders must not capture their free variables:
    --
    -- > (let x = w in \y -> y) x  ==>  let x' = w in x
    --
    -- Without deshadowing, the let-binding captures the argument:
    -- @let x = w in x@.
    -- https://github.com/clash-lang/clash-compiler/pull/492
    -- https://github.com/clash-lang/clash-compiler/pull/1040
    testCase "appProp deshadows let-bindings in the function part (#492, #1040)" $ do
      rewrites appPropS [varD "x_1" "Int", varD "w_3" "Int"]
        (tm "(let { (x :: Int) = w_3 } in \\(y :: Int) -> y) x_1")
        (tm "let { (x :: Int) = w_3 } in x_1")
      -- The let-binder @x@ has the name of the argument @x@
      capturing [varD "w_3" "Int"] "let { (x :: Int) = w_3 } in \\(y :: Int) -> y" [varD "x_1" "Int"]
        (\ts fn -> App fn (termIn ts (tm "x_1")))
        "let { (x :: Int) = w_3 } in x_1"
    -- A lambda applied to an argument that performs work becomes a
    -- let-binding, so its binder must not capture the free variables of the
    -- remaining arguments:
    --
    -- > (\x -> \y -> y) (f x) (f x)  ==>  let x' = f x in let y = f x in y
    --
    -- https://github.com/clash-lang/clash-compiler/pull/1040
  , testCase "appProp let-binds lambda arguments without capture (#1040)" $ do
      rewrites appPropS [varD "x_1" "Int", varD "f_3" "Int -> Int"]
        (tm "(\\(x :: Int) (y :: Int) -> y) (f_3 x_1) (f_3 x_1)")
        (tm "let { !(x :: Int) = f_3 x_1; !(y :: Int) = f_3 x_1 } in y")
      -- The lambda binder @x@ has the name of the @x@ of the arguments
      capturing [varD "f_3" "Int -> Int"] "\\(x :: Int) (y :: Int) -> y" [varD "x_1" "Int"]
        (\ts fn -> App (App fn (termIn ts (tm "f_3 x_1"))) (termIn ts (tm "f_3 x_1")))
        "let { !(x :: Int) = f_3 x_1; !(y :: Int) = f_3 x_1 } in y"
    -- Arguments are pushed into case alternatives, so pattern binders must
    -- not capture their free variables:
    --
    -- > (case s of MkPair a b -> \k -> k) b  ==>  case s of MkPair a b' -> b
    --
    -- https://github.com/clash-lang/clash-compiler/pull/1040
  , testCase "appProp pushes arguments into alternatives without capture (#1040)" $ do
      rewrites appPropS [varD "b_2" "Int", varD "s_5" "Pair"]
        (tm "((case s_5 of { MkPair (a :: Int) (b :: Int) -> \\(k :: Int) -> k }) :: Int -> Int) b_2")
        (tm "(case s_5 of { MkPair (a :: Int) (b :: Int) -> b_2 }) :: Int")
      -- The pattern binder @b@ has the name of the argument @b@
      capturing [varD "s_5" "Pair"]
        "(case s_5 of { MkPair (a :: Int) (b :: Int) -> \\(k :: Int) -> k }) :: Int -> Int"
        [varD "a2" "Int", varD "b_2" "Int"]
        (\ts fn -> App fn (termIn ts (tm "b_2")))
        "(case s_5 of { MkPair (a :: Int) (b :: Int) -> b_2 }) :: Int"
    -- Issue #1035: 'bindConstantVar' inlines a let-bound lambda without
    -- deshadowing it, so the result shadows:
    --
    -- > let a = \k x -> k in \x -> a x p  ==>  \x -> (\k x -> k) x p
    --
    -- This broke the no-shadowing invariant 'appProp' relied on. #1036 (and
    -- #1037, which absorbed it) made substitution deshadow, but were closed in
    -- favour of #1040, which made 'appProp' deshadow instead. 'appProp' should
    -- reduce the body of the resulting lambda to the outer @x@, not to @p@.
    --
    -- https://github.com/clash-lang/clash-compiler/issues/1035
  , testCase "appProp handles shadowing introduced by bindConstantVar (#1035)" $
      bindConstantVarThenAppProp [varD "p_5" "Int"]
        (tm "let { !(a :: Int -> Int -> Int) = \\(k :: Int) (x :: Int) -> k } in \\(x :: Int) -> a x p_5")
        (tm "\\(x :: Int) -> x")
    -- Issue #990: inlining let-bindings ('inlineBinders', used by
    -- 'bindConstantVar') introduced shadowing, after which 'appProp'
    -- let-bound an argument with a binder that captured the free variables of
    -- the other arguments. The resulting HDL contained a signal assigned to
    -- itself. #991 deshadowed after inlining let-bindings; #1040 replaced that
    -- by deshadowing in 'appProp'.
    --
    -- > let a = \x y -> y in \x -> a (f x) (f x)
    -- >   ==> (bindConstantVar)  \x -> (\x y -> y) (f x) (f x)
    -- >   ==> (appProp)          \x -> let x' = f x in let y = f x in y
    --
    -- https://github.com/clash-lang/clash-compiler/issues/990
    -- https://github.com/clash-lang/clash-compiler/pull/991
  , testCase "appProp does not let-bind a captured argument after inlining (#991)" $
      bindConstantVarThenAppProp [varD "f_3" "Int -> Int"]
        (tm "let { !(a :: Int -> Int -> Int) = \\(x :: Int) (y :: Int) -> y } in \\(x :: Int) -> a (f_3 x) (f_3 x)")
        (tm "\\(x :: Int) -> let { !(x2 :: Int) = f_3 x; !(y :: Int) = f_3 x } in y")
    -- @(\\f -> f) (\\k x -> k) x y@ reduces to the free variable @x@. The
    -- first argument binds @x@, which is also free in the second argument.
    -- 'appProp' substituted work-free arguments without avoiding capture:
    -- after substituting the first argument for @f@, that argument is the
    -- head of the application, and substituting @x@ for @k@ puts it under the
    -- binder @x@.
    --
    -- https://github.com/clash-lang/clash-compiler/pull/3479
  , testCase "appProp does not capture free variables (#3479)" $ do
      rewrites appPropS [varD "x_2" "Int", varD "y_5" "Int"]
        (tm "(\\(f :: Int) -> f) (\\(k :: Int) (x :: Int) -> k) x_2 y_5")
        (tm "x_2")
      -- The binder @x@ of the first argument has the name of the second
      -- argument @x@
      capturing [varD "y_5" "Int"] "\\(k :: Int) (x :: Int) -> k" [varD "k2" "Int", varD "x_2" "Int"]
        (\ts arg -> App (App (App (termIn ts (tm "\\(f :: Int) -> f")) arg) (termIn ts (tm "x_2"))) (termIn ts (tm "y_5")))
        "x_2"
    -- 'appProp' substitutes work-free arguments into the body of the lambda it
    -- applies them to. Such an argument can then end up as the function part
    -- of the application, but unlike the original function part, it was never
    -- deshadowed. Its binders can then capture the free variables of the
    -- remaining arguments, which 'appProp' moves under them. The tests below
    -- apply @\\f -> f@ to a work-free function that binds @x@, followed by
    -- arguments in which @x@ is free.
    --
    -- https://github.com/clash-lang/clash-compiler/pull/3479
    --
    -- Lambda path: the binder @x@ of the argument becomes a let-binder for
    -- the argument @g x@, which performs work:
    --
    -- > (\f -> f) (\x k -> k) (g x) x  ==>  let x' = g x in x
  , testCase "appProp deshadows substituted arguments, lambda" $
      rewrites appPropS [varD "g_4" "Int -> Int", varD "x_7" "Int"]
        (tm "(\\(f :: Int -> Int -> Int) -> f) (\\(x :: Int) (k :: Int) -> k) (g_4 x_7) x_7")
        (tm "let { !(x :: Int) = g_4 x_7 } in x_7")
    -- Let path:
    --
    -- > (\f -> f) (let x = w in \k -> k) x  ==>  let x' = w in x
  , testCase "appProp deshadows substituted arguments, let" $
      rewrites appPropS [varD "x_7" "Int", varD "w_8" "Int"]
        (tm "(\\(f :: Int -> Int) -> f) (let { !(x :: Int) = w_8 } in \\(k :: Int) -> k) x_7")
        (tm "let { !(x :: Int) = w_8 } in x_7")
    -- Case path:
    --
    -- > (\f -> f) (case s of MkPair a b -> \k -> k) b  ==>  case s of MkPair a b' -> b
  , testCase "appProp deshadows substituted arguments, case" $
      rewrites appPropS [varD "b_2" "Int", varD "s_5" "Pair"]
        (tm "(\\(f :: Int -> Int) -> f) ((case s_5 of { MkPair (a :: Int) (b :: Int) -> \\(k :: Int) -> k }) :: Int -> Int) b_2")
        (tm "(case s_5 of { MkPair (a :: Int) (b :: Int) -> b_2 }) :: Int")
    -- 'bindConstantVar' inlines let-bound lambdas without deshadowing them,
    -- so it creates such arguments:
    --
    -- > let a = \x k -> k in \x -> (\f -> f) a (g x) x
    -- >   ==> (bindConstantVar)  \x -> (\f -> f) (\x k -> k) (g x) x
    -- >   ==> (appProp)          \x -> let x' = g x in x
  , testCase "appProp deshadows substituted arguments after bindConstantVar" $
      bindConstantVarThenAppProp [varD "g_4" "Int -> Int"]
        (tm [strQQ|
          let { !(a :: Int -> Int -> Int) = \(x :: Int) (k :: Int) -> k } in
            \(x :: Int) -> (\(f :: Int -> Int -> Int) -> f) a (g_4 x) x
        |])
        (tm "\\(x :: Int) -> let { !(x2 :: Int) = g_4 x } in x")
  ]

-- * The regression tests of specialize

-- | Global function @f :: Int -> Int@ with body @\\a -> a@, and its binding
fBinding :: GlobalEnv (Binding (Term 'VoidS))
fBinding = globalEnvFromList [(f, mkGlobalBinding f (closedTerm "\\(a :: Int) -> a"))]

-- | A state with the given bindings
stWith :: GlobalEnv (Binding (Term 'VoidS)) -> RewriteState NormalizeState
stWith bs = def {_bindings = bs}

-- | Assert that 'specialize' specializes @f@ on @g x y@, in a state with the
-- given bindings
specializeF :: GlobalEnv (Binding (Term 'VoidS)) -> Assertion
specializeF bs =
  assertSpecializesTo def (stWith bs) specializeS [varD "x_12" "Int", varD "y_13" "Int"]
    (tm "f_G10 (g_G11 x_12 y_13)")
    (tm "f_G9001 x_12 y_13")
    [newFun "f" (tm "\\(x :: Int) (y :: Int) -> (\\(a :: Int) -> a) (g_G11 x y)")]

-- | Global function @fHo :: (Int -> Int) -> Int@ with body @\\h -> h 1@, and a
-- global function @k@ of the given name sort with body @\\p q -> q@
fHoAndK :: String -> GlobalEnv (Binding (Term 'VoidS))
fHoAndK kIdent = globalEnvFromList
  [ (fHo30, mkGlobalBinding fHo30 (closedTerm "\\(h :: Int -> Int) -> h 1"))
  , (kGlobal, mkGlobalBinding kGlobal (closedTerm "\\(p :: Int) (q :: Int) -> q")) ]
 where
  fHo30 = testGlobalId "fHo_G30" "(Int -> Int) -> Int"
  kGlobal = testGlobalId kIdent "Int -> Int -> Int"

-- | Run a transformation on @fHo (k x)@, where @k@ is a local function that
-- shares its unique with the global function @k@, given the identifier of
-- that global. It abstracts over the local @k@ rather than inlining the body
-- of the global @k@.
specializeOnLocalArgHead :: ScopedRewrite NormalizeState -> String -> Assertion
specializeOnLocalArgHead rw kIdent =
  assertSpecializesTo def (stWith (fHoAndK kIdent)) rw
    [varD "x_12" "Int", varD "k_31" "Int -> Int -> Int"]
    (tm "(fHo_G30 :: (Int -> Int) -> Int) (k_31 x_12)")
    (tm "fHok_G9001 k_31 x_12")
    [newFun "fHok" (tm "\\(k :: Int -> Int -> Int) (x :: Int) -> (\\(h :: Int -> Int) -> h 1) (k x)")]

specializeTests :: [TestTree]
specializeTests =
  [ -- 'specArgBndrsAndVars' used to determine whether a free variable of the
    -- argument specialized on is global by looking up its unique in the
    -- global bindings. A local variable with the same unique as a global
    -- binder was therefore not abstracted over, and ended up free in the
    -- specialized function. Clash creates such variables when it turns a
    -- group of mutually recursive global binders into a let-expression. Here
    -- the local @x@ has the unique of the global @x@.
    --
    -- https://github.com/clash-lang/clash-compiler/pull/712
    testCase "specialize abstracts over locals that shadow a global (#712)" $
      specializeF (fBinding <> globalEnvFromList [(xGlobal, mkGlobalBinding xGlobal (closedTerm "1"))])

    -- 'specialize' creates binders for the arguments that precede the one it
    -- specializes on. It used to give them the types of those arguments, which
    -- can mention type variables bound at the call site. Specializing
    -- @f \@a (x :: a) (g u v)@, with @f :: forall b. b -> Int -> Int@, created:
    --
    -- > f_g = /\b' -> \(x' :: a) (u' :: Int) (v' :: Int) -> f b' x' (g u' v')
    --
    -- in which @a@ is free. 'argCastSpec' passes applications with type
    -- arguments to 'specialize', for example under a case alternative binding
    -- an existential type variable.
  , testCase "specialize gives argument binders the types of the parameters" $
      assertSpecializesTo def
        (stWith (globalEnvFromList
          [(fPoly, mkGlobalBinding fPoly (closedTerm "\\(b :: Type) (y :: b) (z :: Int) -> z"))]))
        specializeS
        [tyVarD "a_50", varD "x_12" "a_50", varD "u_15" "Int", varD "v_16" "Int"]
        (tm "fPoly_G14 @a_50 x_12 (g_G11 u_15 v_16)")
        (tm "fPoly_G9001 @a_50 x_12 u_15 v_16")
        [newFun "fPoly"
          (tm "\\(b :: Type) (y :: b) (u :: Int) (v :: Int) -> (\\(b1 :: Type) (y2 :: b1) (z :: Int) -> z) @b y (g_G11 u v)")]

    -- 'specArgBndrsAndVars' used to collect the free variables of the
    -- argument specialized on in a set ordered by unique. Specializing
    -- @f (g x_123 y_456)@ and @f (g x_456 y_123)@ then yielded the binders
    -- @[x, y]@ and @[y, x]@, so the second specialization missed the
    -- specialization cache.
    --
    -- https://github.com/clash-lang/clash-compiler/pull/1087
  , testCase "specialize orders free variables by occurrence (#1087)" $ do
      let st0 = stWith fBinding
      (r1, st1) <- withTestScope [varD "x_123" "Int", varD "y_456" "Int"] $ \ts -> do
        (e, st1, _) <- runScopedIn def st0 ts specializeS (termIn ts (tm "f_G10 (g_G11 x_123 y_456)"))
        pure (headGlobal e, st1)
      withTestScope [varD "y_123" "Int", varD "x_456" "Int"] $ \ts -> do
        (e, st2, _) <- runScopedIn def st1 ts specializeS (termIn ts (tm "f_G10 (g_G11 x_456 y_123)"))
        assertEqual "Number of specializations" 1
          (Map.size (Lens.view (extra . scopedSpecialisationCache) st2))
        case r1 of
          Just g1 -> assertTermIs ts (tm (showTestTerm (GlobalVar @'VoidS g1) <> " x_456 y_123")) e
          Nothing -> assertFailure "the first specialization didn't give an application of a global"

    -- 'specialize' abstracts the specialized function over the free type
    -- variables of the argument it specializes on. 'specArgBndrsAndVars' used
    -- to order them by occurrence, without regard for their kinds.
    -- Specializing @f (case (x :: a) of {})@, with @a :: k@, created:
    --
    -- > f_spec = /\(a :: k) -> /\k -> \(x :: a) -> f (case x of {})
    --
    -- in which the @k@ in the kind of @a@ is free.
  , testCase "specialize orders type variables after the variables in their kinds" $
      assertSpecializesTo def (stWith fBinding) specializeS
        [tyVarD "k_51", kindedTyVarD "a_50" "k_51", varD "x_12" "a_50"]
        (tm "f_G10 ((case x_12 of {}) :: Int)")
        (tm "f_G9001 @k_51 @a_50 x_12")
        [newFun "f" (tm "\\(k :: Type) (a :: Core.Kind k) (x :: a) -> (\\(b :: Int) -> b) ((case x of {}) :: Int)")]

    -- 'mkFunction', used by 'specialize' to create the specialized function,
    -- used to give the new function a fresh unique without checking whether a
    -- global binder with that unique already exists, overwriting it.
    --
    -- https://github.com/clash-lang/clash-compiler/pull/790
  , testCase "specialize does not overwrite existing binders (#790)" $ do
      supply <- newSupply
      let -- Global binders occupying the uniques the supply hands out first
          victims =
            [ (testGlobalId ("victim_G" <> show u) "Int", closedTerm (parseTestTerm (show n)))
            | (n, u) <- zip [0 :: Int ..] (firstUniques 8 supply) ]
          bs = fBinding <> globalEnvFromList [(v, mkGlobalBinding v t) | (v, t) <- victims]
          st0 = (stWith bs) {_uniqSupply = supply}
      withTestScope [varD "x_12" "Int", varD "y_13" "Int"] $ \ts -> do
        (_, st1, _) <- runScopedIn def st0 ts specializeS (termIn ts (tm "f_G10 (g_G11 x_12 y_13)"))
        assertEqual "new global bindings" 1 (length (newBindings st0 st1))
        sequence_
          [ case lookupGlobalEnv v (_bindings st1) of
              Just b | bindingId b == v -> assertTermAlphaEq t (bindingTerm b)
              _ -> assertFailure ("Binder " <> showTestTerm (GlobalVar @'VoidS v) <> " was overwritten")
          | (v, t) <- victims ]

    -- The binding map and the specialisation history are keyed on uniques
    -- alone, but a local variable can have the same unique as a global
    -- binder. Clash creates such variables when it turns a group of mutually
    -- recursive global binders into a let-expression, and new local and
    -- global binders are made unique with respect to their own namespace
    -- only. 'specialize' used to look up the head of an application, and the
    -- head of the argument specialized on, in the binding map regardless of
    -- whether it was a global.
    --
    -- Specialize a local function that shares its unique with a global
    -- function. There is nothing to specialize, but 'specialize' used to
    -- create a new function from the body of the global.
  , testCase "specialize does not specialize a local head that shares a unique with a global" $
      assertNoRewrite def (stWith fBinding) specializeS
        [varD "f_10" "Int -> Int", varD "x_12" "Int", varD "y_13" "Int"]
        (tm "f_10 (g_G11 x_12 y_13)")
  , testCase "specialize does not inline a global into a local argument head that shares its unique" $
      specializeOnLocalArgHead specializeS "k_G31"

    -- 'specialize' creates a binder for every argument before the one it
    -- specializes on, and inferred the kind of a type argument with
    -- 'Clash.Core.Scoped.HasType.inferCoreKindOf' to do so. With the @debug@
    -- flag, that errored for applications of type constructors, such as
    -- @Maybe a@ in
    --
    -- > h @(Maybe a) f
  , testCase "specialize binds a type argument applying a type constructor" $
      assertSpecializesTo (withTyConMap testTyConMap def)
        (stWith (fBinding <> globalEnvFromList
          [(h, mkGlobalBinding h (closedTerm "\\(b :: Type) (k :: Int -> Int) -> k 1"))]))
        specializeS
        [tyVarD "a_30"]
        (tm "(h_G33 :: forall (b :: Type). (Int -> Int) -> Int) @(Maybe a_30) f_G10")
        (tm "hf_G9001 @(Maybe a_30)")
        [newFun "hf" (tm "\\(b :: Type) -> (\\(b1 :: Type) (k :: Int -> Int) -> k 1) @b (\\(a :: Int) -> a)")]
  ]
 where
  xGlobal = testGlobalId "x_G12" "Int"
  h = testGlobalId "h_G33" "forall (b :: Type). (Int -> Int) -> Int"
  headGlobal e = case e of
    App e1 _ -> headGlobal e1
    TyApp e1 _ -> headGlobal e1
    GlobalVar gl -> Just gl
    _ -> Nothing

nonRepSpecTests :: [TestTree]
nonRepSpecTests =
  [ testCase "nonRepSpec does not inline a global into a local argument head that shares its unique" $
      specializeOnLocalArgHead nonRepSpecS "k_IG31"
  ]
