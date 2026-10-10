{-|
  Copyright   :  (C) 2026, QBayLogic B.V.
  License     :  BSD2 (see the file LICENSE)
  Maintainer  :  QBayLogic B.V. <devops@qbaylogic.com>

  Tests for "Clash.Normalize.Transformations.Specialize"
-}

{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Clash.Tests.Normalize.Transformations.Specialize (tests) where

import Data.Default (def)

import Test.Tasty
import Test.Tasty.HUnit

import Clash.Core.Name (NameSort (User), noSrcSpan)
import Clash.Core.Term (Bind (..), Pat (..), Term (..), mkApps)
import Clash.Core.Var (Id)
import Clash.Normalize.Transformations.Inline (bindConstantVar)
import Clash.Normalize.Transformations.Specialize (appProp)
import Clash.Normalize.Types (NormRewrite)
import Clash.Rewrite.StrategyDSL.TH (asRewriteQ)
import Clash.Rewrite.Types (RewriteState (..))

import Test.Clash.Rewrite
  ( assertAlphaEq, globalId, inScopeOf, intFunTy, intId, intTy, localId
  , pairDataCon, pairTy, parseToTerm, parseToTermQQ, runSingleTransformation
  , showPprU )

appPropR :: NormRewrite
appPropR = $(asRewriteQ appProp)

bindConstantVarR :: NormRewrite
bindConstantVarR = $(asRewriteQ bindConstantVar)

-- | Run 'appProp' on a term, with the given variables in scope
runAppProp :: [Id] -> Term -> IO Term
runAppProp inScope = runSingleTransformation def def (inScopeOf inScope) appPropR

-- | Run 'bindConstantVar' on @let a = .. in \\x -> e@, and 'appProp' on the
-- body of the resulting lambda
bindConstantVarThenAppProp :: [Id] -> Term -> IO Term
bindConstantVarThenAppProp inScope term = do
  inlined <- runSingleTransformation def st (inScopeOf inScope) bindConstantVarR term
  case inlined of
    Lam x body -> runAppProp (x : inScope) body
    _ -> assertFailure ("Unexpected result of bindConstantVar: " <> showPprU inlined)
 where
  -- 'bindConstantVar' looks at the function being normalized
  st = def { _curFun = (globalId User "top" 200 intTy, noSrcSpan) }

-- * appProp (#492, #1040, #1035, #3479)
--
-- Up to #1040, 'appProp' relied on its input being deshadowed. Several passes
-- broke that invariant: the flattening stage (fixed in #492 by deshadowing
-- there), and 'bindConstantVar' (#1035). #1040 made 'appProp' deshadow the
-- function part of the application itself, see Note [AppProp no shadowing],
-- and removed the deshadowing from the flattening stage and the inlining
-- transformations again. The tests below run 'appProp' on terms in which a
-- binder in the function part has the same unique as a free variable of an
-- argument.

-- | Case 3 of Note [AppProp no shadowing]:
--
-- > (let x = w in \y -> y) x  ==>  let x' = w in x
--
-- Without deshadowing, the let-binding captures the argument: @let x = w in x@.
-- https://github.com/clash-lang/clash-compiler/pull/492
-- https://github.com/clash-lang/clash-compiler/pull/1040
appPropLetCapture :: IO Term
appPropLetCapture = runAppProp [intId User "x" 1, intId User "w" 3] [parseToTermQQ|
  (let { (x_1 :: Int) = (w_3 :: Int) } in \(y_4 :: Int) -> y_4) (x_1 :: Int)
|]

-- | Case 2 of Note [AppProp no shadowing]: a lambda applied to an argument that
-- performs work becomes a let-binding, so its binder must not capture the free
-- variables of the remaining arguments:
--
-- > (\x -> \y -> y) (f x) (f x)  ==>  let x' = f x in let y = f x in y
--
-- https://github.com/clash-lang/clash-compiler/pull/1040
appPropLamCapture :: IO Term
appPropLamCapture = runAppProp [x, f] (mkApps fun [Left fx, Left fx])
 where
  x = intId User "x" 1
  y = intId User "y" 2
  f = localId User "f" 3 (intFunTy 1)
  fx = App (Var f) (Var x)
  fun = Lam x (Lam y (Var y))

-- | The expected result of 'appPropLamCapture'
appPropLamCaptureExpected :: Term
appPropLamCaptureExpected = Let (NonRec x9 fx) (Let (NonRec y fx) (Var y))
 where
  x9 = intId User "x" 9
  y = intId User "y" 2
  f = localId User "f" 3 (intFunTy 1)
  fx = App (Var f) (Var (intId User "x" 1))

-- | Case 1 of Note [AppProp no shadowing]: arguments are pushed into case
-- alternatives, so pattern binders must not capture their free variables:
--
-- > (case s of MkPair a b -> \k -> k) b  ==>  case s of MkPair a b' -> b
--
-- https://github.com/clash-lang/clash-compiler/pull/1040
appPropCaseCapture :: IO Term
appPropCaseCapture = runAppProp [s, b] (App fun (Var b))
 where
  s = localId User "s" 5 pairTy
  a = intId User "a" 1
  b = intId User "b" 2
  k = intId User "k" 6
  fun = Case (Var s) (intFunTy 1) [(DataPat pairDataCon [] [a, b], Lam k (Var k))]

-- | The expected result of 'appPropCaseCapture'
appPropCaseCaptureExpected :: Term
appPropCaseCaptureExpected =
  Case (Var s) intTy
    [(DataPat pairDataCon [] [intId User "a" 1, intId User "b" 9], Var b)]
 where
  s = localId User "s" 5 pairTy
  b = intId User "b" 2

-- | Issue #1035: 'bindConstantVar' inlines a let-bound lambda without
-- deshadowing it, so the result shadows:
--
-- > let a = \k x -> k in \x -> a x p  ==>  \x -> (\k x -> k) x p
--
-- This broke the no-shadowing invariant 'appProp' relied on. #1036 (and #1037,
-- which absorbed it) made substitution deshadow, but were closed in favour of
-- #1040, which made 'appProp' deshadow instead. 'appProp' should reduce the
-- body of the resulting lambda to the outer @x@, not to @p@.
--
-- https://github.com/clash-lang/clash-compiler/issues/1035
bindConstantVarAppProp :: IO Term
bindConstantVarAppProp = bindConstantVarThenAppProp [p] term
 where
  k = intId User "k" 6
  x = intId User "x" 1
  p = intId User "p" 5
  a = localId User "a" 3 (intFunTy 2)
  term =
    Let (NonRec a (Lam k (Lam x (Var k))))
      (Lam x (mkApps (Var a) [Left (Var x), Left (Var p)]))

-- | Issue #990: inlining let-bindings ('inlineBinders', used by
-- 'bindConstantVar') introduced shadowing, after which 'appProp' let-bound an
-- argument with a binder that captured the free variables of the other
-- arguments. The resulting HDL contained a signal assigned to itself. #991
-- deshadowed after inlining let-bindings; #1040 replaced that by deshadowing
-- in 'appProp'.
--
-- > let a = \x y -> y in \x -> a (f x) (f x)
-- >   ==> (bindConstantVar)  \x -> (\x y -> y) (f x) (f x)
-- >   ==> (appProp)          \x -> let x' = f x in let y = f x in y
--
-- https://github.com/clash-lang/clash-compiler/issues/990
-- https://github.com/clash-lang/clash-compiler/pull/991
bindConstantVarAppPropLet :: IO Term
bindConstantVarAppPropLet = bindConstantVarThenAppProp [f] term
 where
  x = intId User "x" 1
  y = intId User "y" 2
  f = localId User "f" 3 (intFunTy 1)
  a = localId User "a" 4 (intFunTy 2)
  fx = App (Var f) (Var x)
  term =
    Let (NonRec a (Lam x (Lam y (Var y))))
      (Lam x (mkApps (Var a) [Left fx, Left fx]))

-- | @(\\f -> f) (\\k x -> k) x y@ reduces to the free variable @x@. The first
-- argument binds @x_2@, which is also free in the second argument. 'appProp'
-- substitutes work-free arguments using 'unsafeSubstTm', which does not avoid
-- capture: after substituting the first argument for @f_1@, that argument is
-- the head of the application, and substituting @x_2@ for @k_4@ puts it under
-- the binder @x_2@.
--
-- https://github.com/clash-lang/clash-compiler/pull/3479
appPropCapture :: IO Term
appPropCapture = runAppProp [intId User "x" 2, intId User "y" 5] [parseToTermQQ|
  (\(f_1 :: Int) -> f_1) (\(k_4 :: Int) (x_2 :: Int) -> k_4) (x_2 :: Int) (y_5 :: Int)
|]

tests :: TestTree
tests =
  testGroup
    "Clash.Tests.Normalize.Transformations.Specialize"
    [ testCase "appProp deshadows let-bindings in the function part (#492, #1040)" $
        appPropLetCapture >>= assertAlphaEq [parseToTermQQ|
          let { (z_9 :: Int) = (w_3 :: Int) } in (x_1 :: Int)
        |]
    , testCase "appProp let-binds lambda arguments without capture (#1040)" $
        appPropLamCapture >>= assertAlphaEq appPropLamCaptureExpected
    , testCase "appProp pushes arguments into alternatives without capture (#1040)" $
        appPropCaseCapture >>= assertAlphaEq appPropCaseCaptureExpected
    , testCase "appProp handles shadowing introduced by bindConstantVar (#1035)" $
        bindConstantVarAppProp >>= assertAlphaEq (parseToTerm "x_1 :: Int")
    , testCase "appProp does not let-bind a captured argument after inlining (#991)" $
        bindConstantVarAppPropLet >>= assertAlphaEq appPropLamCaptureExpected
    , testCase "appProp does not capture free variables (#3479)" $
        appPropCapture >>= assertAlphaEq (parseToTerm "x_2 :: Int")
    ]
