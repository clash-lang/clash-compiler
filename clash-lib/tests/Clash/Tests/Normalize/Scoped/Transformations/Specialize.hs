{-|
  Copyright   :  (C) 2026, QBayLogic B.V.
  License     :  BSD2 (see the file LICENSE)
  Maintainer  :  QBayLogic B.V. <devops@qbaylogic.com>

  Tests for "Clash.Normalize.Scoped.Transformations.Specialize" and
  "Clash.Normalize.Scoped.Transformations.Cast": the regression tests of
  "Clash.Normalize.Transformations.Specialize", run on the scoped
  transformations through 'Test.Clash.Scoped.ViaFoil.viaFoil', and
  agreement with the transformations of Clash Core.
-}

{-# LANGUAGE TemplateHaskell #-}

module Clash.Tests.Normalize.Scoped.Transformations.Specialize (tests) where

import Data.Default (def)

import Test.Tasty
import Test.Tasty.HUnit

import Clash.Core.Literal (Literal (IntLiteral))
import Clash.Core.Name (NameSort (Internal, User), noSrcSpan)
import qualified Clash.Core.Term as C
import qualified Clash.Core.Type as C
import Clash.Core.Var (Id)
import Clash.Core.VarEnv (extendInScopeSetList)
import qualified Clash.Normalize.Scoped.Transformations.Cast as Scoped
import qualified Clash.Normalize.Scoped.Transformations.Specialize as Scoped
import qualified Clash.Normalize.Transformations.Cast as C
import qualified Clash.Normalize.Transformations.Specialize as C
import Clash.Normalize.Types (NormRewrite, NormalizeState)
import Clash.Rewrite.Scoped (ScopedTransform (..))
import Test.Clash.Scoped.ViaFoil (viaFoil)
import Clash.Rewrite.StrategyDSL
  (onApp, onAppNode, onLam, onTyApp, onTyAppNode, toTransformation)
import Clash.Rewrite.StrategyDSL.TH (asRewriteQ, asScopedRewriteQ)
import Clash.Rewrite.Types (RewriteEnv, RewriteState (..))
import Clash.Rewrite.Util (runRewrite)
import Clash.Util.Supply (Supply, newSupply)

import Clash.Tests.Normalize.Transformations.Specialize
  (appPropTests, nonRepSpecTests, specializeTests)
import Test.Clash.Rewrite
  ( assertAlphaEq, bitVectorTy, exZwTy, globalId, globalsByName, inScopeOf
  , indexTy, intFunTy, intId, intTy, kindedTyVar, localId, mkBindingMap, pairDataCon
  , pairTy, proxyTy, runRewriteTest, signedTy, streamTy, tProxyTy, tyVar, unsignedTy
  , withTyConMap, zeroWidthTyConMap, zwTy )
import Test.Clash.Scoped (assertNewBindingsAgree)
import Clash.Core.Scoped.Convert (mkGlobalIdFromClash)

-- * The transformations

-- | The scoped 'Scoped.specialize', as a rewrite of Clash Core
specializeViaFoil :: NormRewrite
specializeViaFoil = viaFoil "specialize" (ScopedTransform Scoped.specialize)

-- | The scoped transformations, through 'viaFoil', on terms of Clash Core
appPropS :: NormRewrite
appPropS = (viaFoil "appProp" $(asScopedRewriteQ Scoped.appProp))
constantSpecS :: NormRewrite
constantSpecS = (viaFoil "constantSpec" $(asScopedRewriteQ Scoped.constantSpec))
nonRepSpecS :: NormRewrite
nonRepSpecS = (viaFoil "nonRepSpec" $(asScopedRewriteQ Scoped.nonRepSpec))
typeSpecS :: NormRewrite
typeSpecS = (viaFoil "typeSpec" $(asScopedRewriteQ Scoped.typeSpec))
zeroWidthSpecS :: NormRewrite
zeroWidthSpecS = (viaFoil "zeroWidthSpec" $(asScopedRewriteQ Scoped.zeroWidthSpec))
argCastSpecS :: NormRewrite
argCastSpecS = (viaFoil "argCastSpec" $(asScopedRewriteQ Scoped.argCastSpec))

-- | The transformations of Clash Core
appPropC :: NormRewrite
appPropC = $(asRewriteQ (toTransformation "applicationPropagation"
  (onAppNode 'C.appPropWorker <> onTyAppNode 'C.appPropWorker)))
constantSpecC :: NormRewrite
constantSpecC = $(asRewriteQ (toTransformation "constantSpec" (onApp 'C.constantSpecWorker)))
nonRepSpecC :: NormRewrite
nonRepSpecC = $(asRewriteQ (toTransformation "nonRepSpec" (onApp 'C.nonRepSpecWorker)))
typeSpecC :: NormRewrite
typeSpecC = $(asRewriteQ (toTransformation "typeSpec" (onTyApp 'C.typeSpecWorker)))
zeroWidthSpecC :: NormRewrite
zeroWidthSpecC = $(asRewriteQ (toTransformation "zeroWidthSpec" (onLam 'C.zeroWidthSpecWorker)))
argCastSpecC :: NormRewrite
argCastSpecC = $(asRewriteQ (toTransformation "argCastSpec" (onApp 'C.argCastSpecWorker)))

-- * Fixtures

-- | Global functions with bindings: @f :: Int -> Int@, @fPoly :: forall b. b
-- -> Int -> Int@, @fT :: forall b. b -> b@, @fHo :: (Int -> Int) -> Int@,
-- @fPair :: Pair -> Int@; and global functions @g, k :: Int -> Int -> Int@,
-- of which only @k@ has a binding, and @kInt@, which is like @k@ but is an
-- internal function (one Clash created)
f, fPoly, fT, fHo, fPair, g, k, kInt :: Id
f = globalId User "f" 10 (intFunTy 1)
fPoly = globalId User "fPoly" 14 (C.ForAllTy b (C.mkFunTy (C.VarTy b) (intFunTy 1)))
fT = globalId User "fT" 16 (C.ForAllTy b (C.mkFunTy (C.VarTy b) (C.VarTy b)))
fHo = globalId User "fHo" 17 (C.mkFunTy (intFunTy 1) intTy)
fPair = globalId User "fPair" 24 (C.mkFunTy pairTy intTy)
g = globalId User "g" 11 (intFunTy 2)
k = globalId User "k" 18 (intFunTy 2)
kInt = globalId Internal "kInt" 25 (intFunTy 2)

b :: C.TyVar
b = tyVar User "b" 51

-- | The bindings of the global functions
bindings :: [(Id, C.Term)]
bindings =
  [ (f, C.Lam p (C.Var p))
  , (fPoly, C.TyLam b (C.Lam (localId User "y" 21 (C.VarTy b)) (C.Lam p (C.Var p))))
  , (fT, C.TyLam b (C.Lam (localId User "y" 21 (C.VarTy b)) (C.Var (localId User "y" 21 (C.VarTy b)))))
  , (fHo, C.Lam h (C.App (C.Var h) (C.Literal (IntLiteral 1))))
  , (fPair, C.Lam pr (C.Case (C.Var pr) intTy [(C.DataPat pairDataCon [] [p, q], C.Var q)]))
  , (k, C.Lam p (C.Lam q (C.Var q)))
  , (kInt, C.Lam p (C.Lam q (C.Var q)))
  ]
 where
  p = intId User "p" 20
  q = intId User "q" 22
  h = localId User "h" 23 (intFunTy 1)
  pr = localId User "pr" 26 pairTy

-- | Run a rewrite on a term, with the given type and term variables in scope
runRw
  :: RewriteEnv -> Supply -> NormRewrite -> [C.TyVar] -> [Id] -> C.Term
  -> IO (C.Term, RewriteState NormalizeState, RewriteState NormalizeState, Bool)
runRw env supply rw tvs ids tm = do
  let st0 :: RewriteState NormalizeState
      st0 = def
        { _bindings = mkBindingMap bindings
        , _uniqSupply = supply
        , _curFun = (mkGlobalIdFromClash (globalId User "top" 42 (intFunTy 1)), noSrcSpan) }
      is = extendInScopeSetList (inScopeOf ids) tvs
  (res, st1, ch) <- runRewriteTest env st0 (runRewrite "test" is rw tm)
  pure (res, st0, st1, ch)

-- | Two implementations of a rewrite agree on a term: the results are
-- alpha-equivalent, up to the uniques of new functions, which are created
-- with alpha-equivalent bodies, and both signal a change or neither does
assertAgreesIn :: RewriteEnv -> NormRewrite -> NormRewrite -> [C.TyVar] -> [Id] -> C.Term -> Assertion
assertAgreesIn env current scoped tvs ids tm = do
  supply <- newSupply
  (expected, st0, st1, ch1) <- runRw env supply current tvs ids tm
  (actual, _, st2, ch2) <- runRw env supply scoped tvs ids tm
  assertAlphaEq (globalsByName expected) (globalsByName actual)
  assertNewBindingsAgree st0 st1 st2
  assertEqual "change signaled" ch1 ch2

-- | 'assertAgreesIn' in the environment of the test types
assertAgrees :: NormRewrite -> NormRewrite -> [C.TyVar] -> [Id] -> C.Term -> Assertion
assertAgrees = assertAgreesIn tcEnv

-- | Assert that a rewrite fires on a term (signals a change), and that it
-- agrees with the one of Clash Core
assertFiresAndAgreesIn
  :: RewriteEnv -> NormRewrite -> NormRewrite -> [C.TyVar] -> [Id] -> C.Term -> Assertion
assertFiresAndAgreesIn env current scoped tvs ids tm = do
  supply <- newSupply
  (_, _, _, ch) <- runRw env supply scoped tvs ids tm
  assertBool "the scoped transformation doesn't fire" ch
  assertAgreesIn env current scoped tvs ids tm

-- | Assert that a rewrite doesn't fire on a term, and leaves it as it is
assertDoesNotFireIn :: RewriteEnv -> NormRewrite -> [C.TyVar] -> [Id] -> C.Term -> Assertion
assertDoesNotFireIn env rw tvs ids tm = do
  supply <- newSupply
  (res, _, _, ch) <- runRw env supply rw tvs ids tm
  assertBool "the transformation fires" (not ch)
  assertAlphaEq tm res

-- | The environment of the test types, including the zero-width types (see
-- 'zeroWidthTyConMap')
tcEnv :: RewriteEnv
tcEnv = withTyConMap zeroWidthTyConMap def

-- | @\\(x :: ty) -> h x@, with @h :: ty -> Int@ a free local function
zwLam :: C.Type -> (C.Term, Id)
zwLam ty = (C.Lam x (C.App (C.Var h) (C.Var x)), h)
 where
  x = localId User "x" 60 ty
  h = localId User "h" 61 (C.mkFunTy ty intTy)

-- | 'zeroWidthSpec' agrees with that of Clash Core on 'zwLam', and fires
zwFires :: C.Type -> Assertion
zwFires ty = case zwLam ty of
  (tm, h) -> assertFiresAndAgreesIn tcEnv zeroWidthSpecC zeroWidthSpecS [] [h] tm

-- | 'zeroWidthSpec' doesn't fire on 'zwLam'
zwDoesNotFire :: C.Type -> Assertion
zwDoesNotFire ty = case zwLam ty of
  (tm, h) -> assertDoesNotFireIn tcEnv zeroWidthSpecS [] [h] tm

tests :: TestTree
tests =
  testGroup "Clash.Tests.Normalize.Scoped.Transformations.Specialize"
    [ testGroup "regression tests of Clash Core, through viaFoil"
        [ testGroup "appProp" (appPropTests appPropS)
        , testGroup "specialize" (specializeTests specializeViaFoil)
        , testGroup "nonRepSpec" (nonRepSpecTests nonRepSpecS)
        ]
    , testGroup "specialize agrees with Clash Core"
        [ testCase "f (g x y)" $
            specializeAgrees [] [x, y] (C.App (C.Var f) (app g [C.Var x, C.Var y]))
        , testCase "fPoly @a (x :: a) (g u v)" $
            specializeAgrees [a] [xa, u, v]
              (C.App (C.mkApps (C.Var fPoly) [Right (C.VarTy a), Left (C.Var xa)])
                     (app g [C.Var u, C.Var v]))
        , testCase "f (case (x :: a) of {}), a :: kk" $
            specializeAgrees [kk, ak] [xak] (C.App (C.Var f) (C.Case (C.Var xak) intTy []))
        , testCase "fT @Int" $
            specializeAgrees [] [] (C.TyApp (C.Var fT) intTy)
        , testCase "fHo (k x)" $
            specializeAgrees [] [x] (C.App (C.Var fHo) (app k [C.Var x]))
        , testCase "fHo (g x)" $
            specializeAgrees [] [x] (C.App (C.Var fHo) (app g [C.Var x]))
        , testCase "(\\z -> z) (g x y)" $
            specializeAgrees [] [x, y]
              (C.App (C.Lam z (C.Var z)) (app g [C.Var x, C.Var y]))
        ]
    , testGroup "appProp agrees with Clash Core"
        [ testCase "lambdas, work-free and not work-free arguments" $
            assertAgrees appPropC appPropS [] [x, y, fl]
              (C.mkApps (C.Lam z (C.Lam w (app g [C.Var z, C.Var w])))
                 [Left (C.Var x), Left (C.App (C.Var fl) (C.Var y))])
        , testCase "type lambda" $
            assertAgrees appPropC appPropS [] [x]
              (C.mkApps (C.TyLam a (C.Lam xa (C.Var xa))) [Right intTy, Left (C.Var x)])
        , testCase "let-expression" $
            assertAgrees appPropC appPropS [] [x, y, fl]
              (C.App (C.Let (C.NonRec z (C.App (C.Var fl) (C.Var x))) (C.Lam w (app g [C.Var z, C.Var w])))
                     (C.Var y))
        , testCase "ticks" $
            assertAgrees appPropC appPropS [] [x]
              (C.App (C.Tick (C.SrcSpan noSrcSpan) (C.Lam z (C.Var z))) (C.Var x))
        , testCase "case-expression, an argument that isn't work free" $
            assertAgrees appPropC appPropS [] [s, x, fl]
              (C.App (C.Case (C.Var s) (intFunTy 1)
                        [(C.DataPat pairDataCon [] [pa, pb], C.Lam z (app g [C.Var pa, C.Var z]))])
                     (C.App (C.Var fl) (C.Var x)))
        , testCase "case-expression, a type argument and a work-free argument" $
            assertAgrees appPropC appPropS [] [s, x]
              (C.mkApps (C.Case (C.Var s) (C.ForAllTy a (C.mkFunTy (C.VarTy a) (C.VarTy a)))
                          [(C.DataPat pairDataCon [] [pa, pb], C.TyLam a (C.Lam xa (C.Var xa)))])
                 [Right intTy, Left (C.Var x)])
        , testCase "doesn't fire on a variable applied to arguments" $
            assertDoesNotFireIn tcEnv appPropS [] [x, fl] (C.App (C.Var fl) (C.Var x))
        ]
    , testGroup "constantSpec agrees with Clash Core"
        [ testCase "f 3: a constant argument" $
            assertFiresAndAgreesIn tcEnv constantSpecC constantSpecS [] [] (C.App (C.Var f) (lit 3))
        , testCase "fPair (MkPair x 2): a partially constant argument" $
            assertFiresAndAgreesIn tcEnv constantSpecC constantSpecS [] [x]
              (C.App (C.Var fPair) (C.mkApps (C.Data pairDataCon) [Left (C.Var x), Left (lit 2)]))
        , testCase "fPair (MkPair (fl x) (fl y)): no constant parts" $
            assertAgrees constantSpecC constantSpecS [] [x, y, fl]
              (C.App (C.Var fPair)
                (C.mkApps (C.Data pairDataCon) [Left (C.App (C.Var fl) (C.Var x)), Left (C.App (C.Var fl) (C.Var y))]))
        , testCase "fl 3: a local function" $
            assertAgrees constantSpecC constantSpecS [] [fl] (C.App (C.Var fl) (lit 3))
        , testCase "fPoly @Int 3 4: a type argument" $
            assertDoesNotFireIn tcEnv constantSpecS [] []
              (C.mkApps (C.Var fPoly) [Right intTy, Left (lit 3), Left (lit 4)])
        ]
    , testGroup "typeSpec agrees with Clash Core"
        [ testCase "fT @Int" $
            assertFiresAndAgreesIn tcEnv typeSpecC typeSpecS [] [] (C.TyApp (C.Var fT) intTy)
        , testCase "fT @a: a free type variable" $
            assertDoesNotFireIn tcEnv typeSpecS [a] [] (C.TyApp (C.Var fT) (C.VarTy a))
        , testCase "fT @Pair" $
            assertFiresAndAgreesIn tcEnv typeSpecC typeSpecS [] [] (C.TyApp (C.Var fT) pairTy)
        ]
    , testGroup "nonRepSpec agrees with Clash Core"
        [ testCase "fHo (g x)" $
            assertFiresAndAgreesIn tcEnv nonRepSpecC nonRepSpecS [] [x] (C.App (C.Var fHo) (app g [C.Var x]))
        , testCase "fHo (kInt x): an internal function is inlined" $
            assertFiresAndAgreesIn tcEnv nonRepSpecC nonRepSpecS [] [x] (C.App (C.Var fHo) (app kInt [C.Var x]))
        , testCase "fHo h: a local variable" $
            assertDoesNotFireIn tcEnv nonRepSpecS [] [hl] (C.App (C.Var fHo) (C.Var hl))
        , testCase "f x: a representable argument" $
            assertDoesNotFireIn tcEnv nonRepSpecS [] [x] (C.App (C.Var f) (C.Var x))
        ]
    , testGroup "zeroWidthSpec agrees with Clash Core"
        [ testCase "BitVector 0" $ zwFires (bitVectorTy 0)
        , testCase "Unsigned 0" $ zwFires (unsignedTy 0)
        , testCase "Signed 0" $ zwFires (signedTy 0)
        , testCase "Index 1" $ zwFires (indexTy (numTy 1))
        , testCase "a product of zero-width types" $ zwFires zwTy
        , testCase "a field type that mentions a type argument" $ zwFires (tProxyTy (numTy 3))
        , testCase "a type argument" $ zwFires (proxyTy (numTy 3))
        , testCase "BitVector 1" $ zwDoesNotFire (bitVectorTy 1)
        , testCase "Index 2" $ zwDoesNotFire (indexTy (numTy 2))
        , testCase "Int" $ zwDoesNotFire intTy
        , testCase "a constructor with an existential type variable" $ zwDoesNotFire exZwTy
        , testCase "a recursive type" $ zwDoesNotFire (streamTy (bitVectorTy 0))
        ]
    , testGroup "argCastSpec agrees with Clash Core"
        [ testCase "fPair (cast x)" $
            assertFiresAndAgreesIn tcEnv argCastSpecC argCastSpecS [] [x]
              (C.App (C.Var fPair) (C.Cast (C.Var x) intTy pairTy))
        , testCase "fPair (cast (fl x)): a cast that isn't work free" $
            assertFiresAndAgreesIn tcEnv argCastSpecC argCastSpecS [] [x, fl]
              (C.App (C.Var fPair) (C.Cast (C.App (C.Var fl) (C.Var x)) intTy pairTy))
        , testCase "fPair <tick> (cast x)" $
            assertFiresAndAgreesIn tcEnv argCastSpecC argCastSpecS [] [x]
              (C.App (C.Var fPair) (C.Tick (C.SrcSpan noSrcSpan) (C.Cast (C.Var x) intTy pairTy)))
        , testCase "fPoly @Pair y (cast x): type arguments" $
            assertFiresAndAgreesIn tcEnv argCastSpecC argCastSpecS [] [x, yp]
              (C.App (C.mkApps (C.Var fPoly) [Right pairTy, Left (C.Var yp)])
                     (C.Cast (C.Var x) pairTy intTy))
        , testCase "fPair (cast (cast x)): a cast of a cast" $
            assertDoesNotFireIn tcEnv argCastSpecS [] [x]
              (C.App (C.Var fPair) (C.Cast (C.Cast (C.Var x) intTy intTy) intTy pairTy))
        , testCase "pl (cast x): a local function" $
            assertDoesNotFireIn tcEnv argCastSpecS [] [x, pl]
              (C.App (C.Var pl) (C.Cast (C.Var x) intTy pairTy))
        ]
    ]
 where
  x = intId User "x" 12
  y = intId User "y" 13
  z = intId User "z" 19
  w = intId User "w" 27
  u = intId User "u" 15
  v = intId User "v" 16
  s = localId User "s" 28 pairTy
  pa = intId User "a" 29
  pb = intId User "b" 30
  fl = localId User "fl" 31 (intFunTy 1)
  hl = localId User "hl" 32 (intFunTy 1)
  pl = localId User "pl" 33 (C.mkFunTy pairTy intTy)
  yp = localId User "yp" 34 pairTy
  a = tyVar User "a" 50
  xa = localId User "x" 12 (C.VarTy a)
  kk = tyVar User "kk" 52
  ak = kindedTyVar User "a" 50 (C.VarTy kk)
  xak = localId User "x" 12 (C.VarTy ak)

  app hd = C.mkApps (C.Var hd) . map Left
  lit = C.Literal . IntLiteral

  numTy = C.LitTy . C.NumTy

  specializeAgrees = assertAgrees C.specialize specializeViaFoil
