{-|
  Copyright   :  (C) 2026, QBayLogic B.V.
  License     :  BSD2 (see the file LICENSE)
  Maintainer  :  QBayLogic B.V. <devops@qbaylogic.com>

  Tests for "Clash.Normalize.Scoped.Transformations.Reduce": the scoped
  'Scoped.reducePrim' agrees with
  'Clash.Normalize.Transformations.Reduce.reducePrimWorker' when run through
  'Test.Clash.Scoped.ViaFoil.viaFoil', the scoped 'Scoped.reduceConst'
  evaluates with the let-bindings of its context, and 'Scoped.reduceBinders'
  removes duplicate bindings.
-}

{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module Clash.Tests.Normalize.Scoped.Transformations.Reduce (tests) where

import Data.Default (def)
import qualified Data.IntMap.Strict as IntMap
import Data.Text (Text)

import Test.Tasty
import Test.Tasty.HUnit

import Clash.Core.Literal (Literal (..))
import Clash.Core.Name (NameSort (User), nameOcc)
import qualified Clash.Core.Term as C
import qualified Clash.Core.Type as C
import Clash.Core.TysPrim (integerPrimTy, typeNatKind)
import Clash.Core.Var (Id)
import qualified Clash.Normalize.Transformations.Reduce as Current
import Clash.Normalize.Types (NormalizeSession, NormalizeState)
import Clash.Rewrite.Types (RewriteEnv (..), RewriteState (..), TransformContext (..))

import Clash.Core.Scoped.NameMap (emptyNameMap, nameMapToList)
import Clash.Core.Scoped.Syntax
import qualified Clash.Normalize.Scoped.Transformations.Reduce as Scoped
import Clash.Rewrite.Scoped (ScopedRewrite, ScopedTransform (..))
import qualified Clash.Rewrite.Scoped as S
import Clash.Rewrite.Scoped.Util (LetBinding (..), openLetBindings)

import Test.Clash.Rewrite
  ( boolTy, heapLookupEvaluator, inScopeOf, intFunTy, intId, intLit, intTy
  , localId, runRewriteTest, showPprU, tcApp, testTyConMap, tyVar, vecTy
  , withTyConMap )
import qualified Test.Clash.Rewrite as T
import Test.Clash.Scoped (aeqTermTyped, runScopedOn, runViaFoil, stripCoercionTypes)

-- | The default environment, with the type constructors of 'testTyConMap'
-- (among which @Vec@), and an evaluator that only looks up variables on the
-- heap: the scoped evaluator reduces the primitives of the 'reduceConst' tests
-- itself.
env :: RewriteEnv
env = (withTyConMap testTyConMap def) {_evaluator = heapLookupEvaluator}

st :: RewriteState NormalizeState
st = def {_globalHeap = (IntMap.empty, 0)}

-- | The 'App' worker of a transformation of Clash Core, as a rewrite
onAppC
  :: (TransformContext -> C.Term -> C.Term -> C.Term -> NormalizeSession C.Term)
  -> TransformContext -> C.Term -> NormalizeSession C.Term
onAppC w ctx e = case e of
  C.App fun arg -> w ctx e fun arg
  _ -> pure e

-- | The 'App' worker of a scoped transformation, as a scoped rewrite
onAppS
  :: (forall n. Distinct n => S.TransformContext n -> Term n -> Term n -> Term n -> NormalizeSession (Term n))
  -> ScopedRewrite NormalizeState
onAppS w = ScopedTransform $ \ctx e -> case e of
  App fun arg -> w ctx e fun arg
  _ -> pure e

-- | The context of a term with the given free variables, and frames
ctxOf :: [Id] -> C.Context -> TransformContext
ctxOf ids = TransformContext (inScopeOf ids)

-- | Run the current and the scoped 'reducePrim' at the root of a term, in the
-- given context, and assert that they agree: alpha-equivalent results
-- (comparing types too, up to the form of coercions), and both signal a
-- change or neither does. Gives the result of the scoped one, and whether it
-- changed.
assertReducePrimAgrees :: TransformContext -> C.Term -> IO (C.Term, Bool)
assertReducePrimAgrees ctx e = do
  (expected, _, changed1) <-
    runRewriteTest env st (onAppC Current.reducePrimWorker ctx e)
  (actual, _, changed2) <-
    runViaFoil env st ctx (onAppS Scoped.reducePrimWorker) e
  assertBool
    ("Clash Core:\n" <> showPprU expected <> "\nscoped:\n" <> showPprU actual)
    (aeqTermTyped (stripCoercionTypes expected) (stripCoercionTypes actual))
  assertEqual "change signaled (Clash Core, scoped)" changed1 changed2
  pure (actual, changed2)

-- | 'assertReducePrimAgrees', and assert that it fired or not
reducePrimFires :: Bool -> TransformContext -> C.Term -> Assertion
reducePrimFires fires ctx e = do
  (actual, changed) <- assertReducePrimAgrees ctx e
  assertEqual ("fired, result:\n" <> showPprU actual) fires changed

-- | A primitive with a name and type
prim :: Text -> C.Type -> C.PrimInfo
prim nm ty = C.PrimInfo nm ty C.WorkNever C.SingleResult C.NoUnfolding

-- | Type variables of the primitives
tvA, tvB, tvN, tvM :: C.TyVar
tvA = tyVar User "a" 50
tvB = tyVar User "b" 51
tvN = T.kindedTyVar User "n" 52 typeNatKind
tvM = T.kindedTyVar User "m" 53 typeNatKind

forAlls :: [C.TyVar] -> C.Type -> C.Type
forAlls tvs ty = foldr C.ForAllTy ty tvs

funTys :: [C.Type] -> C.Type -> C.Type
funTys args res = foldr C.mkFunTy res args

-- | @map :: forall a b n. (a -> b) -> Vec n a -> Vec n b@
mapPrim :: C.PrimInfo
mapPrim = prim "Clash.Sized.Vector.map" $ forAlls [tvA, tvB, tvN] $
  funTys [C.mkFunTy (C.VarTy tvA) (C.VarTy tvB), vecTy (C.VarTy tvN) (C.VarTy tvA)]
    (vecTy (C.VarTy tvN) (C.VarTy tvB))

-- | @head :: forall n a. Vec (n + 1) a -> a@
headPrim :: C.PrimInfo
headPrim = prim "Clash.Sized.Vector.head" $ forAlls [tvN, tvA] $
  funTys [vecTy (T.addTy (C.VarTy tvN) (T.numTy 1)) (C.VarTy tvA)] (C.VarTy tvA)

-- | @split# :: forall n m. KnownNat n => BitVector (m + n) -> (BitVector m,
-- BitVector n)@, with @Int@ for @KnownNat n@ and the bit vectors, and @T2@
-- for the tuple
splitPrim :: C.PrimInfo
splitPrim = prim "Clash.Sized.Internal.BitVector.split#" $ forAlls [tvN, tvM] $
  funTys [intTy, intTy] (tcApp T.t2TyConName [intTy, intTy])

-- | @eq# :: forall n. KnownNat n => BitVector n -> BitVector n -> Bool@, with
-- @Int@ for @KnownNat n@ and the bit vectors
eqPrim :: C.PrimInfo
eqPrim = prim "Clash.Sized.Internal.BitVector.eq#" $ forAlls [tvN] $
  funTys [intTy, intTy, intTy] boolTy

-- | A primitive @vio :: Vec 0 Int@ that always performs work
vioPrim :: C.PrimInfo
vioPrim = (prim "vio" (vecTy (T.numTy 0) intTy)) {C.primWorkInfo = C.WorkAlways}

-- | @GHC.Num.Integer.integerAdd :: Integer -> Integer -> Integer@, which the
-- scoped evaluator reduces itself
integerAddPrim :: C.PrimInfo
integerAddPrim = prim "GHC.Num.Integer.integerAdd" $
  funTys [integerPrimTy, integerPrimTy] integerPrimTy

integerLit :: Integer -> C.Term
integerLit = C.Literal . IntegerLiteral

-- | Free variables: @f :: Int -> Int@, @g :: (Int -> Int) -> Int@, vectors
-- @v0 :: Vec 0 Int@, @v1 :: Vec 1 Int@, @v3 :: Vec 3 Int@, @vf :: Vec 1 (Int
-- -> Int)@, and @k :: Int@
f, v0, v1, v3, vf, k :: Id
f = localId User "f" 1 (intFunTy 1)
v0 = localId User "v0" 2 (vecTy (T.numTy 0) intTy)
v1 = localId User "v1" 3 (vecTy (T.numTy 1) intTy)
v3 = localId User "v3" 4 (vecTy (T.numTy 3) intTy)
vf = localId User "vf" 5 (vecTy (T.numTy 1) (intFunTy 1))
k = intId User "k" 6

mapApp :: Integer -> [C.Term] -> C.Term
mapApp n tms =
  C.mkApps (C.Prim mapPrim) (map Right [intTy, intTy, T.numTy n] <> map Left tms)

reducePrimTests :: TestTree
reducePrimTests = testGroup "reducePrim"
  [ testCase "unrolls map on a vector of one element" $
      reducePrimFires True (ctxOf [f, v1] []) (mapApp 1 [C.Var f, C.Var v1])
  , testCase "abstracts over the missing arguments of map" $
      reducePrimFires True (ctxOf [f] []) (mapApp 1 [C.Var f])
  , testCase "keeps map on a long vector of representable elements" $
      reducePrimFires False (ctxOf [f, v3] []) (mapApp 3 [C.Var f, C.Var v3])
  , testCase "replaces map returning an empty vector by Nil" $
      reducePrimFires True (ctxOf [f, v0] []) (mapApp 0 [C.Var f, C.Var v0])
  , testCase "unrolls map returning an empty vector, abstracting over the vector" $
      reducePrimFires True (ctxOf [f] []) (mapApp 0 [C.Var f])
  , testCase "keeps a primitive returning an empty vector that always performs work" $
      -- Applied to a type argument, which 'vio' doesn't take, so that it is an
      -- application
      reducePrimFires False (ctxOf [] [])
        (C.TyApp (C.Prim vioPrim {C.primType = C.ForAllTy tvA (C.primType vioPrim)}) intTy)
  , testCase "keeps the ticks of the primitive" $
      reducePrimFires True (ctxOf [f, v1] [])
        (C.App (C.Tick C.NoDeDup (mapApp 1 [C.Var f])) (C.Var v1))
  , testCase "applies the result of head to the remaining arguments" $
      -- 'head' of a vector of functions, applied to an argument
      reducePrimFires True (ctxOf [vf, k] [])
        (C.mkApps (C.Prim headPrim) [Right (T.numTy 0), Right (intFunTy 1), Left (C.Var vf), Left (C.Var k)])
  , testCase "only fires at the root of an application spine" $
      reducePrimFires False (ctxOf [f, v1] [C.AppFun]) (mapApp 1 [C.Var f, C.Var v1])
  , testCase "replaces split# of a zero-width left part by a tuple" $
      reducePrimFires True (ctxOf [k] [])
        (C.mkApps (C.Prim splitPrim) [Right (T.numTy 0), Right (T.numTy 4), Left (intLit 4), Left (C.Var k)])
  , testCase "replaces split# of a zero-width right part, abstracting over the bit vector" $
      reducePrimFires True (ctxOf [] [])
        (C.mkApps (C.Prim splitPrim) [Right (T.numTy 4), Right (T.numTy 0), Left (intLit 0)])
  , testCase "replaces eq# of zero-width bit vectors by True" $
      reducePrimFires True (ctxOf [k] [])
        (C.mkApps (C.Prim eqPrim) [Right (T.numTy 0), Left (intLit 0), Left (C.Var k), Left (C.Var k)])
  , testCase "keeps eq# of bit vectors that aren't zero-width" $
      reducePrimFires False (ctxOf [k] [])
        (C.mkApps (C.Prim eqPrim) [Right (T.numTy 1), Left (intLit 1), Left (C.Var k), Left (C.Var k)])
  , testCase "keeps a primitive that has no handler" $
      reducePrimFires False (ctxOf [k] [])
        (C.App (C.Prim (prim "somePrim" (intFunTy 1))) (C.Var k))
  ]

-- | Run the scoped 'reduceConst' at the root of a term, in the given context,
-- through 'Test.Clash.Scoped.ViaFoil.viaFoil'. Gives the result, and
-- whether it changed.
reduceConstAt :: TransformContext -> C.Term -> IO (C.Term, Bool)
reduceConstAt ctx e = do
  (t, _, ch) <- runViaFoil env st ctx (onAppS Scoped.reduceConstWorker) e
  pure (t, ch)

reduceConstTests :: TestTree
reduceConstTests = testGroup "reduceConst"
  [ testCase "evaluates an applied primitive" $ do
      (t, ch) <- reduceConstAt (ctxOf [] []) (add (integerLit 1) (integerLit 2))
      T.assertAlphaEq (integerLit 3) t
      ch @?= True
  , testCase "only fires at the root of an application spine" $ do
      let e = add (integerLit 1) (integerLit 2)
      (t, ch) <- reduceConstAt (ctxOf [] [C.AppFun]) e
      T.assertAlphaEq e t
      ch @?= False
  , testCase "keeps a primitive that doesn't reduce" $ do
      let e = add (C.Var x) (integerLit 2)
      (t, ch) <- reduceConstAt (ctxOf [x] []) e
      T.assertAlphaEq e t
      ch @?= False
  , testCase "uses the let-bindings of the context" $ do
      (t, ch) <- reduceConstAt (ctxOf [x] [C.LetBody [(x, integerLit 1)]]) (add (C.Var x) (integerLit 2))
      T.assertAlphaEq (integerLit 3) t
      ch @?= True
    -- #2571: a binder between a let-binding of the context and the term
    -- shadows it. Scoped, the binders of the context have different names.
  , testCase "doesn't use a let-binding of the context that is shadowed" $ do
      let e = add (C.Var x) (integerLit 2)
      (t, ch) <- reduceConstAt (ctxOf [x] [C.LamBody x, C.LetBody [(x, integerLit 1)]]) e
      T.assertAlphaEq e t
      ch @?= False
  ]
 where
  x = localId User "x" 10 integerPrimTy
  add a b = C.mkApps (C.Prim integerAddPrim) [Left a, Left b]

-- | Run 'Scoped.reduceBinders' on the bindings of a let-expression, in the
-- order they have. Gives the names of the binders it substitutes for, with the
-- names of the variables it substitutes, the names of the remaining bindings,
-- and whether it signaled a change.
reduceBindersOn :: C.Term -> IO ([(Text, Text)], [Text], Bool)
reduceBindersOn tm = do
  ((substituted, kept), _, ch) <- runScopedOn env st tm $ \sc e -> case e of
    Let bind body -> openLetBindings sc bind body $ \sc' _ lbs _ -> do
      (subst, kept) <- Scoped.reduceBinders sc' emptyNameMap [] lbs
      let occOf nm = case [nameOcc (lbOcc lb) | lb <- lbs, lbName lb == nm] of
            o : _ -> o
            [] -> "?"
          varOcc = \case
            Var o _ _ -> nameOcc o
            t -> error ("reduceBindersOn: not a variable: " <> show t)
      pure ( [(occOf nm, varOcc t) | (nm, t) <- nameMapToList subst]
           , map (nameOcc . lbOcc) kept )
    _ -> error "reduceBindersOn: not a let-expression"
  pure (substituted, kept, ch)

reduceBindersTests :: TestTree
reduceBindersTests = testGroup "reduceBinders"
  [ testCase "substitutes for a binding equal to an earlier one" $ do
      (substituted, kept, ch) <- reduceBindersOn
        (C.Letrec [(a, fk), (b, fk), (c, C.App (C.Var f) (C.Var b))] (C.Var c))
      substituted @?= [("b", "a")]
      -- The right-hand side of c refers to a after the substitution
      kept @?= ["c", "a"]
      ch @?= True
  , testCase "compares right-hand sides after substituting" $ do
      (substituted, kept, _) <- reduceBindersOn
        (C.Letrec [(a, fk), (b, fk), (c, C.App (C.Var f) (C.Var a)), (d, C.App (C.Var f) (C.Var b))] (C.Var d))
      substituted @?= [("b", "a"), ("d", "c")]
      kept @?= ["c", "a"]
  , testCase "keeps bindings marked NoDeDup" $ do
      (substituted, kept, ch) <- reduceBindersOn
        (C.Letrec [(a, fk), (b, C.Tick C.NoDeDup fk)] (C.App (C.Var f) (C.Var b)))
      substituted @?= []
      kept @?= ["b", "a"]
      ch @?= False
  ]
 where
  a = intId User "a" 20
  b = intId User "b" 21
  c = intId User "c" 22
  d = intId User "d" 23
  fk = C.App (C.Var f) (C.Var k)

tests :: TestTree
tests = testGroup "Clash.Tests.Normalize.Scoped.Transformations.Reduce"
  [ reducePrimTests
  , reduceConstTests
  , reduceBindersTests
  ]
