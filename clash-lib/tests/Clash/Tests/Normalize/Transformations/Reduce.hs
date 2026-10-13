{-|
  Copyright   :  (C) 2026, QBayLogic B.V.
  License     :  BSD2 (see the file LICENSE)
  Maintainer  :  QBayLogic B.V. <devops@qbaylogic.com>

  Tests for "Clash.Normalize.Transformations.Reduce":
  'Scoped.reducePrim', which unrolls primitives; 'Scoped.reduceConst', which
  evaluates with the let-bindings of its context; and 'Scoped.reduceBinders',
  which removes duplicate bindings. 'assertRewritesTo' runs every test in the
  scope of the free variables of its input, and in a scope that the binders
  of the input clash with.
-}

{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}

module Clash.Tests.Normalize.Transformations.Reduce (tests) where

import Data.Default (def)
import qualified Data.IntMap.Strict as IntMap
import Data.Text (Text)

import Test.Tasty
import Test.Tasty.HUnit

import Clash.Core.Name (nameOcc)
import qualified Clash.Core.Basic as C (IsMultiPrim (SingleResult), WorkInfo (..))
import Clash.Normalize.Types (NormalizeSession, NormalizeState)
import Clash.Rewrite.Types (RewriteEnv (..), RewriteState (..))

import Clash.Core.NameMap (emptyNameMap, nameMapToList)
import Clash.Core.Syntax
import qualified Clash.Normalize.Transformations.Reduce as Scoped
import Clash.Rewrite (Rewrite, Transform (..))
import qualified Clash.Rewrite as S
import Clash.Rewrite.Util (LetBinding (..), openLetBindings)

import Test.Clash.Rewrite
  ( TestDefs (..), TestTerm, ghcName, parseTestTermWith, runRewriteTest, strQQ
  , testTyConMap, withTyConMap )
import Test.Clash.Core
  ( ScopeDecl, Step (..), TestScope (..), assertNoRewriteAt, assertRewritesAt, closedType
  , varD, withTerm )

-- | The default environment, with the type constructors of 'testTyConMap'
-- (among which @Vec@)
env :: RewriteEnv
env = withTyConMap testTyConMap def

st :: RewriteState NormalizeState
st = def {_globalHeap = (IntMap.empty, 0)}

-- | The 'App' worker of a scoped transformation, as a scoped rewrite
onAppS
  :: (forall n. Distinct n => S.TransformContext n -> Term n -> Term n -> Term n -> NormalizeSession (Term n))
  -> Rewrite NormalizeState
onAppS w = Transform $ \ctx e -> case e of
  App fun arg -> w ctx e fun arg
  _ -> pure e

-- | The primitives of the tests, which do no work, except for @vio@:
--
-- * @map :: forall a b n. (a -> b) -> Vec n a -> Vec n b@
-- * @head :: forall n a. Vec (n + 1) a -> a@
-- * @split# :: forall n m. KnownNat n => BitVector (m + n) -> (BitVector m,
--   BitVector n)@, with @Int@ for @KnownNat n@ and the bit vectors, and @T2@
--   for the tuple
-- * @eq# :: forall n. KnownNat n => BitVector n -> BitVector n -> Bool@, with
--   @Int@ for @KnownNat n@ and the bit vectors
-- * @vio :: forall a. Vec 0 Int@, which always performs work, and takes a type
--   argument so that it is an application
-- * @GHC.Num.Integer.integerAdd :: Integer -> Integer -> Integer@, which the
--   scoped evaluator reduces itself
-- * @somePrim :: Int -> Int@, which has no handler
prims :: [PrimInfo]
prims =
  [ prim "Clash.Sized.Vector.map"
      "forall (a :: Type) (b :: Type) (n :: Nat). (a -> b) -> Clash.Sized.Vector.Vec n a -> Clash.Sized.Vector.Vec n b"
  , prim "Clash.Sized.Vector.head"
      "forall (n :: Nat) (a :: Type). Clash.Sized.Vector.Vec ((GHC.TypeNats.+) n 1) a -> a"
  , prim "Clash.Sized.Internal.BitVector.split#" "forall (n :: Nat) (m :: Nat). Int -> Int -> T2_3024 Int Int"
  , prim "Clash.Sized.Internal.BitVector.eq#" "forall (n :: Nat). Int -> Int -> Int -> Bool_3010"
  , (prim "vio" "forall (a :: Type). Clash.Sized.Vector.Vec 0 Int")
  , prim "GHC.Num.Integer.integerAdd"
      "GHC.Num.Integer.Integer -> GHC.Num.Integer.Integer -> GHC.Num.Integer.Integer"
  , prim "somePrim" "Int -> Int" ]
 where
  prim nm ty =
    mkPrimInfo (ghcName nm) (closedType ty) (if nm == "vio" then C.WorkAlways else C.WorkNever) C.SingleResult NoUnfolding

tm :: String -> TestTerm
tm = parseTestTermWith (TestDefs [] prims [])

reducePrimS, reduceConstS :: Rewrite NormalizeState
reducePrimS = onAppS Scoped.reducePrimWorker
reduceConstS = onAppS Scoped.reduceConstWorker

-- | Assert that a transformation, at the given path, rewrites a term to the
-- expected one
rewritesAt :: Rewrite NormalizeState -> [Step] -> [ScopeDecl] -> TestTerm -> TestTerm -> Assertion
rewritesAt rw path = assertRewritesAt env st path rw

-- | Assert that a transformation, at the given path, leaves a term as it is
keepsAt :: Rewrite NormalizeState -> [Step] -> [ScopeDecl] -> TestTerm -> Assertion
keepsAt rw path = assertNoRewriteAt env st path rw

reducePrimTests :: TestTree
reducePrimTests = testGroup "reducePrim"
  [ testCase "unrolls map on a vector of one element" $
      rewritesAt reducePrimS [] [varD "f_1" "Int -> Int", varD "v1_3" "Clash.Sized.Vector.Vec 1 Int"]
        (tm "Clash.Sized.Vector.map @Int @Int @1 f_1 v1_3")
        (tm [strQQ|
          Cons @1 @Int @0 (Prim._CO_ @((GHC.Prim.~#) Nat Nat 1 ((GHC.TypeNats.+) 0 1)))
            (f_1 ((case v1_3 of { Cons (m :: Nat) (co :: (GHC.Prim.~#) Nat Nat 1 ((GHC.TypeNats.+) 0 1)) (el :: Int) (res :: Clash.Sized.Vector.Vec 0 Int) -> el }) :: Int))
            (Clash.Sized.Vector.map @Int @Int @0 f_1
              ((case v1_3 of { Cons (m :: Nat) (co :: (GHC.Prim.~#) Nat Nat 1 ((GHC.TypeNats.+) 0 1)) (el :: Int) (res :: Clash.Sized.Vector.Vec 0 Int) -> res }) :: Clash.Sized.Vector.Vec 0 Int))
        |])
  , testCase "abstracts over the missing arguments of map" $
      rewritesAt reducePrimS [] [varD "f_1" "Int -> Int"]
        (tm "Clash.Sized.Vector.map @Int @Int @1 f_1")
        (tm [strQQ|
          \(v :: Clash.Sized.Vector.Vec 1 Int) ->
            Cons @1 @Int @0 (Prim._CO_ @((GHC.Prim.~#) Nat Nat 1 ((GHC.TypeNats.+) 0 1)))
              (f_1 ((case v of { Cons (m :: Nat) (co :: (GHC.Prim.~#) Nat Nat 1 ((GHC.TypeNats.+) 0 1)) (el :: Int) (res :: Clash.Sized.Vector.Vec 0 Int) -> el }) :: Int))
              (Clash.Sized.Vector.map @Int @Int @0 f_1
                ((case v of { Cons (m :: Nat) (co :: (GHC.Prim.~#) Nat Nat 1 ((GHC.TypeNats.+) 0 1)) (el :: Int) (res :: Clash.Sized.Vector.Vec 0 Int) -> res }) :: Clash.Sized.Vector.Vec 0 Int))
        |])
  , testCase "keeps map on a long vector of representable elements" $
      keepsAt reducePrimS [] [varD "f_1" "Int -> Int", varD "v3_4" "Clash.Sized.Vector.Vec 3 Int"]
        (tm "Clash.Sized.Vector.map @Int @Int @3 f_1 v3_4")
  , testCase "replaces map returning an empty vector by Nil" $
      rewritesAt reducePrimS [] [varD "f_1" "Int -> Int", varD "v0_2" "Clash.Sized.Vector.Vec 0 Int"]
        (tm "Clash.Sized.Vector.map @Int @Int @0 f_1 v0_2")
        (tm "Nil @0 @Int (Prim._CO_ @((GHC.Prim.~#) Nat Nat 0 0))")
  , testCase "unrolls map returning an empty vector, abstracting over the vector" $
      rewritesAt reducePrimS [] [varD "f_1" "Int -> Int"]
        (tm "Clash.Sized.Vector.map @Int @Int @0 f_1")
        (tm "\\(v :: Clash.Sized.Vector.Vec 0 Int) -> Nil @0 @Int (Prim._CO_ @((GHC.Prim.~#) Nat Nat 0 0))")
  , testCase "keeps a primitive returning an empty vector that always performs work" $
      keepsAt reducePrimS [] [] (tm "Prim.vio @Int")
  , testCase "keeps the ticks of the primitive" $
      rewritesAt reducePrimS [] [varD "f_1" "Int -> Int", varD "v1_3" "Clash.Sized.Vector.Vec 1 Int"]
        (tm "Core.noDeDup (Clash.Sized.Vector.map @Int @Int @1 f_1) v1_3")
        (tm [strQQ|
          Core.noDeDup
            (Cons @1 @Int @0 (Prim._CO_ @((GHC.Prim.~#) Nat Nat 1 ((GHC.TypeNats.+) 0 1)))
              (f_1 ((case v1_3 of { Cons (m :: Nat) (co :: (GHC.Prim.~#) Nat Nat 1 ((GHC.TypeNats.+) 0 1)) (el :: Int) (res :: Clash.Sized.Vector.Vec 0 Int) -> el }) :: Int))
              (Clash.Sized.Vector.map @Int @Int @0 f_1
                ((case v1_3 of { Cons (m :: Nat) (co :: (GHC.Prim.~#) Nat Nat 1 ((GHC.TypeNats.+) 0 1)) (el :: Int) (res :: Clash.Sized.Vector.Vec 0 Int) -> res }) :: Clash.Sized.Vector.Vec 0 Int)))
        |])
    -- 'head' of a vector of functions, applied to an argument
  , testCase "applies the result of head to the remaining arguments" $
      rewritesAt reducePrimS [] [varD "vf_5" "Clash.Sized.Vector.Vec 1 (Int -> Int)", varD "k_6" "Int"]
        (tm "Clash.Sized.Vector.head @0 @(Int -> Int) vf_5 k_6")
        (tm [strQQ|
          (let
            { (h :: Int -> Int) =
                (case vf_5 of
                  { Cons (m :: Nat) (co :: (GHC.Prim.~#) Nat Nat 1 ((GHC.TypeNats.+) 0 1)) (el :: Int -> Int) (rest :: Clash.Sized.Vector.Vec 0 (Int -> Int)) -> el
                  }) :: Int -> Int
            } in h) k_6
        |])
  , testCase "only fires at the root of an application spine" $
      keepsAt reducePrimS [AppFunStep] [varD "f_1" "Int -> Int", varD "v1_3" "Clash.Sized.Vector.Vec 1 Int", varD "k_6" "Int"]
        (tm "(Clash.Sized.Vector.map @Int @Int @1 f_1 v1_3) k_6")
  , testCase "replaces split# of a zero-width left part by a tuple" $
      rewritesAt reducePrimS [] [varD "k_6" "Int"]
        (tm "Clash.Sized.Internal.BitVector.split# @0 @4 4 k_6")
        (tm "MkT2 @Int @Int k_6 (Clash.Normalize.Primitives.removedArg @Int)")
  , testCase "replaces split# of a zero-width right part, abstracting over the bit vector" $
      rewritesAt reducePrimS [] []
        (tm "Clash.Sized.Internal.BitVector.split# @4 @0 0")
        (tm "\\(bv :: Int) -> MkT2 @Int @Int (Clash.Normalize.Primitives.removedArg @Int) bv")
  , testCase "replaces eq# of zero-width bit vectors by True" $
      rewritesAt reducePrimS [] [varD "k_6" "Int"]
        (tm "Clash.Sized.Internal.BitVector.eq# @0 0 k_6 k_6")
        (tm "True")
  , testCase "keeps eq# of bit vectors that aren't zero-width" $
      keepsAt reducePrimS [] [varD "k_6" "Int"] (tm "Clash.Sized.Internal.BitVector.eq# @1 1 k_6 k_6")
  , testCase "keeps a primitive that has no handler" $
      keepsAt reducePrimS [] [varD "k_6" "Int"] (tm "Prim.somePrim k_6")
  ]

reduceConstTests :: TestTree
reduceConstTests = testGroup "reduceConst"
  [ testCase "evaluates an applied primitive" $
      rewritesAt reduceConstS [] []
        (tm "GHC.Num.Integer.integerAdd (1 :: Integer) (2 :: Integer)")
        (tm "(3 :: Integer)")
  , testCase "only fires at the root of an application spine" $
      keepsAt reduceConstS [AppFunStep] [varD "k_6" "Int"]
        (tm "(GHC.Num.Integer.integerAdd (1 :: Integer) (2 :: Integer)) k_6")
  , testCase "keeps a primitive that doesn't reduce" $
      keepsAt reduceConstS [] [varD "x_10" "GHC.Num.Integer.Integer"]
        (tm "GHC.Num.Integer.integerAdd x_10 (2 :: Integer)")
  , testCase "uses the let-bindings of the context" $
      rewritesAt reduceConstS [LetBodyStep] []
        (tm "let { (x :: GHC.Num.Integer.Integer) = (1 :: Integer) } in GHC.Num.Integer.integerAdd x (2 :: Integer)")
        (tm "let { (x :: GHC.Num.Integer.Integer) = (1 :: Integer) } in (3 :: Integer)")
    -- #2571: a binder between a let-binding of the context and the term
    -- shadows it, so the let-binding must not be used for the variable of
    -- the binder. Scoped, the binders of the context have different names.
  , testCase "doesn't use a let-binding of the context that is shadowed" $
      keepsAt reduceConstS [LetBodyStep, LamStep] []
        (tm [strQQ|
          let { (x :: GHC.Num.Integer.Integer) = (1 :: Integer) } in
            \(x :: GHC.Num.Integer.Integer) -> GHC.Num.Integer.integerAdd x (2 :: Integer)
        |])
  ]

-- | Run 'Scoped.reduceBinders' on the bindings of a let-expression, in the
-- order they have. Gives the names of the binders it substitutes for, with the
-- names of the variables it substitutes, the names of the remaining bindings,
-- and whether it signaled a change.
reduceBindersOn :: TestTerm -> IO ([(Text, Text)], [Text], Bool)
reduceBindersOn input = withTerm input $ \ts tm0 -> do
  ((substituted, kept), _, ch) <- runRewriteTest env st $ case tm0 of
    Let bind body -> openLetBindings (testScope ts) bind body $ \sc' _ lbs _ -> do
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
        "let { (a :: Int) = (f_1 :: Int -> Int) (k_6 :: Int); (b :: Int) = f_1 k_6; (c :: Int) = f_1 b } in c"
      substituted @?= [("b", "a")]
      -- The right-hand side of c refers to a after the substitution
      kept @?= ["c", "a"]
      ch @?= True
  , testCase "compares right-hand sides after substituting" $ do
      (substituted, kept, _) <- reduceBindersOn
        "let { (a :: Int) = (f_1 :: Int -> Int) (k_6 :: Int); (b :: Int) = f_1 k_6; (c :: Int) = f_1 a; (d :: Int) = f_1 b } in d"
      substituted @?= [("b", "a"), ("d", "c")]
      kept @?= ["c", "a"]
  , testCase "keeps bindings marked NoDeDup" $ do
      (substituted, kept, ch) <- reduceBindersOn
        "let { (a :: Int) = (f_1 :: Int -> Int) (k_6 :: Int); (b :: Int) = Core.noDeDup (f_1 k_6) } in f_1 b"
      substituted @?= []
      kept @?= ["b", "a"]
      ch @?= False
  ]

tests :: TestTree
tests = testGroup "Clash.Tests.Normalize.Transformations.Reduce"
  [ reducePrimTests
  , reduceConstTests
  , reduceBindersTests
  ]
