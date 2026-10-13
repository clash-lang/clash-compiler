{-|
  Copyright   :  (C) 2026, QBayLogic B.V.
  License     :  BSD2 (see the file LICENSE)
  Maintainer  :  QBayLogic B.V. <devops@qbaylogic.com>

  Tests for the primitive reductions of
  "Clash.Normalize.Scoped.Transformations.Reduce": regression tests, and
  tests of the results of the reductions. 'assertRewritesTo'
  runs every test in the scope of the free variables of its input, and in a
  scope that the binders of the input clash with.
-}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Clash.Tests.Normalize.Scoped.PrimitiveReductions (tests) where

import qualified Data.Either as Either
import Data.Default (def)
import qualified Data.IntMap.Strict as IntMap
import qualified Data.List as List

import Test.Tasty
import Test.Tasty.HUnit

import qualified Clash.Core.Basic as C (IsMultiPrim (SingleResult), WorkInfo (WorkNever))
import Clash.Core.Scoped.Syntax (mkPrimInfo)
import Clash.Core.Scoped.Syntax (PrimInfo, PrimUnfolding (NoUnfolding), Term (Prim))
import Clash.Core.Scoped.Term (collectArgs)
import Clash.Normalize.Scoped.Transformations.Reduce (PrimReduction (..))
import qualified Clash.Normalize.Scoped.Transformations.Reduce as Scoped
import Clash.Normalize.Types (NormalizeState)
import Clash.Rewrite.Scoped (ScopedRewrite, ScopedTransform (..))
import Clash.Rewrite.Types (RewriteEnv (..), RewriteState (..))

import Test.Clash.Rewrite
  ( TestDefs (..), TestTerm, kindedTyVarD, parseTestTermWith
  , parseTestType, strQQ, testGlobalId, testTyConMap, withTyConMap )
import Test.Clash.Scoped (ScopeDecl, assertRewritesTo, closedType, varD, withCurFun)

-- | The scoped primitive reduction, as a transformation of the saturated
-- application of the primitive
reductionS :: PrimReduction -> ScopedRewrite NormalizeState
reductionS red = ScopedTransform $ \ctx e ->
  case collectArgs e of
    (Prim p, args) ->
      let (tms, tys) = Either.partitionEithers args
      in Scoped.runPrimReduction ctx red p tys tms
    _ -> error "reductionS: not a primitive application"

-- | The primitives of the tests, which do no work. Their type is @Int@, unless
-- a test annotates them with another one.
prims :: [PrimInfo]
prims =
  [ mkPrimInfo ("Clash.Sized." <> nm) (closedType "Int") C.WorkNever C.SingleResult NoUnfolding
  | nm <-
      [ "Vector.zipWith", "Vector.map", "Vector.traverse#", "Vector.fold", "Vector.foldr"
      , "Vector.dfold", "Vector.++", "Vector.head", "Vector.tail", "Vector.last", "Vector.init"
      , "Vector.unconcat", "Vector.transpose", "Vector.replicate", "Vector.replace_int"
      , "Vector.index_int", "Vector.imap", "Vector.iterateI", "Vector.dtfold", "Vector.reverse"
      , "RTree.tdfold", "RTree.treplicate" ] ]

-- | Terms that mention the primitives of the tests, and a global function
-- @f :: a -> a -> a@, where @a@ is a function type of many arguments
tm :: String -> TestTerm
tm = parseTestTermWith (TestDefs [] prims [testGlobalId "f_G2" (parseTestType (a <> " -> " <> a <> " -> " <> a))])
 where
  a = "(" <> List.intercalate " -> " (replicate 17 "Int") <> ")"

-- | The environment of the reductions: the type constructors of
-- 'testTyConMap'
env :: RewriteEnv
env = withTyConMap testTyConMap def

st :: RewriteState NormalizeState
st = withCurFun (testGlobalId "top_G100" "Int") def {_globalHeap = (IntMap.empty, 0)}

-- | Assert that a reduction rewrites the saturated application of a primitive
-- to the expected term
reduces :: PrimReduction -> [ScopeDecl] -> TestTerm -> TestTerm -> Assertion
reduces red = assertRewritesTo env st (reductionS red)

tests :: TestTree
tests = testGroup "Clash.Tests.Normalize.Scoped.PrimitiveReductions"
  [ testGroup "regression tests"
      [ testCase "reduceReverse on an empty vector" $
          reduces (ReduceReverse 0) [varD "v_1" "Clash.Sized.Vector.Vec 0 Int"]
            (tm "Clash.Sized.Vector.reverse @0 @Int v_1")
            (tm "Nil @0 @Int (Prim._CO_ @((GHC.Prim.~#) Nat Nat 0 0))")
      , testCase "reduceReplace_int on an empty vector" $
          reduces (ReduceReplaceInt 0) [varD "v_1" "Clash.Sized.Vector.Vec 0 Int", varD "i_2" "Int", varD "a_3" "Int"]
            (tm [strQQ|
              (Clash.Sized.Vector.replace_int :: forall (n :: Nat) (a :: Type). Nat -> Clash.Sized.Vector.Vec n a -> Int -> a -> Clash.Sized.Vector.Vec n a) @0 @Int (0 :: Natural) v_1 i_2 a_3
            |])
            (tm "Nil @0 @Int (Prim._CO_ @((GHC.Prim.~#) Nat Nat 0 0))")
      , testCase "reduceMap binds the elements of a vector at their type" $
          reduces (ReduceMap 1 False) [kindedTyVarD "a_0" "Type", varD "v_1000" "Clash.Sized.Vector.Vec 1 a_0", varD "f_1001" "a_0 -> a_0"]
            (tm [strQQ|
              (Clash.Sized.Vector.map :: forall (a :: Type) (b :: Type) (n :: Nat). (a -> b) -> Clash.Sized.Vector.Vec n a -> Clash.Sized.Vector.Vec n b) @a_0 @a_0 @1 f_1001 v_1000
            |])
            (tm [strQQ|
              Cons @1 @a_0 @0 (Prim._CO_ @((GHC.Prim.~#) Nat Nat 1 ((GHC.TypeNats.+) 0 1))) (f_1001 ((case v_1000 of
                { Cons (m_S :: Nat) (co_S :: (GHC.Prim.~#) Nat Nat 1 ((GHC.TypeNats.+) 0 1)) (el_S :: a_0) (res_S :: Clash.Sized.Vector.Vec 0 a_0) -> el_S
                }) :: a_0)) ((Clash.Sized.Vector.map :: forall (a :: Type) (b :: Type) (n :: Nat). (a -> b) -> Clash.Sized.Vector.Vec n a -> Clash.Sized.Vector.Vec n b) @a_0 @a_0 @0 f_1001 ((case v_1000 of
                { Cons (m_S :: Nat) (co_S :: (GHC.Prim.~#) Nat Nat 1 ((GHC.TypeNats.+) 0 1)) (el_S :: a_0) (res_S :: Clash.Sized.Vector.Vec 0 a_0) -> res_S
                }) :: Clash.Sized.Vector.Vec 0 a_0))
            |])
      , testCase "reduceFold does not rewind the unique supply" $
          reduces (ReduceFold 2) [varD "v_1" "Clash.Sized.Vector.Vec 2 (Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int)"]
            (tm [strQQ|
              Clash.Sized.Vector.fold @1 @(Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int) f_G2 v_1
            |])
            (tm [strQQ|
              let
                { (elF0_S :: Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int) = (case v_1 of
                    { Cons (m_S :: Nat) (co_S :: (GHC.Prim.~#) Nat Nat 2 ((GHC.TypeNats.+) 1 1)) (el_S :: Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int) (rest_S :: Clash.Sized.Vector.Vec 1 (Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int)) -> el_S
                    }) :: Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int
                ; (restF0_S3 :: Clash.Sized.Vector.Vec 1 (Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int)) = (case v_1 of
                    { Cons (m_S :: Nat) (co_S :: (GHC.Prim.~#) Nat Nat 2 ((GHC.TypeNats.+) 1 1)) (el_S :: Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int) (rest_S :: Clash.Sized.Vector.Vec 1 (Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int)) -> rest_S
                    }) :: Clash.Sized.Vector.Vec 1 (Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int)
                ; (elF1_S :: Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int) = (case restF0_S3 of
                    { Cons (m_S :: Nat) (co_S :: (GHC.Prim.~#) Nat Nat 1 ((GHC.TypeNats.+) 0 1)) (el_S :: Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int) (rest_S :: Clash.Sized.Vector.Vec 0 (Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int)) -> el_S
                    }) :: Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int
                } in (\(carg_I :: Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int) (carg_I1000003 :: Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int) (carg_I4 :: Int) (carg_I5 :: Int) (carg_I6 :: Int) (carg_I7 :: Int) (carg_I8 :: Int) (carg_I9 :: Int) (carg_I10 :: Int) (carg_I11 :: Int) (carg_I12 :: Int) (carg_I13 :: Int) (carg_I14 :: Int) (carg_I15 :: Int) (carg_I16 :: Int) (carg_I17 :: Int) (carg_I18 :: Int) (carg_I19 :: Int) -> f_G2 carg_I carg_I1000003 carg_I4 carg_I5 carg_I6 carg_I7 carg_I8 carg_I9 carg_I10 carg_I11 carg_I12 carg_I13 carg_I14 carg_I15 carg_I16 carg_I17 carg_I18 carg_I19) elF0_S elF1_S
            |])
      , testCase "reduceMap on an empty vector gives an empty vector of the result element type" $
          reduces (ReduceMap 0 True) [varD "v_1" "Clash.Sized.Vector.Vec 0 Int", varD "f_4" "Int -> Int -> Int"]
            (tm [strQQ|
              (Clash.Sized.Vector.map :: forall (a :: Type) (b :: Type) (n :: Nat). (a -> b) -> Clash.Sized.Vector.Vec n a -> Clash.Sized.Vector.Vec n b) @Int @(Int -> Int) @0 f_4 v_1
            |])
            (tm [strQQ|
              Nil @0 @(Int -> Int) (Prim._CO_ @((GHC.Prim.~#) Nat Nat 0 0))
            |])
      ]
  , testGroup "reductions"
      [ testCase "zipWith, let-binding a vector" $
          reduces (ReduceZipWith 2 True True) [varD "g_2" "Int -> Int -> Int", varD "mkV_3" "Int -> Clash.Sized.Vector.Vec 2 Int", varD "v2_22" "Clash.Sized.Vector.Vec 2 Int", varD "k_30" "Int"]
            (tm "Clash.Sized.Vector.zipWith @Int @Int @Int @2 g_2 v2_22 (mkV_3 k_30)")
            (tm [strQQ|
              let { !(cvec_I :: Clash.Sized.Vector.Vec 2 Int) = mkV_3 k_30 } in Cons @2 @Int @1 (Prim._CO_ @((GHC.Prim.~#) Nat Nat 2 ((GHC.TypeNats.+) 1 1))) (g_2 ((case v2_22 of
                { Cons (m_S :: Nat) (co_S :: (GHC.Prim.~#) Nat Nat 2 ((GHC.TypeNats.+) 1 1)) (el_S :: Int) (res_S :: Clash.Sized.Vector.Vec 1 Int) -> el_S
                }) :: Int) ((case cvec_I of
                { Cons (m_S :: Nat) (co_S :: (GHC.Prim.~#) Nat Nat 2 ((GHC.TypeNats.+) 1 1)) (el_S :: Int) (res_S :: Clash.Sized.Vector.Vec 1 Int) -> el_S
                }) :: Int)) (Clash.Sized.Vector.zipWith @Int @Int @Int @1 g_2 ((case v2_22 of
                { Cons (m_S :: Nat) (co_S :: (GHC.Prim.~#) Nat Nat 2 ((GHC.TypeNats.+) 1 1)) (el_S :: Int) (res_S :: Clash.Sized.Vector.Vec 1 Int) -> res_S
                }) :: Clash.Sized.Vector.Vec 1 Int) ((case cvec_I of
                { Cons (m_S :: Nat) (co_S :: (GHC.Prim.~#) Nat Nat 2 ((GHC.TypeNats.+) 1 1)) (el_S :: Int) (res_S :: Clash.Sized.Vector.Vec 1 Int) -> res_S
                }) :: Clash.Sized.Vector.Vec 1 Int))
            |])
      , testCase "zipWith of non-representable vectors" $
          reduces (ReduceZipWith 2 False False) [varD "g_2" "Int -> Int -> Int", varD "mkV_3" "Int -> Clash.Sized.Vector.Vec 2 Int", varD "k_30" "Int", varD "z_31" "Int"]
            (tm "Clash.Sized.Vector.zipWith @Int @Int @Int @2 g_2 (mkV_3 k_30) (mkV_3 z_31)")
            (tm [strQQ|
              Cons @2 @Int @1 (Prim._CO_ @((GHC.Prim.~#) Nat Nat 2 ((GHC.TypeNats.+) 1 1))) (g_2 ((case mkV_3 k_30 of
                { Cons (m_S :: Nat) (co_S :: (GHC.Prim.~#) Nat Nat 2 ((GHC.TypeNats.+) 1 1)) (el_S :: Int) (res_S :: Clash.Sized.Vector.Vec 1 Int) -> el_S
                }) :: Int) ((case mkV_3 z_31 of
                { Cons (m_S :: Nat) (co_S :: (GHC.Prim.~#) Nat Nat 2 ((GHC.TypeNats.+) 1 1)) (el_S :: Int) (res_S :: Clash.Sized.Vector.Vec 1 Int) -> el_S
                }) :: Int)) (Clash.Sized.Vector.zipWith @Int @Int @Int @1 g_2 ((case mkV_3 k_30 of
                { Cons (m_S :: Nat) (co_S :: (GHC.Prim.~#) Nat Nat 2 ((GHC.TypeNats.+) 1 1)) (el_S :: Int) (res_S :: Clash.Sized.Vector.Vec 1 Int) -> res_S
                }) :: Clash.Sized.Vector.Vec 1 Int) ((case mkV_3 z_31 of
                { Cons (m_S :: Nat) (co_S :: (GHC.Prim.~#) Nat Nat 2 ((GHC.TypeNats.+) 1 1)) (el_S :: Int) (res_S :: Clash.Sized.Vector.Vec 1 Int) -> res_S
                }) :: Clash.Sized.Vector.Vec 1 Int))
            |])
      , testCase "zipWith on empty vectors" $
          reduces (ReduceZipWith 0 True True) [varD "g_2" "Int -> Int -> Int", varD "v0_20" "Clash.Sized.Vector.Vec 0 Int"]
            (tm "Clash.Sized.Vector.zipWith @Int @Int @Int @0 g_2 v0_20 v0_20")
            (tm "Nil @0 @Int (Prim._CO_ @((GHC.Prim.~#) Nat Nat 0 0))")
      , testCase "map, let-binding the vector" $
          reduces (ReduceMap 2 True) [varD "f_1" "Int -> Int", varD "mkV_3" "Int -> Clash.Sized.Vector.Vec 2 Int", varD "k_30" "Int"]
            (tm "Clash.Sized.Vector.map @Int @Int @2 f_1 (mkV_3 k_30)")
            (tm [strQQ|
              let { !(cvec_I :: Clash.Sized.Vector.Vec 2 Int) = mkV_3 k_30 } in Cons @2 @Int @1 (Prim._CO_ @((GHC.Prim.~#) Nat Nat 2 ((GHC.TypeNats.+) 1 1))) (f_1 ((case cvec_I of
                { Cons (m_S :: Nat) (co_S :: (GHC.Prim.~#) Nat Nat 2 ((GHC.TypeNats.+) 1 1)) (el_S :: Int) (res_S :: Clash.Sized.Vector.Vec 1 Int) -> el_S
                }) :: Int)) (Clash.Sized.Vector.map @Int @Int @1 f_1 ((case cvec_I of
                { Cons (m_S :: Nat) (co_S :: (GHC.Prim.~#) Nat Nat 2 ((GHC.TypeNats.+) 1 1)) (el_S :: Int) (res_S :: Clash.Sized.Vector.Vec 1 Int) -> res_S
                }) :: Clash.Sized.Vector.Vec 1 Int))
            |])
      , testCase "map on an empty vector" $
          reduces (ReduceMap 0 True) [varD "f_1" "Int -> Int", varD "v0_20" "Clash.Sized.Vector.Vec 0 Int"]
            (tm "Clash.Sized.Vector.map @Int @Int @0 f_1 v0_20")
            (tm "Nil @0 @Int (Prim._CO_ @((GHC.Prim.~#) Nat Nat 0 0))")
      , testCase "traverse#" $
          reduces (ReduceTraverse 2) [varD "dict_4" "GHC.Base.Applicative Maybe", varD "h_5" "Int -> Maybe Int", varD "v2_22" "Clash.Sized.Vector.Vec 2 Int"]
            (tm "Clash.Sized.Vector.traverse# @Int @Maybe @Int @2 dict_4 h_5 v2_22")
            (tm [strQQ|
              let
                { (cfunctorDict_I23 :: GHC.Base.Functor Maybe) = (case dict_4 of
                    { CApplicative (functorDict_S27 :: GHC.Base.Functor Maybe) (pure_S :: forall (a :: Type). a -> Maybe a) (ap_S :: forall (a :: Type) (b :: Type). Maybe (a -> b) -> Maybe a -> Maybe b) (liftA2_S30 :: Int) (apConstL_S31 :: Int) (apConstR_S32 :: Int) -> functorDict_S27
                    }) :: GHC.Base.Functor Maybe
                ; (cpure_I24 :: forall (a :: Type). a -> Maybe a) = (case dict_4 of
                    { CApplicative (functorDict_S27 :: GHC.Base.Functor Maybe) (pure_S :: forall (a :: Type). a -> Maybe a) (ap_S :: forall (a :: Type) (b :: Type). Maybe (a -> b) -> Maybe a -> Maybe b) (liftA2_S30 :: Int) (apConstL_S31 :: Int) (apConstR_S32 :: Int) -> pure_S
                    }) :: forall (a :: Type). a -> Maybe a
                ; (cap_I :: forall (a :: Type) (b :: Type). Maybe (a -> b) -> Maybe a -> Maybe b) = (case dict_4 of
                    { CApplicative (functorDict_S27 :: GHC.Base.Functor Maybe) (pure_S :: forall (a :: Type). a -> Maybe a) (ap_S :: forall (a :: Type) (b :: Type). Maybe (a -> b) -> Maybe a -> Maybe b) (liftA2_S30 :: Int) (apConstL_S31 :: Int) (apConstR_S32 :: Int) -> ap_S
                    }) :: forall (a :: Type) (b :: Type). Maybe (a -> b) -> Maybe a -> Maybe b
                ; (cfmap_I26 :: forall (a :: Type) (b :: Type). (a -> b) -> Maybe a -> Maybe b) = (case cfunctorDict_I23 of
                    { CFunctor (fmap_S :: forall (a :: Type) (b :: Type). (a -> b) -> Maybe a -> Maybe b) (fmapConst_S1000028 :: Int) -> fmap_S
                    }) :: forall (a :: Type) (b :: Type). (a -> b) -> Maybe a -> Maybe b
                ; (elT0_S :: Int) = (case v2_22 of
                    { Cons (m_S :: Nat) (co_S :: (GHC.Prim.~#) Nat Nat 2 ((GHC.TypeNats.+) 1 1)) (el_S :: Int) (rest_S :: Clash.Sized.Vector.Vec 1 Int) -> el_S
                    }) :: Int
                ; (restT0_S28 :: Clash.Sized.Vector.Vec 1 Int) = (case v2_22 of
                    { Cons (m_S :: Nat) (co_S :: (GHC.Prim.~#) Nat Nat 2 ((GHC.TypeNats.+) 1 1)) (el_S :: Int) (rest_S :: Clash.Sized.Vector.Vec 1 Int) -> rest_S
                    }) :: Clash.Sized.Vector.Vec 1 Int
                ; (elT1_S :: Int) = (case restT0_S28 of
                    { Cons (m_S :: Nat) (co_S :: (GHC.Prim.~#) Nat Nat 1 ((GHC.TypeNats.+) 0 1)) (el_S :: Int) (rest_S :: Clash.Sized.Vector.Vec 0 Int) -> el_S
                    }) :: Int
                } in cap_I @(Clash.Sized.Vector.Vec 1 Int) @(Clash.Sized.Vector.Vec 2 Int) (cfmap_I26 @Int @(Clash.Sized.Vector.Vec 1 Int -> Clash.Sized.Vector.Vec 2 Int) (Cons @2 @Int @1 (Prim._CO_ @((GHC.Prim.~#) Nat Nat 2 ((GHC.TypeNats.+) 1 1)))) ((\(carg_I :: Int) -> h_5 carg_I) elT0_S)) (cap_I @(Clash.Sized.Vector.Vec 0 Int) @(Clash.Sized.Vector.Vec 1 Int) (cfmap_I26 @Int @(Clash.Sized.Vector.Vec 0 Int -> Clash.Sized.Vector.Vec 1 Int) (Cons @1 @Int @0 (Prim._CO_ @((GHC.Prim.~#) Nat Nat 1 ((GHC.TypeNats.+) 0 1)))) ((\(carg_I :: Int) -> h_5 carg_I) elT1_S)) (cpure_I24 @(Clash.Sized.Vector.Vec 0 Int) (Nil @0 @Int (Prim._CO_ @((GHC.Prim.~#) Nat Nat 0 0)))))
            |])
      , testCase "traverse# on an empty vector" $
          reduces (ReduceTraverse 0) [varD "dict_4" "GHC.Base.Applicative Maybe", varD "h_5" "Int -> Maybe Int", varD "v0_20" "Clash.Sized.Vector.Vec 0 Int"]
            (tm "Clash.Sized.Vector.traverse# @Int @Maybe @Int @0 dict_4 h_5 v0_20")
            (tm [strQQ|
              let
                { (cfunctorDict_I21 :: GHC.Base.Functor Maybe) = (case dict_4 of
                    { CApplicative (functorDict_S25 :: GHC.Base.Functor Maybe) (pure_S :: forall (a :: Type). a -> Maybe a) (ap_S :: forall (a :: Type) (b :: Type). Maybe (a -> b) -> Maybe a -> Maybe b) (liftA2_S28 :: Int) (apConstL_S29 :: Int) (apConstR_S30 :: Int) -> functorDict_S25
                    }) :: GHC.Base.Functor Maybe
                ; (cpure_I22 :: forall (a :: Type). a -> Maybe a) = (case dict_4 of
                    { CApplicative (functorDict_S25 :: GHC.Base.Functor Maybe) (pure_S :: forall (a :: Type). a -> Maybe a) (ap_S :: forall (a :: Type) (b :: Type). Maybe (a -> b) -> Maybe a -> Maybe b) (liftA2_S28 :: Int) (apConstL_S29 :: Int) (apConstR_S30 :: Int) -> pure_S
                    }) :: forall (a :: Type). a -> Maybe a
                ; (cap_I :: forall (a :: Type) (b :: Type). Maybe (a -> b) -> Maybe a -> Maybe b) = (case dict_4 of
                    { CApplicative (functorDict_S25 :: GHC.Base.Functor Maybe) (pure_S :: forall (a :: Type). a -> Maybe a) (ap_S :: forall (a :: Type) (b :: Type). Maybe (a -> b) -> Maybe a -> Maybe b) (liftA2_S28 :: Int) (apConstL_S29 :: Int) (apConstR_S30 :: Int) -> ap_S
                    }) :: forall (a :: Type) (b :: Type). Maybe (a -> b) -> Maybe a -> Maybe b
                ; (cfmap_I24 :: forall (a :: Type) (b :: Type). (a -> b) -> Maybe a -> Maybe b) = (case cfunctorDict_I21 of
                    { CFunctor (fmap_S :: forall (a :: Type) (b :: Type). (a -> b) -> Maybe a -> Maybe b) (fmapConst_S26 :: Int) -> fmap_S
                    }) :: forall (a :: Type) (b :: Type). (a -> b) -> Maybe a -> Maybe b
                } in cpure_I22 @(Clash.Sized.Vector.Vec 0 Int) (Nil @0 @Int (Prim._CO_ @((GHC.Prim.~#) Nat Nat 0 0)))
            |])
      , testCase "fold" $
          reduces (ReduceFold 3) [varD "g_2" "Int -> Int -> Int", varD "v3_23" "Clash.Sized.Vector.Vec 3 Int"]
            (tm "Clash.Sized.Vector.fold @2 @Int g_2 v3_23")
            (tm [strQQ|
              let
                { (elF0_S :: Int) = (case v3_23 of
                    { Cons (m_S :: Nat) (co_S :: (GHC.Prim.~#) Nat Nat 3 ((GHC.TypeNats.+) 2 1)) (el_S :: Int) (rest_S :: Clash.Sized.Vector.Vec 2 Int) -> el_S
                    }) :: Int
                ; (restF0_S25 :: Clash.Sized.Vector.Vec 2 Int) = (case v3_23 of
                    { Cons (m_S :: Nat) (co_S :: (GHC.Prim.~#) Nat Nat 3 ((GHC.TypeNats.+) 2 1)) (el_S :: Int) (rest_S :: Clash.Sized.Vector.Vec 2 Int) -> rest_S
                    }) :: Clash.Sized.Vector.Vec 2 Int
                ; (elF1_S :: Int) = (case restF0_S25 of
                    { Cons (m_S :: Nat) (co_S :: (GHC.Prim.~#) Nat Nat 2 ((GHC.TypeNats.+) 1 1)) (el_S :: Int) (rest_S :: Clash.Sized.Vector.Vec 1 Int) -> el_S
                    }) :: Int
                ; (restF1_S27 :: Clash.Sized.Vector.Vec 1 Int) = (case restF0_S25 of
                    { Cons (m_S :: Nat) (co_S :: (GHC.Prim.~#) Nat Nat 2 ((GHC.TypeNats.+) 1 1)) (el_S :: Int) (rest_S :: Clash.Sized.Vector.Vec 1 Int) -> rest_S
                    }) :: Clash.Sized.Vector.Vec 1 Int
                ; (elF2_S :: Int) = (case restF1_S27 of
                    { Cons (m_S :: Nat) (co_S :: (GHC.Prim.~#) Nat Nat 1 ((GHC.TypeNats.+) 0 1)) (el_S :: Int) (rest_S :: Clash.Sized.Vector.Vec 0 Int) -> el_S
                    }) :: Int
                } in (\(carg_I :: Int) (carg_I1000025 :: Int) -> g_2 carg_I carg_I1000025) elF0_S ((\(carg_I :: Int) (carg_I1000025 :: Int) -> g_2 carg_I carg_I1000025) elF1_S elF2_S)
            |])
      , testCase "foldr, let-binding the vector" $
          reduces (ReduceFoldr 2 True) [varD "g_2" "Int -> Int -> Int", varD "mkV_3" "Int -> Clash.Sized.Vector.Vec 2 Int", varD "k_30" "Int", varD "z_31" "Int"]
            (tm "Clash.Sized.Vector.foldr @Int @Int @2 g_2 z_31 (mkV_3 k_30)")
            (tm [strQQ|
              let { !(cvec_I :: Clash.Sized.Vector.Vec 2 Int) = mkV_3 k_30 } in g_2 ((case cvec_I of
                { Cons (m_S :: Nat) (co_S :: (GHC.Prim.~#) Nat Nat 2 ((GHC.TypeNats.+) 1 1)) (el_S :: Int) (res_S :: Clash.Sized.Vector.Vec 1 Int) -> el_S
                }) :: Int) (Clash.Sized.Vector.foldr @Int @Int @1 g_2 z_31 ((case cvec_I of
                { Cons (m_S :: Nat) (co_S :: (GHC.Prim.~#) Nat Nat 2 ((GHC.TypeNats.+) 1 1)) (el_S :: Int) (res_S :: Clash.Sized.Vector.Vec 1 Int) -> res_S
                }) :: Clash.Sized.Vector.Vec 1 Int))
            |])
      , testCase "foldr on an empty vector" $
          reduces (ReduceFoldr 0 True) [varD "g_2" "Int -> Int -> Int", varD "v0_20" "Clash.Sized.Vector.Vec 0 Int", varD "z_31" "Int"]
            (tm "Clash.Sized.Vector.foldr @Int @Int @0 g_2 z_31 v0_20")
            (tm "z_31")
      , testCase "dfold" $
          reduces (ReduceDFold 2) [varD "df_6" "forall (l :: Nat). UBP_2035 ((GHC.TypeNats.+) l 1) 2 -> Clash.Promoted.Nat.SNat l -> Int -> Int -> Int", varD "p_9" "Int", varD "v2_22" "Clash.Sized.Vector.Vec 2 Int", varD "z_31" "Int"]
            (tm "Clash.Sized.Vector.dfold @Int @2 @Int (2 :: Natural) p_9 df_6 z_31 v2_22")
            (tm [strQQ|
              let
                { (elD0_S :: Int) = (case v2_22 of
                    { Cons (m_S :: Nat) (co_S :: (GHC.Prim.~#) Nat Nat 2 ((GHC.TypeNats.+) 1 1)) (el_S :: Int) (rest_S :: Clash.Sized.Vector.Vec 1 Int) -> el_S
                    }) :: Int
                ; (restD0_S33 :: Clash.Sized.Vector.Vec 1 Int) = (case v2_22 of
                    { Cons (m_S :: Nat) (co_S :: (GHC.Prim.~#) Nat Nat 2 ((GHC.TypeNats.+) 1 1)) (el_S :: Int) (rest_S :: Clash.Sized.Vector.Vec 1 Int) -> rest_S
                    }) :: Clash.Sized.Vector.Vec 1 Int
                ; (elD1_S :: Int) = (case restD0_S33 of
                    { Cons (m_S :: Nat) (co_S :: (GHC.Prim.~#) Nat Nat 1 ((GHC.TypeNats.+) 0 1)) (el_S :: Int) (rest_S :: Clash.Sized.Vector.Vec 0 Int) -> el_S
                    }) :: Int
                } in df_6 @1 MkUBP (SNat @1 (1 :: Natural)) elD0_S (df_6 @0 MkUBP (SNat @0 (0 :: Natural)) elD1_S z_31)
            |])
      , testCase "++" $
          reduces (ReduceAppend 2 1) [varD "v1_21" "Clash.Sized.Vector.Vec 1 Int", varD "v2_22" "Clash.Sized.Vector.Vec 2 Int"]
            (tm "(Clash.Sized.Vector.++) @2 @Int @1 v2_22 v1_21")
            (tm [strQQ|
              let
                { (elC0_S :: Int) = (case v2_22 of
                    { Cons (m_S :: Nat) (co_S :: (GHC.Prim.~#) Nat Nat 2 ((GHC.TypeNats.+) 1 1)) (el_S :: Int) (rest_S :: Clash.Sized.Vector.Vec 1 Int) -> el_S
                    }) :: Int
                ; (restC0_S24 :: Clash.Sized.Vector.Vec 1 Int) = (case v2_22 of
                    { Cons (m_S :: Nat) (co_S :: (GHC.Prim.~#) Nat Nat 2 ((GHC.TypeNats.+) 1 1)) (el_S :: Int) (rest_S :: Clash.Sized.Vector.Vec 1 Int) -> rest_S
                    }) :: Clash.Sized.Vector.Vec 1 Int
                ; (elC1_S :: Int) = (case restC0_S24 of
                    { Cons (m_S :: Nat) (co_S :: (GHC.Prim.~#) Nat Nat 1 ((GHC.TypeNats.+) 0 1)) (el_S :: Int) (rest_S :: Clash.Sized.Vector.Vec 0 Int) -> el_S
                    }) :: Int
                } in Cons @3 @Int @2 (Prim._CO_ @((GHC.Prim.~#) Nat Nat 3 ((GHC.TypeNats.+) 2 1))) elC0_S (Cons @2 @Int @1 (Prim._CO_ @((GHC.Prim.~#) Nat Nat 2 ((GHC.TypeNats.+) 1 1))) elC1_S v1_21)
            |])
      , testCase "head" $
          reduces (ReduceHead 2) [varD "v2_22" "Clash.Sized.Vector.Vec 2 Int"]
            (tm "Clash.Sized.Vector.head @1 @Int v2_22")
            (tm [strQQ|
              let
                { (elH0_S :: Int) = (case v2_22 of
                    { Cons (m_S :: Nat) (co_S :: (GHC.Prim.~#) Nat Nat 2 ((GHC.TypeNats.+) 1 1)) (el_S :: Int) (rest_S :: Clash.Sized.Vector.Vec 1 Int) -> el_S
                    }) :: Int
                } in elH0_S
            |])
      , testCase "tail" $
          reduces (ReduceTail 2) [varD "v2_22" "Clash.Sized.Vector.Vec 2 Int"]
            (tm "Clash.Sized.Vector.tail @1 @Int v2_22")
            (tm [strQQ|
              let
                { (restL0_S23 :: Clash.Sized.Vector.Vec 1 Int) = (case v2_22 of
                    { Cons (m_S :: Nat) (co_S :: (GHC.Prim.~#) Nat Nat 2 ((GHC.TypeNats.+) 1 1)) (el_S :: Int) (rest_S :: Clash.Sized.Vector.Vec 1 Int) -> rest_S
                    }) :: Clash.Sized.Vector.Vec 1 Int
                } in restL0_S23
            |])
      , testCase "last" $
          reduces (ReduceLast 3) [varD "v3_23" "Clash.Sized.Vector.Vec 3 Int"]
            (tm "Clash.Sized.Vector.last @2 @Int v3_23")
            (tm [strQQ|
              let
                { (elL0_S :: Int) = (case v3_23 of
                    { Cons (m_S :: Nat) (co_S :: (GHC.Prim.~#) Nat Nat 3 ((GHC.TypeNats.+) 2 1)) (el_S :: Int) (rest_S :: Clash.Sized.Vector.Vec 2 Int) -> el_S
                    }) :: Int
                ; (restL0_S25 :: Clash.Sized.Vector.Vec 2 Int) = (case v3_23 of
                    { Cons (m_S :: Nat) (co_S :: (GHC.Prim.~#) Nat Nat 3 ((GHC.TypeNats.+) 2 1)) (el_S :: Int) (rest_S :: Clash.Sized.Vector.Vec 2 Int) -> rest_S
                    }) :: Clash.Sized.Vector.Vec 2 Int
                ; (elL1_S :: Int) = (case restL0_S25 of
                    { Cons (m_S :: Nat) (co_S :: (GHC.Prim.~#) Nat Nat 2 ((GHC.TypeNats.+) 1 1)) (el_S :: Int) (rest_S :: Clash.Sized.Vector.Vec 1 Int) -> el_S
                    }) :: Int
                ; (restL1_S27 :: Clash.Sized.Vector.Vec 1 Int) = (case restL0_S25 of
                    { Cons (m_S :: Nat) (co_S :: (GHC.Prim.~#) Nat Nat 2 ((GHC.TypeNats.+) 1 1)) (el_S :: Int) (rest_S :: Clash.Sized.Vector.Vec 1 Int) -> rest_S
                    }) :: Clash.Sized.Vector.Vec 1 Int
                ; (elL2_S :: Int) = (case restL1_S27 of
                    { Cons (m_S :: Nat) (co_S :: (GHC.Prim.~#) Nat Nat 1 ((GHC.TypeNats.+) 0 1)) (el_S :: Int) (rest_S :: Clash.Sized.Vector.Vec 0 Int) -> el_S
                    }) :: Int
                } in elL2_S
            |])
      , testCase "init" $
          reduces (ReduceInit 2 True) [varD "v3_23" "Clash.Sized.Vector.Vec 3 Int"]
            (tm "Clash.Sized.Vector.init @2 @Int v3_23")
            (tm [strQQ|
              Cons @2 @Int @1 (Prim._CO_ @((GHC.Prim.~#) Nat Nat 2 ((GHC.TypeNats.+) 1 1))) ((case v3_23 of
                { Cons (m_S :: Nat) (co_S :: (GHC.Prim.~#) Nat Nat 3 ((GHC.TypeNats.+) 2 1)) (el_S :: Int) (res_S :: Clash.Sized.Vector.Vec 2 Int) -> el_S
                }) :: Int) (Clash.Sized.Vector.init @1 @Int ((case v3_23 of
                { Cons (m_S :: Nat) (co_S :: (GHC.Prim.~#) Nat Nat 3 ((GHC.TypeNats.+) 2 1)) (el_S :: Int) (res_S :: Clash.Sized.Vector.Vec 2 Int) -> res_S
                }) :: Clash.Sized.Vector.Vec 2 Int))
            |])
      , testCase "init to an empty vector" $
          reduces (ReduceInit 0 True) [varD "v1_21" "Clash.Sized.Vector.Vec 1 Int"]
            (tm "Clash.Sized.Vector.init @0 @Int v1_21")
            (tm "Nil @0 @Int (Prim._CO_ @((GHC.Prim.~#) Nat Nat 0 0))")
      , testCase "unconcat" $
          reduces (ReduceUnconcat 2 2) [varD "v4_24" "Clash.Sized.Vector.Vec 4 Int"]
            (tm [strQQ|
              Clash.Sized.Vector.unconcat @2 @2 @Int (2 :: Natural) (SNat @2 (2 :: Natural)) v4_24
            |])
            (tm [strQQ|
              let
                { (elU0_S :: Int) = (case v4_24 of
                    { Cons (m_S :: Nat) (co_S :: (GHC.Prim.~#) Nat Nat 4 ((GHC.TypeNats.+) 3 1)) (el_S :: Int) (rest_S :: Clash.Sized.Vector.Vec 3 Int) -> el_S
                    }) :: Int
                ; (restU0_S26 :: Clash.Sized.Vector.Vec 3 Int) = (case v4_24 of
                    { Cons (m_S :: Nat) (co_S :: (GHC.Prim.~#) Nat Nat 4 ((GHC.TypeNats.+) 3 1)) (el_S :: Int) (rest_S :: Clash.Sized.Vector.Vec 3 Int) -> rest_S
                    }) :: Clash.Sized.Vector.Vec 3 Int
                ; (elU1_S :: Int) = (case restU0_S26 of
                    { Cons (m_S :: Nat) (co_S :: (GHC.Prim.~#) Nat Nat 3 ((GHC.TypeNats.+) 2 1)) (el_S :: Int) (rest_S :: Clash.Sized.Vector.Vec 2 Int) -> el_S
                    }) :: Int
                } in Cons @2 @(Clash.Sized.Vector.Vec 2 Int) @1 (Prim._CO_ @((GHC.Prim.~#) Nat Nat 2 ((GHC.TypeNats.+) 1 1))) (Cons @2 @Int @1 (Prim._CO_ @((GHC.Prim.~#) Nat Nat 2 ((GHC.TypeNats.+) 1 1))) elU0_S (Cons @1 @Int @0 (Prim._CO_ @((GHC.Prim.~#) Nat Nat 1 ((GHC.TypeNats.+) 0 1))) elU1_S (Nil @0 @Int (Prim._CO_ @((GHC.Prim.~#) Nat Nat 0 0))))) (Clash.Sized.Vector.unconcat @1 @2 @Int (1 :: Natural) (SNat @2 (2 :: Natural)) ((case restU0_S26 of
                { Cons (m_S :: Nat) (co_S :: (GHC.Prim.~#) Nat Nat 3 ((GHC.TypeNats.+) 2 1)) (el_S :: Int) (rest_S :: Clash.Sized.Vector.Vec 2 Int) -> rest_S
                }) :: Clash.Sized.Vector.Vec 2 Int))
            |])
      , testCase "unconcat into empty vectors" $
          reduces (ReduceUnconcat 2 0) [varD "v0_20" "Clash.Sized.Vector.Vec 0 Int"]
            (tm [strQQ|
              Clash.Sized.Vector.unconcat @2 @0 @Int (2 :: Natural) (SNat @0 (0 :: Natural)) v0_20
            |])
            (tm [strQQ|
              Cons @2 @(Clash.Sized.Vector.Vec 0 Int) @1 (Prim._CO_ @((GHC.Prim.~#) Nat Nat 2 ((GHC.TypeNats.+) 1 1))) (Nil @0 @Int (Prim._CO_ @((GHC.Prim.~#) Nat Nat 0 0))) (Cons @1 @(Clash.Sized.Vector.Vec 0 Int) @0 (Prim._CO_ @((GHC.Prim.~#) Nat Nat 1 ((GHC.TypeNats.+) 0 1))) (Nil @0 @Int (Prim._CO_ @((GHC.Prim.~#) Nat Nat 0 0))) (Nil @0 @(Clash.Sized.Vector.Vec 0 Int) (Prim._CO_ @((GHC.Prim.~#) Nat Nat 0 0))))
            |])
      , testCase "unconcat into an empty vector" $
          reduces (ReduceUnconcat 0 2) [varD "v0_20" "Clash.Sized.Vector.Vec 0 Int"]
            (tm [strQQ|
              Clash.Sized.Vector.unconcat @0 @2 @Int (0 :: Natural) (SNat @2 (2 :: Natural)) v0_20
            |])
            (tm [strQQ|
              Nil @0 @(Clash.Sized.Vector.Vec 2 Int) (Prim._CO_ @((GHC.Prim.~#) Nat Nat 0 0))
            |])
      , testCase "transpose" $
          reduces (ReduceTranspose 2) [varD "xss_11" "Clash.Sized.Vector.Vec 0 (Clash.Sized.Vector.Vec 2 Int)"]
            (tm "Clash.Sized.Vector.transpose @2 @0 @Int (2 :: Natural) xss_11")
            (tm [strQQ|
              Cons @2 @(Clash.Sized.Vector.Vec 0 Int) @1 (Prim._CO_ @((GHC.Prim.~#) Nat Nat 2 ((GHC.TypeNats.+) 1 1))) (Nil @0 @Int (Prim._CO_ @((GHC.Prim.~#) Nat Nat 0 0))) (Cons @1 @(Clash.Sized.Vector.Vec 0 Int) @0 (Prim._CO_ @((GHC.Prim.~#) Nat Nat 1 ((GHC.TypeNats.+) 0 1))) (Nil @0 @Int (Prim._CO_ @((GHC.Prim.~#) Nat Nat 0 0))) (Nil @0 @(Clash.Sized.Vector.Vec 0 Int) (Prim._CO_ @((GHC.Prim.~#) Nat Nat 0 0))))
            |])
      , testCase "replicate" $
          reduces (ReduceReplicate 3) [varD "k_30" "Int"]
            (tm [strQQ|
              (Clash.Sized.Vector.replicate :: forall (n :: Nat) (a :: Type). Clash.Promoted.Nat.SNat n -> a -> Clash.Sized.Vector.Vec n a) @3 @Int (SNat @3 (3 :: Natural)) k_30
            |])
            (tm [strQQ|
              Cons @3 @Int @2 (Prim._CO_ @((GHC.Prim.~#) Nat Nat 3 ((GHC.TypeNats.+) 2 1))) k_30 (Cons @2 @Int @1 (Prim._CO_ @((GHC.Prim.~#) Nat Nat 2 ((GHC.TypeNats.+) 1 1))) k_30 (Cons @1 @Int @0 (Prim._CO_ @((GHC.Prim.~#) Nat Nat 1 ((GHC.TypeNats.+) 0 1))) k_30 (Nil @0 @Int (Prim._CO_ @((GHC.Prim.~#) Nat Nat 0 0)))))
            |])
      , testCase "replace_int" $
          reduces (ReduceReplaceInt 2) [varD "v2_22" "Clash.Sized.Vector.Vec 2 Int", varD "k_30" "Int", varD "i_32" "GHC.Types.Int"]
            (tm [strQQ|
              (Clash.Sized.Vector.replace_int :: forall (n :: Nat) (a :: Type). Nat -> Clash.Sized.Vector.Vec n a -> GHC.Types.Int -> a -> Clash.Sized.Vector.Vec n a) @2 @Int (2 :: Natural) v2_22 i_32 k_30
            |])
            (tm [strQQ|
              let
                { (elI0_S :: Int) = (case v2_22 of
                    { Cons (m_S :: Nat) (co_S :: (GHC.Prim.~#) Nat Nat 2 ((GHC.TypeNats.+) 1 1)) (el_S :: Int) (rest_S :: Clash.Sized.Vector.Vec 1 Int) -> el_S
                    }) :: Int
                ; (restI0_S34 :: Clash.Sized.Vector.Vec 1 Int) = (case v2_22 of
                    { Cons (m_S :: Nat) (co_S :: (GHC.Prim.~#) Nat Nat 2 ((GHC.TypeNats.+) 1 1)) (el_S :: Int) (rest_S :: Clash.Sized.Vector.Vec 1 Int) -> rest_S
                    }) :: Clash.Sized.Vector.Vec 1 Int
                ; (elI1_S :: Int) = (case restI0_S34 of
                    { Cons (m_S :: Nat) (co_S :: (GHC.Prim.~#) Nat Nat 1 ((GHC.TypeNats.+) 0 1)) (el_S :: Int) (rest_S :: Clash.Sized.Vector.Vec 0 Int) -> el_S
                    }) :: Int
                } in Cons @2 @Int @1 (Prim._CO_ @((GHC.Prim.~#) Nat Nat 2 ((GHC.TypeNats.+) 1 1))) ((case (GHC.Classes.eqInt :: GHC.Types.Int -> GHC.Types.Int -> GHC.Types.Bool) i_32 (GHC.Types.I# 0) of
                { _ -> elI0_S
                ; GHC.Types.True -> k_30
                }) :: Int) (Cons @1 @Int @0 (Prim._CO_ @((GHC.Prim.~#) Nat Nat 1 ((GHC.TypeNats.+) 0 1))) ((case (GHC.Classes.eqInt :: GHC.Types.Int -> GHC.Types.Int -> GHC.Types.Bool) i_32 (GHC.Types.I# 1) of
                { _ -> elI1_S
                ; GHC.Types.True -> k_30
                }) :: Int) (Nil @0 @Int (Prim._CO_ @((GHC.Prim.~#) Nat Nat 0 0))))
            |])
      , testCase "index_int" $
          reduces (ReduceIndexInt 2) [varD "v2_22" "Clash.Sized.Vector.Vec 2 Int", varD "i_32" "GHC.Types.Int"]
            (tm "Clash.Sized.Vector.index_int @2 @Int (2 :: Natural) v2_22 i_32")
            (tm [strQQ|
              let
                { (elI0_S :: Int) = (case v2_22 of
                    { Cons (m_S :: Nat) (co_S :: (GHC.Prim.~#) Nat Nat 2 ((GHC.TypeNats.+) 1 1)) (el_S :: Int) (rest_S :: Clash.Sized.Vector.Vec 1 Int) -> el_S
                    }) :: Int
                ; (restI0_S34 :: Clash.Sized.Vector.Vec 1 Int) = (case v2_22 of
                    { Cons (m_S :: Nat) (co_S :: (GHC.Prim.~#) Nat Nat 2 ((GHC.TypeNats.+) 1 1)) (el_S :: Int) (rest_S :: Clash.Sized.Vector.Vec 1 Int) -> rest_S
                    }) :: Clash.Sized.Vector.Vec 1 Int
                ; (elI1_S :: Int) = (case restI0_S34 of
                    { Cons (m_S :: Nat) (co_S :: (GHC.Prim.~#) Nat Nat 1 ((GHC.TypeNats.+) 0 1)) (el_S :: Int) (rest_S :: Clash.Sized.Vector.Vec 0 Int) -> el_S
                    }) :: Int
                } in (case (GHC.Classes.eqInt :: GHC.Types.Int -> GHC.Types.Int -> GHC.Types.Bool) i_32 (GHC.Types.I# 0) of
                { _ -> (case (GHC.Classes.eqInt :: GHC.Types.Int -> GHC.Types.Int -> GHC.Types.Bool) i_32 (GHC.Types.I# 1) of
                    { _ -> Clash.Normalize.Primitives.undefined @Int
                    ; GHC.Types.True -> elI1_S
                    }) :: Int
                ; GHC.Types.True -> elI0_S
                }) :: Int
            |])
      , testCase "index_int on an empty vector" $
          reduces (ReduceIndexInt 0) [varD "v0_20" "Clash.Sized.Vector.Vec 0 Int", varD "i_32" "GHC.Types.Int"]
            (tm "Clash.Sized.Vector.index_int @0 @Int (0 :: Natural) v0_20 i_32")
            (tm "Clash.Normalize.Primitives.undefined @Int")
      , testCase "imap" $
          reduces (ReduceImap 2) [varD "im_8" "Clash.Sized.Internal.Index.Index 2 -> Int -> Int", varD "v2_22" "Clash.Sized.Vector.Vec 2 Int"]
            (tm "Clash.Sized.Vector.imap @2 @Int @Int (2 :: Natural) im_8 v2_22")
            (tm [strQQ|
              let
                { (elI0_S :: Int) = (case v2_22 of
                    { Cons (m_S :: Nat) (co_S :: (GHC.Prim.~#) Nat Nat 2 ((GHC.TypeNats.+) 1 1)) (el_S :: Int) (rest_S :: Clash.Sized.Vector.Vec 1 Int) -> el_S
                    }) :: Int
                ; (restI0_S24 :: Clash.Sized.Vector.Vec 1 Int) = (case v2_22 of
                    { Cons (m_S :: Nat) (co_S :: (GHC.Prim.~#) Nat Nat 2 ((GHC.TypeNats.+) 1 1)) (el_S :: Int) (rest_S :: Clash.Sized.Vector.Vec 1 Int) -> rest_S
                    }) :: Clash.Sized.Vector.Vec 1 Int
                ; (elI1_S :: Int) = (case restI0_S24 of
                    { Cons (m_S :: Nat) (co_S :: (GHC.Prim.~#) Nat Nat 1 ((GHC.TypeNats.+) 0 1)) (el_S :: Int) (rest_S :: Clash.Sized.Vector.Vec 0 Int) -> el_S
                    }) :: Int
                } in Cons @2 @Int @1 (Prim._CO_ @((GHC.Prim.~#) Nat Nat 2 ((GHC.TypeNats.+) 1 1))) ((\(carg_I :: Clash.Sized.Internal.Index.Index 2) (carg_I1000024 :: Int) -> im_8 carg_I carg_I1000024) ((Clash.Sized.Internal.Index.fromInteger# :: forall (n_S :: Nat). GHC.Num.Integer.Integer -> GHC.Num.Integer.Integer -> Clash.Sized.Internal.Index.Index n_S) @2 (2 :: Integer) (0 :: Integer)) elI0_S) (Cons @1 @Int @0 (Prim._CO_ @((GHC.Prim.~#) Nat Nat 1 ((GHC.TypeNats.+) 0 1))) ((\(carg_I :: Clash.Sized.Internal.Index.Index 2) (carg_I1000024 :: Int) -> im_8 carg_I carg_I1000024) ((Clash.Sized.Internal.Index.fromInteger# :: forall (n_S :: Nat). GHC.Num.Integer.Integer -> GHC.Num.Integer.Integer -> Clash.Sized.Internal.Index.Index n_S) @2 (2 :: Integer) (1 :: Integer)) elI1_S) (Nil @0 @Int (Prim._CO_ @((GHC.Prim.~#) Nat Nat 0 0))))
            |])
      , testCase "iterateI" $
          reduces (ReduceIterateI 3) [varD "f_1" "Int -> Int", varD "k_30" "Int"]
            (tm [strQQ|
              (Clash.Sized.Vector.iterateI :: forall (n :: Nat) (a :: Type). Nat -> (a -> a) -> a -> Clash.Sized.Vector.Vec n a) @3 @Int (3 :: Natural) f_1 k_30
            |])
            (tm [strQQ|
              let
                { (cel1_I :: Int) = (\(carg_I :: Int) -> f_1 carg_I) k_30
                ; (cel2_I :: Int) = (\(carg_I :: Int) -> f_1 carg_I) cel1_I
                } in Cons @3 @Int @2 (Prim._CO_ @((GHC.Prim.~#) Nat Nat 3 ((GHC.TypeNats.+) 2 1))) k_30 (Cons @2 @Int @1 (Prim._CO_ @((GHC.Prim.~#) Nat Nat 2 ((GHC.TypeNats.+) 1 1))) cel1_I (Cons @1 @Int @0 (Prim._CO_ @((GHC.Prim.~#) Nat Nat 1 ((GHC.TypeNats.+) 0 1))) cel2_I (Nil @0 @Int (Prim._CO_ @((GHC.Prim.~#) Nat Nat 0 0)))))
            |])
      , testCase "iterateI of an empty vector" $
          reduces (ReduceIterateI 0) [varD "f_1" "Int -> Int", varD "k_30" "Int"]
            (tm [strQQ|
              (Clash.Sized.Vector.iterateI :: forall (n :: Nat) (a :: Type). Nat -> (a -> a) -> a -> Clash.Sized.Vector.Vec n a) @0 @Int (0 :: Natural) f_1 k_30
            |])
            (tm [strQQ|
              let {  } in Nil @0 @Int (Prim._CO_ @((GHC.Prim.~#) Nat Nat 0 0))
            |])
      , testCase "dtfold" $
          reduces (ReduceDTFold 2) [varD "f_1" "Int -> Int", varD "br_7" "forall (l :: Nat). Clash.Promoted.Nat.SNat l -> Int -> Int -> Int", varD "p_9" "Int", varD "v4_24" "Clash.Sized.Vector.Vec 4 Int"]
            (tm "Clash.Sized.Vector.dtfold @Int @2 @Int (2 :: Natural) p_9 f_1 br_7 v4_24")
            (tm [strQQ|
              let
                { (elT0_S :: Int) = (case v4_24 of
                    { Cons (m_S :: Nat) (co_S :: (GHC.Prim.~#) Nat Nat 4 ((GHC.TypeNats.+) 3 1)) (el_S :: Int) (rest_S :: Clash.Sized.Vector.Vec 3 Int) -> el_S
                    }) :: Int
                ; (restT0_S26 :: Clash.Sized.Vector.Vec 3 Int) = (case v4_24 of
                    { Cons (m_S :: Nat) (co_S :: (GHC.Prim.~#) Nat Nat 4 ((GHC.TypeNats.+) 3 1)) (el_S :: Int) (rest_S :: Clash.Sized.Vector.Vec 3 Int) -> rest_S
                    }) :: Clash.Sized.Vector.Vec 3 Int
                ; (elT1_S :: Int) = (case restT0_S26 of
                    { Cons (m_S :: Nat) (co_S :: (GHC.Prim.~#) Nat Nat 3 ((GHC.TypeNats.+) 2 1)) (el_S :: Int) (rest_S :: Clash.Sized.Vector.Vec 2 Int) -> el_S
                    }) :: Int
                ; (restT1_S28 :: Clash.Sized.Vector.Vec 2 Int) = (case restT0_S26 of
                    { Cons (m_S :: Nat) (co_S :: (GHC.Prim.~#) Nat Nat 3 ((GHC.TypeNats.+) 2 1)) (el_S :: Int) (rest_S :: Clash.Sized.Vector.Vec 2 Int) -> rest_S
                    }) :: Clash.Sized.Vector.Vec 2 Int
                ; (elT2_S :: Int) = (case restT1_S28 of
                    { Cons (m_S :: Nat) (co_S :: (GHC.Prim.~#) Nat Nat 2 ((GHC.TypeNats.+) 1 1)) (el_S :: Int) (rest_S :: Clash.Sized.Vector.Vec 1 Int) -> el_S
                    }) :: Int
                ; (restT2_S30 :: Clash.Sized.Vector.Vec 1 Int) = (case restT1_S28 of
                    { Cons (m_S :: Nat) (co_S :: (GHC.Prim.~#) Nat Nat 2 ((GHC.TypeNats.+) 1 1)) (el_S :: Int) (rest_S :: Clash.Sized.Vector.Vec 1 Int) -> rest_S
                    }) :: Clash.Sized.Vector.Vec 1 Int
                ; (elT3_S :: Int) = (case restT2_S30 of
                    { Cons (m_S :: Nat) (co_S :: (GHC.Prim.~#) Nat Nat 1 ((GHC.TypeNats.+) 0 1)) (el_S :: Int) (rest_S :: Clash.Sized.Vector.Vec 0 Int) -> el_S
                    }) :: Int
                } in br_7 @1 (SNat @1 (1 :: Natural)) (br_7 @0 (SNat @0 (0 :: Natural)) (f_1 elT0_S) (f_1 elT1_S)) (br_7 @0 (SNat @0 (0 :: Natural)) (f_1 elT2_S) (f_1 elT3_S))
            |])
      , testCase "reverse" $
          reduces (ReduceReverse 3) [varD "v3_23" "Clash.Sized.Vector.Vec 3 Int"]
            (tm "Clash.Sized.Vector.reverse @3 @Int v3_23")
            (tm [strQQ|
              let
                { (elV0_S :: Int) = (case v3_23 of
                    { Cons (m_S :: Nat) (co_S :: (GHC.Prim.~#) Nat Nat 3 ((GHC.TypeNats.+) 2 1)) (el_S :: Int) (rest_S :: Clash.Sized.Vector.Vec 2 Int) -> el_S
                    }) :: Int
                ; (restV0_S25 :: Clash.Sized.Vector.Vec 2 Int) = (case v3_23 of
                    { Cons (m_S :: Nat) (co_S :: (GHC.Prim.~#) Nat Nat 3 ((GHC.TypeNats.+) 2 1)) (el_S :: Int) (rest_S :: Clash.Sized.Vector.Vec 2 Int) -> rest_S
                    }) :: Clash.Sized.Vector.Vec 2 Int
                ; (elV1_S :: Int) = (case restV0_S25 of
                    { Cons (m_S :: Nat) (co_S :: (GHC.Prim.~#) Nat Nat 2 ((GHC.TypeNats.+) 1 1)) (el_S :: Int) (rest_S :: Clash.Sized.Vector.Vec 1 Int) -> el_S
                    }) :: Int
                ; (restV1_S27 :: Clash.Sized.Vector.Vec 1 Int) = (case restV0_S25 of
                    { Cons (m_S :: Nat) (co_S :: (GHC.Prim.~#) Nat Nat 2 ((GHC.TypeNats.+) 1 1)) (el_S :: Int) (rest_S :: Clash.Sized.Vector.Vec 1 Int) -> rest_S
                    }) :: Clash.Sized.Vector.Vec 1 Int
                ; (elV2_S :: Int) = (case restV1_S27 of
                    { Cons (m_S :: Nat) (co_S :: (GHC.Prim.~#) Nat Nat 1 ((GHC.TypeNats.+) 0 1)) (el_S :: Int) (rest_S :: Clash.Sized.Vector.Vec 0 Int) -> el_S
                    }) :: Int
                } in Cons @3 @Int @2 (Prim._CO_ @((GHC.Prim.~#) Nat Nat 3 ((GHC.TypeNats.+) 2 1))) elV2_S (Cons @2 @Int @1 (Prim._CO_ @((GHC.Prim.~#) Nat Nat 2 ((GHC.TypeNats.+) 1 1))) elV1_S (Cons @1 @Int @0 (Prim._CO_ @((GHC.Prim.~#) Nat Nat 1 ((GHC.TypeNats.+) 0 1))) elV0_S (Nil @0 @Int (Prim._CO_ @((GHC.Prim.~#) Nat Nat 0 0)))))
            |])
      , testCase "tdfold" $
          reduces (ReduceTDFold 1) [varD "f_1" "Int -> Int", varD "br_7" "forall (l :: Nat). Clash.Promoted.Nat.SNat l -> Int -> Int -> Int", varD "p_9" "Int", varD "t_10" "Clash.Sized.RTree.RTree 1 Int"]
            (tm "Clash.Sized.RTree.tdfold @Int @1 @Int (1 :: Natural) p_9 f_1 br_7 t_10")
            (tm [strQQ|
              let
                { (ltT0_S :: Clash.Sized.RTree.RTree 0 Int) = (case t_10 of
                    { BR (m_S :: Nat) (co_S :: (GHC.Prim.~#) Nat Nat 1 ((GHC.TypeNats.+) 0 1)) (lt_S :: Clash.Sized.RTree.RTree 0 Int) (rt_S :: Clash.Sized.RTree.RTree 0 Int) -> lt_S
                    }) :: Clash.Sized.RTree.RTree 0 Int
                ; (rtT1_S :: Clash.Sized.RTree.RTree 0 Int) = (case t_10 of
                    { BR (m_S :: Nat) (co_S :: (GHC.Prim.~#) Nat Nat 1 ((GHC.TypeNats.+) 0 1)) (lt_S :: Clash.Sized.RTree.RTree 0 Int) (rt_S :: Clash.Sized.RTree.RTree 0 Int) -> rt_S
                    }) :: Clash.Sized.RTree.RTree 0 Int
                ; (elT0_S :: Int) = (case ltT0_S of
                    { LR (co_S :: (GHC.Prim.~#) Nat Nat 0 0) (el_S :: Int) -> el_S
                    }) :: Int
                ; (elT1_S :: Int) = (case rtT1_S of
                    { LR (co_S :: (GHC.Prim.~#) Nat Nat 0 0) (el_S :: Int) -> el_S
                    }) :: Int
                } in br_7 @0 (SNat @0 (0 :: Natural)) (f_1 elT0_S) (f_1 elT1_S)
            |])
      , testCase "treplicate" $
          reduces (ReduceTReplicate 2) [varD "k_30" "Int"]
            (tm [strQQ|
              (Clash.Sized.RTree.treplicate :: forall (n :: Nat) (a :: Type). Clash.Promoted.Nat.SNat n -> a -> Clash.Sized.RTree.RTree n a) @2 @Int (SNat @2 (2 :: Natural)) k_30
            |])
            (tm [strQQ|
              BR @2 @Int @1 (Prim._CO_ @((GHC.Prim.~#) Nat Nat 2 ((GHC.TypeNats.+) 1 1))) (BR @1 @Int @0 (Prim._CO_ @((GHC.Prim.~#) Nat Nat 1 ((GHC.TypeNats.+) 0 1))) (LR @0 @Int (Prim._CO_ @((GHC.Prim.~#) Nat Nat 0 0)) k_30) (LR @0 @Int (Prim._CO_ @((GHC.Prim.~#) Nat Nat 0 0)) k_30)) (BR @1 @Int @0 (Prim._CO_ @((GHC.Prim.~#) Nat Nat 1 ((GHC.TypeNats.+) 0 1))) (LR @0 @Int (Prim._CO_ @((GHC.Prim.~#) Nat Nat 0 0)) k_30) (LR @0 @Int (Prim._CO_ @((GHC.Prim.~#) Nat Nat 0 0)) k_30))
            |])
      ]
  , testCase "map on an empty vector gives an empty vector of the result element type" $
      reduces (ReduceMap 0 True) [varD "v0_20" "Clash.Sized.Vector.Vec 0 Int", varD "fb_40" "Int -> Bool_3010"]
        (tm "Clash.Sized.Vector.map @Int @Bool_3010 @0 fb_40 v0_20")
        (tm "Nil @0 @Bool_3010 (Prim._CO_ @((GHC.Prim.~#) Nat Nat 0 0))")
  ]
