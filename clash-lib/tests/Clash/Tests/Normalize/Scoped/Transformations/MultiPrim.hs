{-|
  Copyright   :  (C) 2026, QBayLogic B.V.
  License     :  BSD2 (see the file LICENSE)
  Maintainer  :  QBayLogic B.V. <devops@qbaylogic.com>

  Tests for "Clash.Normalize.Scoped.Transformations.MultiPrim".
-}

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Clash.Tests.Normalize.Scoped.Transformations.MultiPrim (tests) where

import Data.Default (def)
import qualified Data.HashMap.Strict as HashMap

import Test.Tasty
import Test.Tasty.HUnit

import Clash.Annotations.Primitive (PrimitiveGuard (HasBlackBox))
import qualified Clash.Core.DataCon as C
import qualified Clash.Core.Basic as C (IsMultiPrim (..), WorkInfo (WorkNever))
import qualified Clash.Core.TyCon as C
import Clash.Core.Scoped.Syntax (mkPrimInfo)
import Clash.Core.Scoped.Syntax (PrimInfo (..), PrimUnfolding (NoUnfolding), S (VoidS), Type)
import qualified Clash.Data.UniqMap as UniqMap
import Clash.Driver.Types (ClashEnv (..))
import Clash.Netlist.BlackBox.Types (RenderVoid (NoRenderVoid), TemplateKind (TDecl))
import Clash.Netlist.Types (BlackBox (BBTemplate), Usage (Cont))
import qualified Clash.Normalize.Scoped.Transformations.MultiPrim as Scoped
import Clash.Normalize.Types (NormalizeState)
import Clash.Primitives.Types (Primitive (..))
import Clash.Rewrite.Scoped (Rewrite)
import Clash.Rewrite.StrategyDSL.TH (asScopedRewriteQ)
import Clash.Rewrite.Types (RewriteEnv (..), RewriteState)

import Test.Clash.Rewrite
  ( TestDefs (..), TestTerm, parseTestTermWith, strQQ, testAlgTyCon, testDataCon
  , testTyConMap, tyVarD, withTyConMap )
import Test.Clash.Scoped (assertNoRewrite, assertRewritesTo, closedType)

-- | The pair type constructor. 'Clash.Core.TyCon.isTupleTyConLike' recognizes
-- it by its name.
pairTc :: C.TyCon
pairTc = testAlgTyCon "GHC.Tuple.Tuple2_3036" ["t1", "t2"] [pairDc]

pairDc :: C.DataCon
pairDc = testDataCon "GHC.Tuple.Tuple2" 3042 1 [tyVarD "t1", tyVarD "t2"] [] ["t1", "t2"]
  "GHC.Tuple.Tuple2_3036 t1 t2"

-- | An environment with the pair type constructor, and the multi-result
-- primitives 'monoPrim' and 'polyPrim', and the single-result 'singlePrim'
env :: RewriteEnv
env = withTyConMap tcm def{_clashEnv = (_clashEnv def){envPrimitives = prims}}
 where
  tcm = UniqMap.insert (C.tyConName pairTc) pairTc testTyConMap
  prims = HashMap.fromList
    [ (primName monoPrim, HasBlackBox [] (bb (primName monoPrim) True))
    , (primName polyPrim, HasBlackBox [] (bb (primName polyPrim) True))
    , (primName singlePrim, HasBlackBox [] (bb (primName singlePrim) False))
    ]
  bb nm multi = BlackBox
    { name = nm
    , workInfo = C.WorkNever
    , renderVoid = NoRenderVoid
    , multiResult = multi
    , kind = TDecl
    , warning = ()
    , outputUsage = Cont
    , libraries = []
    , imports = []
    , functionPlurality = []
    , includes = []
    , resultNames = []
    , resultInits = []
    , template = BBTemplate []
    }

-- | @mono :: Int -> Int -> (Int, Int)@, with a single or multiple results
monoPrim, monoMultiPrim :: PrimInfo
monoPrim = mkPrimInfo "mono" monoTy C.WorkNever C.SingleResult NoUnfolding
monoMultiPrim = mkPrimInfo "mono" monoTy C.WorkNever C.MultiResult NoUnfolding

monoTy :: Type 'VoidS
monoTy = closedType "Int -> Int -> GHC.Tuple.Tuple2_3036 Int Int"

-- | @poly :: forall a. a -> (a, Int)@
polyPrim :: PrimInfo
polyPrim = mkPrimInfo "poly" (closedType "forall (a :: Type). a -> GHC.Tuple.Tuple2_3036 a Int")
  C.WorkNever C.SingleResult NoUnfolding

-- | @single :: Int -> (Int, Int)@, a primitive with a single result
singlePrim :: PrimInfo
singlePrim = mkPrimInfo "single" (closedType "Int -> GHC.Tuple.Tuple2_3036 Int Int")
  C.WorkNever C.SingleResult NoUnfolding

tm :: String -> TestTerm
tm = parseTestTermWith (TestDefs [pairDc] [monoPrim, polyPrim, singlePrim] [])

st :: RewriteState NormalizeState
st = def

setupS :: Rewrite NormalizeState
setupS = $(asScopedRewriteQ Scoped.setupMultiResultPrim)

tests :: TestTree
tests = testGroup "Clash.Tests.Normalize.Scoped.Transformations.MultiPrim"
  [ testCase "a monomorphic multi-result primitive" $
      assertRewritesTo env st setupS [] (tm "Prim.mono")
        (tm [strQQ|
          \(a1 :: Int) (a2 :: Int) ->
            let
              { (r :: GHC.Tuple.Tuple2_3036 Int Int) = Prim.mono a1 a2 r3 r4
              ; (r3 :: Int) = (Core.prim @"c$multiPrimSelect") @Int @(GHC.Tuple.Tuple2_3036 Int Int) r3 r
              ; (r4 :: Int) = (Core.prim @"c$multiPrimSelect") @Int @(GHC.Tuple.Tuple2_3036 Int Int) r4 r
              } in GHC.Tuple.Tuple2 @Int @Int r3 r4
        |])
  , testCase "a polymorphic multi-result primitive" $
      assertRewritesTo env st setupS [] (tm "Prim.poly")
        (tm [strQQ|
          \(a :: Type) (a1 :: a) ->
            let
              { (r :: GHC.Tuple.Tuple2_3036 a Int) = Prim.poly @a a1 r2 r3
              ; (r2 :: a) = (Core.prim @"c$multiPrimSelect") @a @(GHC.Tuple.Tuple2_3036 a Int) r2 r
              ; (r3 :: Int) = (Core.prim @"c$multiPrimSelect") @Int @(GHC.Tuple.Tuple2_3036 a Int) r3 r
              } in GHC.Tuple.Tuple2 @a @Int r2 r3
        |])
  , testCase "doesn't fire on a primitive with a single result" $
      assertNoRewrite env st setupS [] (tm "Prim.single")
  , testCase "doesn't fire on a primitive set up already" $
      assertNoRewrite env st setupS []
        (parseTestTermWith (TestDefs [pairDc] [monoMultiPrim] []) "Prim.mono")
  ]
