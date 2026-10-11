{-|
  Copyright   :  (C) 2026, QBayLogic B.V.
  License     :  BSD2 (see the file LICENSE)
  Maintainer  :  QBayLogic B.V. <devops@qbaylogic.com>

  Tests for "Clash.Normalize.Scoped.Transformations.MultiPrim": agreement of
  the port of 'Clash.Normalize.Transformations.MultiPrim.setupMultiResultPrim'
  with that of Clash Core.
-}

{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Clash.Tests.Normalize.Scoped.Transformations.MultiPrim (tests) where

import Data.Default (def)
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Text as Text

import Test.Tasty
import Test.Tasty.HUnit

import Clash.Annotations.Primitive (PrimitiveGuard (HasBlackBox))
import Clash.Core.Name (NameSort (User))
import Clash.Core.Term
  ( IsMultiPrim (..), PrimInfo (..), PrimUnfolding (NoUnfolding), Term (..)
  , WorkInfo (WorkNever) )
import Clash.Core.TyCon (TyCon (..))
import Clash.Core.Type (Type (ForAllTy, VarTy), mkFunTy)
import qualified Clash.Data.UniqMap as UniqMap
import Clash.Driver.Types (ClashEnv (..))
import Clash.Netlist.BlackBox.Types (RenderVoid (NoRenderVoid), TemplateKind (TDecl))
import Clash.Netlist.Types (BlackBox (BBTemplate), Usage (Cont))
import qualified Clash.Normalize.Scoped.Transformations.MultiPrim as Scoped
import Clash.Normalize.Transformations.MultiPrim (setupMultiResultPrimWorker)
import Clash.Normalize.Types (NormRewrite, NormalizeState)
import Clash.Primitives.Types (Primitive (..))
import Clash.Rewrite.StrategyDSL (onPrim, toTransformation)
import Clash.Rewrite.StrategyDSL.TH (asRewriteQ)
import Clash.Rewrite.Types
  (RewriteEnv (..), RewriteState (..), TransformContext (..))
import Clash.Util.Supply (newSupply)

import Test.Clash.Differential (Outcome (..), letrecOrder)
import Test.Clash.Rewrite
  ( assertFires, inScopeOf, intTy, mkAlgTyCon, mkDataCon, mkTyConName, testTyConMap
  , tupleTy, tyVar, withTyConMap )
import Test.Clash.Scoped (assertRewritesAgreeUpTo)

-- | 'setupMultiResultPrim' of Clash Core
setupCurrent :: NormRewrite
setupCurrent =
  $(asRewriteQ (toTransformation "setupMultiResultPrim" (onPrim 'setupMultiResultPrimWorker)))

-- | The port, as the normalizer runs it: through
-- 'Clash.Rewrite.Scoped.ViaFoil.viaFoil'
setupScoped :: NormRewrite
setupScoped = $(asRewriteQ Scoped.setupMultiResultPrim)

-- | The pair type constructor, as 'tupleTy' names it
pairTc :: TyCon
pairTc = mkAlgTyCon nm [t1, t2] [dc]
 where
  nm = mkTyConName "GHC.Tuple.(,)" 3036
  t1 = tyVar User "t1" 3040
  t2 = tyVar User "t2" 3041
  dc = mkDataCon "GHC.Tuple.(,)" 3042 [t1, t2] [] [VarTy t1, VarTy t2]
         (tupleTy [VarTy t1, VarTy t2])

-- | An environment with the pair type constructor, and the multi-result
-- primitives 'monoPrim' and 'polyPrim', and the single-result 'singlePrim'
env :: RewriteEnv
env = withTyConMap tcm def{_clashEnv = (_clashEnv def){envPrimitives = prims}}
 where
  tcm = UniqMap.insert (tyConName pairTc) pairTc testTyConMap
  prims = HashMap.fromList
    [ (primName monoPrim, HasBlackBox [] (bb (primName monoPrim) True))
    , (primName polyPrim, HasBlackBox [] (bb (primName polyPrim) True))
    , (primName singlePrim, HasBlackBox [] (bb (primName singlePrim) False))
    ]
  bb nm multi = BlackBox
    { name = nm
    , workInfo = WorkNever
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

prim :: Text.Text -> Type -> PrimInfo
prim nm ty = PrimInfo nm ty WorkNever SingleResult NoUnfolding

-- | @mono :: Int -> Int -> (Int, Int)@
monoPrim :: PrimInfo
monoPrim = prim "mono" (mkFunTy intTy (mkFunTy intTy (tupleTy [intTy, intTy])))

-- | @poly :: forall a. a -> (a, Int)@
polyPrim :: PrimInfo
polyPrim = prim "poly" (ForAllTy a (mkFunTy (VarTy a) (tupleTy [VarTy a, intTy])))
 where
  a = tyVar User "a" 3050

-- | @single :: Int -> (Int, Int)@, a primitive with a single result
singlePrim :: PrimInfo
singlePrim = prim "single" (mkFunTy intTy (tupleTy [intTy, intTy]))

-- | Assert that 'setupMultiResultPrim' and its port agree on a term, up to
-- the order of let-bindings. Gives the known differences.
agree :: Bool -> Term -> IO Outcome
agree fires tm = do
  supply <- newSupply
  let st = def{_uniqSupply = supply} :: RewriteState NormalizeState
      ctx = TransformContext (inScopeOf []) []
  assertFires fires env st ctx setupCurrent tm
  assertRewritesAgreeUpTo [letrecOrder] env st ctx setupCurrent setupScoped tm

tests :: TestTree
tests = testGroup "Clash.Tests.Normalize.Scoped.Transformations.MultiPrim"
  [ testCase "a monomorphic multi-result primitive" $
      agree True (Prim monoPrim) >>= \case
        Different _ -> assertFailure "different"
        KnownDifferent kds -> assertBool (show kds) ("prim-type-form" `notElem` kds)
        Equal -> pure ()
  , testCase "a polymorphic multi-result primitive" $
      () <$ agree True (Prim polyPrim)
  , testCase "doesn't fire on a primitive with a single result" $
      () <$ agree False (Prim singlePrim)
  , testCase "doesn't fire on a primitive set up already" $
      () <$ agree False (Prim monoPrim{primMultiResult = MultiResult})
  ]
