{-|
  Copyright   :  (C) 2026, QBayLogic B.V.
  License     :  BSD2 (see the file LICENSE)
  Maintainer  :  QBayLogic B.V. <devops@qbaylogic.com>

  Tests for "Clash.Normalize.Transformations.MultiPrim"
-}

{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}

module Clash.Tests.Normalize.Transformations.MultiPrim (tests) where

import qualified Control.Lens as Lens
import Data.Default (def)
import qualified Data.HashMap.Strict as HashMap

import Test.Tasty
import Test.Tasty.HUnit

import Clash.Annotations.Primitive (PrimitiveGuard (HasBlackBox))
import Clash.Core.FreeVars (typeFreeVars)
import Clash.Core.HasType (coreTypeOf, inferCoreTypeOf)
import Clash.Core.Name (NameSort (User), mkUnsafeName)
import Clash.Core.Pretty (showPpr)
import Clash.Core.Term
  ( Bind (..), IsMultiPrim (..), PrimInfo (..), PrimUnfolding (..), Term (..)
  , WorkInfo (..), collectArgs, collectBndrs )
import Clash.Core.TyCon (TyConMap, TyConName)
import Clash.Core.Type (Type (..), mkFunTy, mkTyConApp)
import qualified Clash.Data.UniqMap as UniqMap
import Clash.Driver.Types (ClashEnv (..))
import Clash.Netlist.BlackBox.Types (RenderVoid (..), TemplateKind (..))
import Clash.Netlist.Types (BlackBox (BBTemplate), Usage (Cont))
import Clash.Normalize.Transformations.MultiPrim (setupMultiResultPrim)
import Clash.Normalize.Types (NormRewrite)
import Clash.Primitives.Types (Primitive (..))
import Clash.Rewrite.StrategyDSL.TH (asRewriteQ)
import Clash.Rewrite.Types (RewriteEnv (..))

import Test.Clash.Rewrite
  ( inScopeOf, mkAlgTyCon, mkDataCon, runSingleTransformation
  , testTyConMap, tyVar, withTyConMap )

setupMultiResultPrimR :: NormRewrite
setupMultiResultPrimR = $(asRewriteQ setupMultiResultPrim)

-- | The name of the type constructor of pairs
tupNm :: TyConName
tupNm = mkUnsafeName User "GHC.Tuple.Prim.Tuple2" 123

-- | @mr :: forall a. a -> (a, a)@, a multi-result primitive
mrPrim :: PrimInfo
mrPrim = PrimInfo
  { primName = "mr"
  , primType = ForAllTy a (mkFunTy (VarTy a) (mkTyConApp tupNm [VarTy a, VarTy a]))
  , primWorkInfo = WorkVariable
  , primMultiResult = SingleResult
  , primUnfolding = NoUnfolding
  }
 where
  a = tyVar User "a" 30

-- | The default environment, with pairs, and 'mrPrim' as a multi-result
-- primitive
env :: RewriteEnv
env = withTyConMap tcm def {_clashEnv = env0 {envPrimitives = HashMap.singleton "mr" (HasBlackBox [] bb)}}
 where
  env0 = _clashEnv def
  bb = BlackBox
    { name = "mr"
    , workInfo = WorkVariable
    , renderVoid = NoRenderVoid
    , multiResult = True
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

-- | The type constructors of the tests, with pairs
tcm :: TyConMap
tcm = UniqMap.insert tupNm (mkAlgTyCon tupNm [b, c] [tupDc]) testTyConMap
 where
  b = tyVar User "b" 31
  c = tyVar User "c" 32
  tupDc =
    mkDataCon "GHC.Tuple.Prim.(,)" 200 [b, c] [] [VarTy b, VarTy c]
      (mkTyConApp tupNm [VarTy b, VarTy c])

-- | The primitives of a term
termPrims :: Term -> [PrimInfo]
termPrims = \case
  Prim p -> [p]
  Lam _ e -> termPrims e
  TyLam _ e -> termPrims e
  App e1 e2 -> termPrims e1 <> termPrims e2
  TyApp e _ -> termPrims e
  Let (NonRec _ x) e -> termPrims x <> termPrims e
  Let (Rec bs) e -> concatMap (termPrims . snd) bs <> termPrims e
  Case e _ alts -> termPrims e <> concatMap (termPrims . snd) alts
  Cast e _ _ -> termPrims e
  Tick _ e -> termPrims e
  _ -> []

tests :: TestTree
tests =
  testGroup
    "Clash.Tests.Normalize.Transformations.MultiPrim"
    [ testCase "setupMultiResultPrim creates primitives with closed types" $ do
        tm <- runSingleTransformation env def (inScopeOf []) setupMultiResultPrimR (Prim mrPrim)
        case [ primName p | p <- termPrims tm, not (Lens.nullOf typeFreeVars (primType p)) ] of
          [] -> pure ()
          ps -> assertFailure ("primitives with free type variables: " <> show ps <> "\n" <> showPpr tm)
    , testCase "setupMultiResultPrim selects results of their types" $ do
        tm <- runSingleTransformation env def (inScopeOf []) setupMultiResultPrimR (Prim mrPrim)
        case collectBndrs tm of
          (_, Let (Rec bs) _) -> do
            let sels = [ (x, rhs) | (x, rhs@(collectArgs -> (Prim p, _))) <- bs
                                  , primName p == "c$multiPrimSelect" ]
            length sels @?= 2
            mapM_ (\(x, rhs) -> inferCoreTypeOf tcm rhs @?= coreTypeOf x) sels
          _ -> assertFailure ("not a let-expression under lambdas: " <> showPpr tm)
    ]
