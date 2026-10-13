{-|
  Copyright   :  (C) 2026, QBayLogic B.V.
  License     :  BSD2 (see the file LICENSE)
  Maintainer  :  QBayLogic B.V. <devops@qbaylogic.com>

  Tests for "Clash.Normalize.Transformations.SeparateArgs"
-}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Clash.Tests.Normalize.Transformations.SeparateArgs (tests) where

import Data.Default (def)

import Test.Tasty
import Test.Tasty.HUnit

import Clash.Core.DataCon (DataCon)
import Clash.Core.HasType (inferCoreTypeOf)
import Clash.Core.Name (Name (..), NameSort (User), mkUnsafeName)
import Clash.Core.Subst (aeqType)
import Clash.Core.Term (Term (..), collectArgs, mkApps)
import Clash.Core.Scoped.Convert (mkAlgTyConFromClash)
import Clash.Core.TyCon (AlgTyConRhs (DataTyCon), TyCon (..), TyConName)
import Clash.Core.TysPrim (liftedTypeKind)
import Clash.Core.Type
  (ConstTy (TyCon), Type (ConstTy, ForAllTy, VarTy), mkPolyFunTy)
import Clash.Core.Var (Id, Var (..))
import qualified Clash.Data.UniqMap as UniqMap
import Clash.Driver.Types (ClashEnv (..))
import Clash.Normalize.Transformations.SeparateArgs (separateArguments)
import Clash.Normalize.Types (NormRewrite)
import Clash.Rewrite.StrategyDSL.TH (asRewriteQ)
import Clash.Rewrite.Types (RewriteEnv (..))

import Test.Clash.Rewrite
  ( globalId, inScopeOf, intFunTy, intId, intTy, localId, mkTestDataCon
  , runSingleTransformation, showPprU, tyVar, withTyConMap )

separateArgumentsR :: NormRewrite
separateArgumentsR = $(asRewriteQ separateArguments)

-- | @Clock@, which Clash splits off from products
clockTcNm :: TyConName
clockTcNm = mkUnsafeName User "Clash.Signal.Internal.Clock" 2020

clockTy :: Type
clockTy = ConstTy (TyCon clockTcNm)

-- | @ClkPair@, a product type with a clock
clkPairTcNm :: TyConName
clkPairTcNm = mkUnsafeName User "ClkPair" 2021

clkPairTy :: Type
clkPairTy = ConstTy (TyCon clkPairTcNm)

-- | @MkClkPair :: Clock -> Int -> ClkPair@
clkPairDc :: DataCon
clkPairDc = mkTestDataCon "MkClkPair" 2022 [] [] [clockTy, intTy] clkPairTy

-- | The default 'RewriteEnv', extended with @Clock@ and @ClkPair@
env :: RewriteEnv
env = withTyConMap tcm def
 where
  cEnv = _clashEnv def
  tcm = foldr (\tc -> UniqMap.insert (tyConName tc) tc) (envTyConMap cEnv)
          [mkTc clockTcNm [], mkTc clkPairTcNm [clkPairDc]]
  mkTc nm dcs = mkAlgTyConFromClash
    (nameUniq nm)
    nm
    liftedTypeKind
    0
    (DataTyCon dcs)
    False

p, x, h :: Id
p = localId User "p" 30 clkPairTy
x = intId User "x" 31
h = localId User "h" 32 (intFunTy 1)

-- | Run 'separateArguments' on an application of a global function, and assert
-- that it gives the global function the expected type, and that the type of
-- the application doesn't change
assertGlobalType :: Type -> Term -> Assertion
assertGlobalType expected tm = do
  res <- runSingleTransformation env def (inScopeOf [p, x, h]) separateArgumentsR tm
  let tcm = envTyConMap (_clashEnv env)
  case collectArgs res of
    (Var g, _) ->
      assertBool
        ("Expected the type\n" <> showPprU expected <> "\nfor the global in:\n"
          <> showPprU res <> "\nbut got:\n" <> showPprU (varType g))
        (aeqType expected (varType g))
    _ -> assertFailure ("Expected an application of a global:\n" <> showPprU res)
  assertBool
    ("The type of\n" <> showPprU tm <> "\nchanged to that of\n" <> showPprU res)
    (aeqType (inferCoreTypeOf tcm tm) (inferCoreTypeOf tcm res))

tests :: TestTree
tests =
  testGroup
    "Clash.Tests.Normalize.Transformations.SeparateArgs"
    [ -- @g :: Int -> ClkPair -> Int@ becomes @g :: Int -> Clock -> Int -> Int@,
      -- not @g :: Int -> ClkPair -> ClkPair -> Int@
      testCase "gives the global the types of the fields" $ do
        let g = globalId User "g" 40 (mkPolyFunTy intTy [Right intTy, Right clkPairTy])
        assertGlobalType (mkPolyFunTy intTy [Right intTy, Right clockTy, Right intTy])
          (App (App (Var g) (Var x)) (Var p))
      -- @g :: ClkPair -> Int -> Int@ becomes @g :: Clock -> Int -> Int -> Int@,
      -- also when it isn't applied to all of its arguments
    , testCase "keeps the parameters that aren't applied" $ do
        let g = globalId User "g" 41 (mkPolyFunTy intTy [Right clkPairTy, Right intTy])
        assertGlobalType (mkPolyFunTy intTy [Right clockTy, Right intTy, Right intTy])
          (App (Var g) (Var p))
      -- @g \@(Int -> Int) p h x@ for @g :: forall a. ClkPair -> a -> a@ keeps
      -- the argument @x@, which is applied to the result of @g@
    , testCase "keeps the arguments applied to the result" $ do
        let a = tyVar User "a" 42
            g = globalId User "g" 43
                  (ForAllTy a (mkPolyFunTy (VarTy a) [Right clkPairTy, Right (VarTy a)]))
        assertGlobalType
          (ForAllTy a (mkPolyFunTy (VarTy a) [Right clockTy, Right intTy, Right (VarTy a)]))
          (mkApps (Var g) [Right (intFunTy 1), Left (Var p), Left (Var h), Left (Var x)])
    ]
