{-|
  Copyright  :  (C) 2026, QBayLogic B.V.
  License    :  BSD2 (see the file LICENSE)
  Maintainer :  QBayLogic B.V. <devops@qbaylogic.com>

  Tests for the builders of scoped Clash Core
-}

{-# LANGUAGE OverloadedStrings #-}

module Clash.Tests.Core.Scoped.Builder (tests) where

import Test.Tasty
import Test.Tasty.HUnit

import qualified Clash.Core.Name as C
import qualified Clash.Core.Type as C
import qualified Clash.Core.TysPrim as C
import qualified Clash.Core.Var as C

import Clash.Core.Scoped.Builder
import Clash.Core.Scoped.Convert (fromClashTypeClosed, toClashType)
import Clash.Core.Scoped.Syntax

import Test.Clash.Rewrite (kindedTyVar, mkTyConName, tyVar)

kTv, aTv :: C.TyVar
kTv = tyVar C.User "k" 60
aTv = kindedTyVar C.User "a" 61 (C.VarTy kTv)

maybeTc :: C.ConstTy
maybeTc = C.TyCon (mkTyConName "Maybe" 2001)

-- | @forall k (a :: k). a -> Maybe a@
clashTy :: C.Type
clashTy =
  C.ForAllTy kTv $ C.ForAllTy aTv $
    C.mkFunTy (C.VarTy aTv) (C.AppTy (C.ConstTy maybeTc) (C.VarTy aTv))

-- | The same type, built natively. The keys differ from the uniques of the
-- Clash type variables.
nativeTy :: TyB
nativeTy =
  forAllTyB 1 (C.varName kTv) (clashTyB C.liftedTypeKind) $
    forAllTyB 2 (C.varName aTv) (varTyB 1) $
      appTyB (appTyB (constTyB C.Arrow) (varTyB 2)) (appTyB (constTyB maybeTc) (varTyB 2))

-- | Compare raw names too: as types of Clash Core, whose uniques are the raw
-- names
sameType :: Type 'VoidS -> Type 'VoidS -> Assertion
sameType t1 t2 = show (toClashType t1) @?= show (toClashType t2)

tests :: TestTree
tests = testGroup "Clash.Tests.Core.Scoped.Builder"
  [ testCase "native type builders number binders like conversion" $
      sameType (runClosedType nativeTy) (fromClashTypeClosed emptyScope clashTy)
  , testCase "clashTyB numbers binders like conversion" $
      sameType (runClosedType (clashTyB clashTy)) (fromClashTypeClosed emptyScope clashTy)
  , testCase "closed types without binders are shared" $
      case appTyB (constTyB maybeTc) (litTyB (C.NumTy 3)) of
        b -> sameType (runClosedType b) (AppTy (ConstTy maybeTc) (LitTy (C.NumTy 3)))
  ]
