{-|
  Copyright  :  (C) 2026, QBayLogic B.V.
  License    :  BSD2 (see the file LICENSE)
  Maintainer :  QBayLogic B.V. <devops@qbaylogic.com>

  Tests for the cast push rules in 'Clash.Core.Util.squashArgs' and
  'Clash.Core.Evaluator.KPush.kpush'.
-}

{-# LANGUAGE OverloadedStrings #-}

module Clash.Tests.Core.Cast (tests) where

import Test.Tasty
import Test.Tasty.HUnit

import Clash.Core.DataCon (DataCon (..), DcStrictness (..))
import Clash.Core.Evaluator.KPush (kpush)
import Clash.Core.Literal (Literal (..))
import Clash.Core.Name (mkUnsafeSystemName)
import Clash.Core.Term (AppArg (..), Term (..), TickInfo (..))
import Clash.Core.TyCon (AlgTyConRhs (..), TyCon (..), TyConMap)
import Clash.Core.Type (Type (..), mkFunTy, mkTyConApp)
import Clash.Core.TysPrim (intPrimTy, liftedTypeKind, tysPrimMap, wordPrimTy)
import Clash.Core.Util (castEqType, squashArgs)
import Clash.Core.Var (TyVar, mkTyVar)
import Clash.Core.VarEnv (emptyInScopeSet)
import qualified Clash.Data.UniqMap as UniqMap
import GHC.Types.SrcLoc (noSrcSpan)

-- * Fixtures

aTv, bTv, cTv :: TyVar
aTv = mkTyVar liftedTypeKind (mkUnsafeSystemName "a" 100)
bTv = mkTyVar liftedTypeKind (mkUnsafeSystemName "b" 101)
cTv = mkTyVar liftedTypeKind (mkUnsafeSystemName "c" 102)

aTy, bTy, cTy :: Type
aTy = VarTy aTv
bTy = VarTy bTv
cTy = VarTy cTv

-- | data Pair a b = MkPair a b
pairTc :: TyCon
pairTc = AlgTyCon
  { tyConUniq = 200
  , tyConName = pairTcNm
  , tyConKind = mkFunTy liftedTypeKind (mkFunTy liftedTypeKind liftedTypeKind)
  , tyConArity = 2
  , algTcRhs = DataTyCon [pairDc]
  , isClassTc = False
  }
 where
  pairTcNm = mkUnsafeSystemName "Pair" 200

pairDc :: DataCon
pairDc = MkData
  { dcName = mkUnsafeSystemName "MkPair" 201
  , dcUniq = 201
  , dcTag = 1
  , dcType =
      ForAllTy aTv (ForAllTy bTv
        (mkFunTy aTy (mkFunTy bTy (pairOf aTy bTy))))
  , dcUnivTyVars = [aTv, bTv]
  , dcExtTyVars = []
  , dcArgTys = [aTy, bTy]
  , dcArgStrict = [Lazy, Lazy]
  , dcFieldLabels = []
  }

pairOf :: Type -> Type -> Type
pairOf t1 t2 = mkTyConApp (mkUnsafeSystemName "Pair" 200) [t1, t2]

tcm :: TyConMap
tcm = UniqMap.insertUnique pairTc tysPrimMap

x0, x1 :: Term
x0 = Literal (IntLiteral 0)
x1 = Literal (IntLiteral 1)

tick :: TickInfo
tick = SrcSpan noSrcSpan

squash :: [AppArg] -> Maybe ([Either Term Type], Maybe (Type, Type), [TickInfo])
squash = squashArgs tcm emptyInScopeSet

-- * Tests

tests :: TestTree
tests = testGroup "Clash.Tests.Core.Cast"
  [ testGroup "squashArgs"
    [ testCase "cast-free stack is unchanged" $
        squash [TermArg x0, TypeArg aTy, TickCtx tick]
          @?= Just ([Left x0, Right aTy], Nothing, [tick])

    , testCase "back-to-back casts merge" $
        squash [CastCtx aTy bTy, CastCtx bTy cTy]
          @?= Just ([], Just (aTy, cTy), [])

    , testCase "inverse casts cancel" $
        squash [CastCtx aTy bTy, CastCtx bTy aTy]
          @?= Just ([], Nothing, [])

    , testCase "refl cast disappears" $
        squash [CastCtx aTy aTy, TermArg x0]
          @?= Just ([Left x0], Nothing, [])

    , testCase "misaligned casts do not squash" $
        squash [CastCtx aTy bTy, CastCtx cTy aTy]
          @?= Nothing

    , testCase "push moves cast into term argument" $
        squash [CastCtx (mkFunTy aTy bTy) (mkFunTy cTy cTy), TermArg x0]
          @?= Just ([Left (Cast x0 cTy aTy)], Just (bTy, cTy), [])

    , testCase "push with refl argument coercion leaves argument alone" $
        squash [CastCtx (mkFunTy aTy bTy) (mkFunTy aTy cTy), TermArg x0]
          @?= Just ([Left x0], Just (bTy, cTy), [])

    , testCase "push fails on non-function types" $
        squash [CastCtx aTy bTy, TermArg x0]
          @?= Nothing

    , testCase "tpush moves cast past type argument" $
        squash [ CastCtx (ForAllTy aTv (mkFunTy aTy bTy))
                         (ForAllTy cTv (mkFunTy cTy cTy))
               , TypeArg intPrimTy ]
          @?= Just ( [Right intPrimTy]
                   , Just (mkFunTy intPrimTy bTy, mkFunTy intPrimTy intPrimTy)
                   , [])

    , testCase "tpush to refl disappears" $
        squash [ CastCtx (ForAllTy aTv aTy) (ForAllTy cTv cTy)
               , TypeArg intPrimTy ]
          @?= Just ([Right intPrimTy], Nothing, [])

    , testCase "ticks float outward past casts" $
        squash [ TickCtx tick, CastCtx (mkFunTy aTy bTy) (mkFunTy aTy cTy)
               , TickCtx tick, TermArg x0 ]
          @?= Just ([Left x0], Just (bTy, cTy), [tick, tick])
    ]

  , testGroup "castEqType"
    [ testCase "syntactic equality" $
        castEqType tcm aTy aTy @?= True
    , testCase "distinct type variables differ" $
        castEqType tcm aTy bTy @?= False
    , testCase "distinct prim types differ" $
        castEqType tcm intPrimTy wordPrimTy @?= False
    ]

  , testGroup "kpush"
    [ testCase "refl coercion leaves arguments alone" $
        kpush tcm pairDc pairArgs (pairOf intPrimTy wordPrimTy, pairOf intPrimTy wordPrimTy)
          @?= Just pairArgs

    , testCase "coercion is pushed into constructor arguments" $
        kpush tcm pairDc pairArgs (pairOf intPrimTy wordPrimTy, pairOf intPrimTy intPrimTy)
          @?= Just [ Right intPrimTy, Right intPrimTy
                   , Left x0, Left (Cast x1 wordPrimTy intPrimTy) ]

    , testCase "tycon mismatch is rejected" $
        kpush tcm pairDc pairArgs (pairOf intPrimTy wordPrimTy, intPrimTy)
          @?= Nothing
    ]
  ]
 where
  pairArgs = [Right intPrimTy, Right wordPrimTy, Left x0, Left x1]
