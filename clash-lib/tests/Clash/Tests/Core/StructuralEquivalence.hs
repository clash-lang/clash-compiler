{-|
  Copyright   :  (C) 2026, QBayLogic B.V.
  License     :  BSD2 (see the file LICENSE)
  Maintainer  :  QBayLogic B.V. <devops@qbaylogic.com>

  Tests for structural equality and comparison of 'Type'
-}

{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Clash.Tests.Core.StructuralEquivalence (tests) where

import Data.Text (Text)

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.TH (testGroupGenerator)

import Clash.Core.Name (NameSort (..), mkUnsafeName)
import Clash.Core.Subst (eqType, eqVar, ordType)
import Clash.Core.Type (Kind, LitTy (..), Type (..))
import Clash.Core.TysPrim (liftedTypeKind)
import Clash.Core.Var (TyVar, Var (..))
import Clash.Unique (Unique)

import Test.Clash.Rewrite (intTy, parseTyConTy)

-- | A 'TyVar' with the given human readable name, unique and kind. Unlike
-- 'Test.Clash.Rewrite.tyVar', whose kind is always 'liftedTypeKind'.
kindedTyVar :: Text -> Unique -> Kind -> TyVar
kindedTyVar nm uniq kind = TyVar (mkUnsafeName User nm uniq) uniq kind

-- | The kind variable @(~)@ and @Coercible@ bind, i.e. @mkAlphaTyVarUnique 0@
kindVarK :: TyVar
kindVarK = kindedTyVar "k" 0 liftedTypeKind

-- | @(,)@'s second type variable, @b :: Type@
bLifted :: TyVar
bLifted = kindedTyVar "b" 2 liftedTypeKind

-- | @(~)@'s second type variable, @b :: k@. GHC gives it the same unique as
-- 'bLifted': both are @mkAlphaTyVarUnique 2@.
bKinded :: TyVar
bKinded = kindedTyVar "b" 2 (VarTy kindVarK)

boolTy :: Type
boolTy = parseTyConTy "Bool"

-- | Assert that two types are structurally equal, and that 'ordType' agrees,
-- in both directions.
assertEqualTy :: Type -> Type -> Assertion
assertEqualTy t1 t2 = do
  assertBool "eqType t1 t2" (eqType t1 t2)
  assertBool "eqType t2 t1" (eqType t2 t1)
  EQ @=? ordType t1 t2
  EQ @=? ordType t2 t1

-- | Assert that two types are not structurally equal, and that 'ordType'
-- agrees. Also checks that comparison in the opposite direction yields the
-- opposite result, i.e. that the order is antisymmetric on this pair.
assertNotEqualTy :: Type -> Type -> Assertion
assertNotEqualTy t1 t2 = do
  assertBool "not (eqType t1 t2)" (not (eqType t1 t2))
  assertBool "not (eqType t2 t1)" (not (eqType t2 t1))
  case ordType t1 t2 of
    EQ -> assertFailure "ordType t1 t2 == EQ"
    LT -> GT @=? ordType t2 t1
    GT -> LT @=? ordType t2 t1

-- | Regression test for #3361: type variables that share a unique but not a
-- kind are not structurally equal.
case_tyVarKindSignificant :: Assertion
case_tyVarKindSignificant = do
  assertNotEqualTy (VarTy bLifted) (VarTy bKinded)
  assertBool "not (eqVar bLifted bKinded)" (not (eqVar bLifted bKinded))
  -- The 'Eq' instance on 'Var' is exactly what is too coarse here: it only
  -- compares uniques (and scope), which is why 'eqVar' exists.
  assertBool "bLifted == bKinded" (bLifted == bKinded)

-- | The kind of a 'ForAllTy' binder is significant. See
-- 'case_tyVarKindSignificant'.
case_forAllTyBinderKindSignificant :: Assertion
case_forAllTyBinderKindSignificant =
  assertNotEqualTy (ForAllTy bLifted intTy) (ForAllTy bKinded intTy)

-- | A kind difference nested inside a type is found too. See
-- 'case_tyVarKindSignificant'.
case_nestedTyVarKindSignificant :: Assertion
case_nestedTyVarKindSignificant =
  assertNotEqualTy (AppTy intTy (VarTy bLifted)) (AppTy intTy (VarTy bKinded))

-- | Type variables agreeing on unique /and/ kind are structurally equal.
case_tyVarSameKindEqual :: Assertion
case_tyVarSameKindEqual =
  assertEqualTy (VarTy bLifted) (VarTy (kindedTyVar "b" 2 liftedTypeKind))

-- | Type variables that differ only in their human readable name are not
-- structurally equal either.
--
-- GHC really does produce such pairs. Its template variables are numbered from
-- zero per wired-in construct, so the alpha uniques are reused wholesale:
-- @(~)@'s @k@ and @(~~)@'s @k0@ are both @mkAlphaTyVarUnique 0@ kinded 'Type',
-- and differ in nothing but their name.
case_tyVarNameSignificant :: Assertion
case_tyVarNameSignificant =
  assertNotEqualTy (VarTy kindVarK) (VarTy (kindedTyVar "k0" 0 liftedTypeKind))

-- | Structural equality is finer than alpha equivalence: alpha-equivalent
-- types whose binders have different uniques are not structurally equal.
case_structuralIsFinerThanAlpha :: Assertion
case_structuralIsFinerThanAlpha = do
  -- @Eq Type@ is alpha equivalence
  t1 @=? t2
  assertNotEqualTy t1 t2
 where
  t1 = ForAllTy a (VarTy a)
  t2 = ForAllTy b (VarTy b)
  a = kindedTyVar "a" 1 liftedTypeKind
  b = kindedTyVar "b" 2 liftedTypeKind

-- | At least one type per 'Type' constructor, plus the pairs that only differ
-- in a nested detail, so that the 'ordType' laws below are exercised on
-- types that compare equal as well as on types that don't.
representativeTypes :: [Type]
representativeTypes =
  [ VarTy bLifted
  , VarTy bKinded
  , VarTy (kindedTyVar "b'" 2 liftedTypeKind)
  , VarTy (kindedTyVar "c" 3 liftedTypeKind)
  , intTy
  , boolTy
  , LitTy (NumTy 5)
  , LitTy (NumTy 6)
  , LitTy (SymTy "sym")
  , LitTy (CharTy 'c')
  , AppTy intTy boolTy
  , AppTy boolTy intTy
  , ForAllTy bLifted intTy
  , ForAllTy bKinded intTy
  , ForAllTy bLifted boolTy
  , AnnType [] intTy
  ]

-- | 'ordType' yields 'EQ' exactly when 'eqType' holds.
case_ordTypeAgreesWithEqType :: Assertion
case_ordTypeAgreesWithEqType =
  sequence_
    [ assertEqual (show (t1, t2)) (eqType t1 t2) (ordType t1 t2 == EQ)
    | t1 <- representativeTypes
    , t2 <- representativeTypes
    ]

-- | Swapping 'ordType''s arguments flips the 'Ordering'.
case_ordTypeAntisymmetric :: Assertion
case_ordTypeAntisymmetric =
  sequence_
    [ assertEqual (show (t1, t2)) (flipOrdering (ordType t1 t2)) (ordType t2 t1)
    | t1 <- representativeTypes
    , t2 <- representativeTypes
    ]
 where
  flipOrdering = \case
    LT -> GT
    EQ -> EQ
    GT -> LT

-- | 'ordType' is reflexive.
case_ordTypeReflexive :: Assertion
case_ordTypeReflexive =
  sequence_
    [ assertEqual (show t) EQ (ordType t t) | t <- representativeTypes ]

-- | 'ordType' is transitive.
case_ordTypeTransitive :: Assertion
case_ordTypeTransitive =
  sequence_
    [ assertBool (show (t1, t2, t3)) (ordType t1 t3 /= GT)
    | t1 <- representativeTypes
    , t2 <- representativeTypes
    , t3 <- representativeTypes
    , ordType t1 t2 /= GT
    , ordType t2 t3 /= GT
    ]

tests :: TestTree
tests = $(testGroupGenerator)
