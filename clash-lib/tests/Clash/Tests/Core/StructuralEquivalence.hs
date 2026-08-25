{-|
  Copyright   :  (C) 2026, QBayLogic B.V.
  License     :  BSD2 (see the file LICENSE)
  Maintainer  :  QBayLogic B.V. <devops@qbaylogic.com>

  Tests for structural equality, comparison and hashing of 'Type' and 'Term'
-}

{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Clash.Tests.Core.StructuralEquivalence (tests) where

import Data.Text (Text)
import qualified Data.Text as Text

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.TH (testGroupGenerator)

import Clash.Core.Literal (Literal (..))
import Clash.Core.Name (NameSort (..), mkUnsafeName)
import Clash.Core.Subst (eqTerm, eqType, eqVar, hashTerm, hashType, ordType)
import Clash.Core.Term (Bind (..), Pat (..), Term (..), TickInfo (..))
import Clash.Core.Type (Kind, LitTy (..), Type (..))
import Clash.Core.TysPrim (liftedTypeKind)
import Clash.Core.Var (Id, TyVar, Var (..))
import Clash.Unique (Unique)

import Test.Clash.Rewrite (intTy, localId, parseTyConTy)

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

-- | An @Int@ binder with the given human readable name and unique.
intId :: Text -> Unique -> Id
intId nm uniq = localId User (Text.unpack nm) uniq intTy

-- | At least one term per 'Term' constructor that is cheap to build, plus
-- pairs differing only in a nested detail.
representativeTerms :: [Term]
representativeTerms =
  [ Var (intId "x" 1)
  , Var (intId "y" 2)
  , Var (intId "y" 1)
  , Literal (IntLiteral 1)
  , Literal (IntLiteral 2)
  , Lam (intId "x" 1) (Var (intId "x" 1))
  , Lam (intId "y" 1) (Var (intId "y" 1))
  , TyLam bLifted (Var (intId "x" 1))
  , App (Var (intId "x" 1)) (Var (intId "y" 2))
  , App (Var (intId "y" 2)) (Var (intId "x" 1))
  , TyApp (Var (intId "x" 1)) intTy
  , TyApp (Var (intId "x" 1)) boolTy
  , Let (NonRec (intId "x" 1) (Var (intId "y" 2))) (Var (intId "x" 1))
  , Let (Rec [(intId "x" 1, Var (intId "x" 1))]) (Var (intId "x" 1))
  , Case (Var (intId "x" 1)) intTy [(DefaultPat, Var (intId "y" 2))]
  , Case (Var (intId "x" 1)) intTy [(LitPat (IntLiteral 1), Var (intId "y" 2))]
  , Cast (Var (intId "x" 1)) intTy boolTy
  , Tick DeDup (Var (intId "x" 1))
  , Tick NoDeDup (Var (intId "x" 1))
  ]

-- | 'hashType' hashes structurally equal types alike. This is the direction of
-- the hash law that holds; the converse may fail on a collision.
case_hashTypeAgreesWithEqType :: Assertion
case_hashTypeAgreesWithEqType =
  sequence_
    [ assertEqual (show (t1, t2)) (hashType 0 t1) (hashType 0 t2)
    | t1 <- representativeTypes
    , t2 <- representativeTypes
    , eqType t1 t2
    ]

-- | 'hashTerm' hashes structurally equal terms alike. See
-- 'case_hashTypeAgreesWithEqType'.
case_hashTermAgreesWithEqTerm :: Assertion
case_hashTermAgreesWithEqTerm =
  sequence_
    [ assertEqual (show (t1, t2)) (hashTerm 0 t1) (hashTerm 0 t2)
    | t1 <- representativeTerms
    , t2 <- representativeTerms
    , eqTerm t1 t2
    ]

-- | Types that differ only in a human readable name hash differently. Like the
-- corresponding alpha-equivalence tests this is a sanity check on how chaotic
-- the hash is rather than a law: a collision here is allowed, just unlikely.
case_hashTypeNameSignificant :: Assertion
case_hashTypeNameSignificant =
  assertBool "hashes differ" $
    hashType 0 (VarTy kindVarK)
      /= hashType 0 (VarTy (kindedTyVar "k0" 0 liftedTypeKind))

-- | 'hashTerm' is /not/ modulo alpha: terms differing only in the name of a
-- binder hash differently, even though they are alpha-equivalent.
--
-- Clash splices the occurrence name of a Core binder into the HDL verbatim
-- (see 'Clash.Netlist.Id.unsafeFromCoreId'), so a design hash that could not
-- tell these apart would report a cache hit after a rename and leave HDL
-- carrying the old names in place.
case_hashTermBinderNameSignificant :: Assertion
case_hashTermBinderNameSignificant = do
  -- @Eq Term@ is alpha equivalence, and these two are alpha-equivalent
  idX @=? idY
  assertBool "not (eqTerm idX idY)" (not (eqTerm idX idY))
  assertBool "hashes differ" (hashTerm 0 idX /= hashTerm 0 idY)
 where
  idX = identity "x"
  idY = identity "y"
  identity nm = let b = intId nm 1 in Lam b (Var b)

tests :: TestTree
tests = $(testGroupGenerator)
