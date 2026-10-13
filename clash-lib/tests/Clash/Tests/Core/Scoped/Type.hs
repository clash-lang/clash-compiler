{-|
  Copyright  :  (C) 2026, QBayLogic B.V.
  License    :  BSD2 (see the file LICENSE)
  Maintainer :  QBayLogic B.V. <devops@qbaylogic.com>

  Tests for the views on, and utilities for, the types of scoped Clash Core.

  Most tests check that a function agrees with its counterpart in
  "Clash.Core.Type": converting a Clash type, applying the scoped function, and
  converting back gives the same result, up to alpha-equivalence, as applying
  the Clash function. They do so on 'corpus', in the scope of the free type
  variables of each type, and sunk into a larger scope where all binders of
  the type clash with the scope (see 'withTypeScopes').
-}

{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module Clash.Tests.Core.Scoped.Type (tests, corpus) where

import Control.Monad (forM_)
import Control.Monad.Trans.Except (runExcept)
import Data.Either (isLeft)
import Data.Maybe (isJust)

import Test.Tasty
import Test.Tasty.HUnit

import qualified Clash.Core.Name as C
import qualified Clash.Core.TyCon as C
import qualified Clash.Core.Type as C
import qualified Clash.Core.TysPrim as C
import qualified Clash.Core.Util as C
import qualified Clash.Data.UniqMap as UniqMap

import Clash.Core.Scoped.Convert
import Clash.Core.Scoped.FreeVars (freeTyVarsOfType)
import Clash.Core.Scoped.NameMap (nullNameSet)
import Clash.Core.Scoped.Subst (unsinkType)
import Clash.Core.Scoped.Syntax
import Clash.Core.Scoped.Type
import Clash.Core.Scoped.TypeEnv (TypeEnv, mkTypeEnv)

import Test.Clash.Rewrite
  ( addTy, boolTy, clkPairTy, clockTy, cmpNatTy, enableTy, fTy, intTy, kTy
  , maxTy, maybeTy, mkTestFunTyCon, mkTyConName, nTy, pairTy, pTy, qTy, resetTy
  , signalTy, testTyConMap, tyVar, unconsSymbolTy, vecTy, wTy )
import Test.Clash.Scoped

tcm :: C.TyConMap
tcm = testTyConMap

tenv :: TypeEnv
tenv = testTypeEnv

xTv, cTv, aTv, bTv :: C.TyVar
xTv = tyVar C.User "x" 3100
-- Has the same unique as the name conversion gives the type variable that
-- the representation type of @P@, and the right-hand side of @Q@, bind
cTv = tyVar C.User "c" 2
aTv = tyVar C.User "a" 3102
bTv = tyVar C.User "b" 3103

x, c :: C.Type
x = C.VarTy xTv
c = C.VarTy cTv

lit :: Integer -> C.Type
lit = C.LitTy . C.NumTy

sym :: String -> C.Type
sym = C.LitTy . C.SymTy

dom :: C.Type
dom = sym "System"

-- | Types with newtypes, type families, function types, binders and
-- annotations, closed and with free type variables
corpus :: [C.Type]
corpus =
  [ intTy, boolTy, maybeTy intTy, maybeTy x
  , nTy intTy, nTy x, nTy (nTy x), pTy intTy, pTy x, pTy c, wTy
  , C.AppTy (nTy intTy) boolTy
  , signalTy dom (nTy intTy), signalTy dom (C.mkFunTy intTy x)
  , fTy intTy, fTy (maybeTy boolTy), fTy x, fTy (maybeTy x), fTy (fTy intTy)
  , fTy (nTy intTy), fTy (C.AnnType [] intTy)
  , maxTy intTy intTy, maxTy intTy boolTy, maxTy x x, maxTy (fTy intTy) boolTy
  , C.AppTy kTy intTy
  , qTy intTy, qTy x, qTy c
  , addTy (lit 3) (lit 4), addTy (addTy (lit 1) (lit 2)) x, addTy x (lit 1)
  , cmpNatTy (lit 1) (lit 2), cmpNatTy (lit 2) (addTy (lit 1) (lit 1))
  , unconsSymbolTy (sym "ab"), unconsSymbolTy (sym "")
  , C.mkFunTy intTy (nTy intTy), C.mkFunTy wTy x, C.mkFunTy (C.mkFunTy intTy x) wTy
  , C.ForAllTy aTv (C.mkFunTy (C.VarTy aTv) (nTy (C.VarTy aTv)))
  , C.ForAllTy bTv (C.mkFunTy x (C.VarTy bTv))
  , C.ForAllTy aTv (C.ForAllTy bTv (C.mkFunTy (C.VarTy aTv) (pTy (C.VarTy bTv))))
  , C.mkFunTy intTy (C.ForAllTy aTv (C.mkFunTy (C.VarTy aTv) wTy))
  , C.AnnType [] (nTy intTy), nTy (C.AnnType [] intTy)
  , lit 5, sym "s", C.liftedTypeKind
  , pairTy, clockTy, resetTy, enableTy, clkPairTy, vecTy (lit 2) clockTy
  , vecTy (lit 2) intTy
  ]

-- | Assert that a function on scoped types agrees with a function on Clash
-- types, on every type of the list: the check gets the type, the result of the
-- Clash function, and the converted type in its scope
agreesOn
  :: [C.Type]
  -> (C.Type -> a)
  -> (forall n. Distinct n => C.Type -> a -> Scope n -> Type n -> Assertion)
  -> Assertion
agreesOn tys old check = forM_ tys $ \ty ->
  withTypeScopes ty $ \sc ty' -> check ty (old ty) sc ty'

-- | 'agreesOn' the corpus, for functions giving a type
agreesTy
  :: (C.Type -> C.Type) -> (forall n. Distinct n => Scope n -> Type n -> Type n) -> Assertion
agreesTy old new = agreesOn corpus old $ \_ e sc ty -> assertScopedTyAlphaEq e (new sc ty)

-- | 'agreesOn' the corpus, for functions giving an optional type
agreesMaybeTy
  :: (C.Type -> Maybe C.Type)
  -> (forall n. Distinct n => Scope n -> Type n -> Maybe (Type n))
  -> Assertion
agreesMaybeTy old new =
  agreesOn corpus old $ \_ e sc ty -> assertScopedMaybeTyAlphaEq e (new sc ty)

-- | 'agreesOn' the corpus, for functions giving a value without types
agreesEq
  :: (Eq a, Show a) => (C.Type -> a) -> (forall n. Distinct n => Scope n -> Type n -> a) -> Assertion
agreesEq old new = agreesOn corpus old $ \ty e sc ty' ->
  assertEqual ("on " <> show ty) e (new sc ty')

-- | Compare views, see 'tyView'
assertViewAgrees :: C.Type -> C.TypeView -> TypeView n -> Assertion
assertViewAgrees ty e a = case (e, a) of
  (C.FunTy arg res, FunTy arg' res') -> do
    assertScopedTyAlphaEq arg arg'
    assertScopedTyAlphaEq res res'
  (C.TyConApp tc args, TyConApp tc' args') -> do
    assertEqual "type constructor" tc tc'
    assertEqual "arguments" args (map toClashType args')
  (C.OtherType t, OtherType t') -> assertScopedTyAlphaEq t t'
  _ -> assertFailure ("views differ on " <> show ty)

-- | The shape of a split function type: which are quantified type variables
-- and which are argument types
piTeleShape :: PiTele n l -> [Bool]
piTeleShape = \case
  PiNil -> []
  PiForall _ rest -> False : piTeleShape rest
  PiArg _ rest -> True : piTeleShape rest

-- | Compare split function types: they have the same shape, and give the same
-- type back
assertSplitAgrees :: ([Either C.TyVar C.Type], C.Type) -> PiTele n l -> Type l -> Assertion
assertSplitAgrees (args, res) tele res' = do
  assertEqual "shape" (map (either (const False) (const True)) args) (piTeleShape tele)
  assertScopedTyAlphaEq (C.mkPolyFunTy res args) (mkPolyFunTy tele res')

tests :: TestTree
tests = testGroup "Clash.Tests.Core.Scoped.Type"
  [ testGroup "agrees with Clash.Core.Type"
    [ testCase "tyView" $
        agreesOn corpus C.tyView $ \ty e _ ty' -> assertViewAgrees ty e (tyView ty')
    , testCase "coreView1" $ agreesMaybeTy (C.coreView1 tcm) (coreView1 tenv)
    , testCase "coreView" $ agreesTy (C.coreView tcm) (coreView tenv)
    , testCase "normalizeType" $ agreesTy (C.normalizeType tcm) (normalizeType tenv)
    , testCase "reduceTypeFamily" $
        agreesMaybeTy (C.reduceTypeFamily tcm) (reduceTypeFamily tenv)
    , testCase "isTypeFamilyApplication" $
        agreesEq (C.isTypeFamilyApplication tcm) (const (isTypeFamilyApplication tenv))
    , testCase "splitTyConAppM" $
        agreesOn corpus C.splitTyConAppM $ \_ e _ ty ->
          assertEqual "split" e (fmap (fmap (map toClashType)) (splitTyConAppM ty))
    , testCase "splitFunTy" $
        agreesOn corpus (C.splitFunTy tcm) $ \_ e sc ty ->
          assertEqual "split" e
            (fmap (\(t1, t2) -> (toClashType t1, toClashType t2)) (splitFunTy tenv sc ty))
    , testCase "splitFunTys" $
        agreesOn corpus (C.splitFunTys tcm) $ \_ e sc ty -> case splitFunTys tenv sc ty of
          (args, res) -> assertEqual "split" e (map toClashType args, toClashType res)
    , testCase "applyFunTy" $
        agreesOn (filter (C.isFunTy tcm) corpus) (\ty -> C.applyFunTy tcm ty intTy) $
          \_ e sc ty -> assertScopedTyAlphaEq e (applyFunTy tenv sc ty (fromClashTypeClosed sc intTy))
    , testCase "isFunTy" $ agreesEq (C.isFunTy tcm) (isFunTy tenv)
    , testCase "isPolyTy" $ agreesEq C.isPolyTy (const isPolyTy)
    , testCase "isPolyFunTy" $ agreesEq C.isPolyFunTy (const isPolyFunTy)
    , testCase "isPolyFunCoreTy" $ agreesEq (C.isPolyFunCoreTy tcm) (isPolyFunCoreTy tenv)
    , testCase "splitFunForallTy" $
        agreesOn corpus C.splitFunForallTy $ \_ e _ ty ->
          case splitFunForallTy ty of SplitFunForall tele res -> assertSplitAgrees e tele res
    , testCase "splitCoreFunForallTy" $
        agreesOn corpus (C.splitCoreFunForallTy tcm) $ \_ e sc ty ->
          case splitCoreFunForallTy tenv sc ty of
            SplitCoreFunForall _ tele res -> assertSplitAgrees e tele res
    , testCase "tyNatSize" $
        agreesEq (eitherToMaybe . runExcept . C.tyNatSize tcm)
          (\sc -> eitherToMaybe . runExcept . tyNatSize tenv sc)
    , testCase "tyLitShow" $
        agreesEq (eitherToMaybe . runExcept . C.tyLitShow tcm)
          (\sc -> eitherToMaybe . runExcept . tyLitShow tenv sc)
    , testCase "isSignalType" $ agreesEq (C.isSignalType tcm) (isSignalType tenv)
    , testCase "isEnable" $ agreesEq (C.isEnable tcm) (isEnable tenv)
    , testCase "isClockOrReset" $ agreesEq (C.isClockOrReset tcm) (isClockOrReset tenv)
    , testCase "isClassTy" $ agreesEq (C.isClassTy tcm) (const (isClassTy tenv))
    , testCase "isIntegerTy" $ agreesEq C.isIntegerTy (const isIntegerTy)
    , testCase "typeAttrs" $ agreesEq C.typeAttrs (const typeAttrs)
    , testCase "stripAnnTypes" $ agreesTy C.stripAnnTypes (const stripAnnTypes)
    , testCase "stripIP" $ agreesTy C.stripIP (const stripIP)
    ]

  , testCase "tyNatSize of a type family application" $
      withTypeScopes (addTy (lit 3) (lit 4)) $ \sc ty ->
        runExcept (tyNatSize tenv sc ty) @?= Right 7

  , testCase "tyNatSize fails on a type that is not a number" $
      withTypeScopes (maybeTy intTy) $ \sc ty ->
        assertBool "fails" (isLeft (runExcept (tyNatSize tenv sc ty)))

    -- A newtype whose representation type binds a type variable, applied to a
    -- type variable with the same raw name as that binder
  , testCase "coreView1 renames binders of a newtype that would capture" $
      withTypeScopes (pTy c) $ \sc ty ->
        assertScopedMaybeTyAlphaEq
          (Just (C.ForAllTy bTv (C.mkFunTy c (C.VarTy bTv))))
          (coreView1 tenv sc ty)

    -- The same for a type family whose right-hand side binds a type variable
  , testCase "reduceTypeFamily renames binders that would capture" $
      withTypeScopes (qTy c) $ \sc ty ->
        assertScopedMaybeTyAlphaEq
          (Just (C.ForAllTy bTv (C.mkFunTy c (C.VarTy bTv))))
          (reduceTypeFamily tenv sc ty)

  , testCase "reduceTypeFamily of builtin type families" $
      forM_ [addTy (lit 3) (lit 4), cmpNatTy (lit 1) (lit 2), unconsSymbolTy (sym "ab")] $ \ty0 ->
        withTypeScopes ty0 $ \sc ty ->
          assertBool ("reduces: " <> show ty0) (isJust (reduceTypeFamily tenv sc ty))

  , testCase "reduceTypeFamily matches repeated pattern variables" $ do
      withTypeScopes (maxTy x x) $ \sc ty ->
        assertScopedMaybeTyAlphaEq (Just x) (reduceTypeFamily tenv sc ty)
      withTypeScopes (maxTy x intTy) $ \sc ty ->
        assertScopedMaybeTyAlphaEq (Just (maybeTy intTy)) (reduceTypeFamily tenv sc ty)

  , testCase "reduceTypeFamily applies the remaining arguments" $
      withTypeScopes (C.AppTy kTy intTy) $ \sc ty ->
        assertScopedMaybeTyAlphaEq (Just (maybeTy intTy)) (reduceTypeFamily tenv sc ty)

  , testCase "reduceTypeFamily reduces the arguments" $
      withTypeScopes (fTy (fTy intTy)) $ \sc ty ->
        -- F (F Int) = F Bool = N Bool
        assertScopedMaybeTyAlphaEq (Just (nTy boolTy)) (reduceTypeFamily tenv sc ty)

    -- Unlike Clash.Core.Type, an equation whose right-hand side mentions a
    -- pattern variable that its left-hand side doesn't bind doesn't reduce
  , testCase "reduceTypeFamily of an equation with an unbound pattern variable" $ do
      let uTc = mkTestFunTyCon (mkTyConName "U" 3105) (C.mkFunTy C.liftedTypeKind C.liftedTypeKind) 1
                  [([intTy], C.VarTy xTv)]
          tenv' = mkTypeEnv (UniqMap.insert (C.tyConName uTc) uTc tcm)
          uInt = C.mkTyConApp (C.tyConName uTc) [intTy]
      withTypeScopes uInt $ \sc ty ->
        assertScopedMaybeTyAlphaEq Nothing (reduceTypeFamily tenv' sc ty)

  , testCase "splitCoreFunForallTy gives the scope of the result type" $
      withTypeScopes (C.ForAllTy aTv (C.mkFunTy (C.VarTy aTv) (signalTy dom (nTy (C.VarTy aTv))))) $
        \sc ty -> case splitCoreFunForallTy tenv sc ty of
          SplitCoreFunForall sc' (PiForall a (PiArg _ PiNil)) res -> do
            let a' = C.VarTy (toClashTyBinder a)
            -- The result type keeps the Signal, and looks through it in its
            -- scope
            assertScopedTyAlphaEq (signalTy dom (nTy a')) res
            assertScopedTyAlphaEq (maybeTy a') (coreView tenv sc' res)
          _ -> assertFailure "unexpected split"

  , testCase "piTele arities" $
      case splitFunForallTy (fromClashTypeClosed emptyScope
             (C.ForAllTy aTv (C.mkFunTy (C.VarTy aTv) (C.ForAllTy bTv (C.mkFunTy intTy (C.VarTy bTv)))))) of
        SplitFunForall tele _ -> do
          piTeleTyArity tele @?= 2
          piTeleTmArity tele @?= 2

  , testCase "piTeleNameBinders moves a result type out of the telescope" $
      case splitFunForallTy (fromClashTypeClosed emptyScope
             (C.ForAllTy aTv (C.mkFunTy (C.VarTy aTv) intTy))) of
        SplitFunForall tele res -> do
          assertScopedMaybeTyAlphaEq (Just intTy) (unsinkType (piTeleNameBinders tele) res)
          case splitFunForallTy (fromClashTypeClosed emptyScope
                 (C.ForAllTy aTv (C.mkFunTy intTy (C.VarTy aTv)))) of
            SplitFunForall tele' res' -> do
              assertBool "mentions a" (not (nullNameSet (freeTyVarsOfType res')))
              assertScopedMaybeTyAlphaEq Nothing (unsinkType (piTeleNameBinders tele') res')
  ]
 where
  eitherToMaybe :: Either e a -> Maybe a
  eitherToMaybe = either (const Nothing) Just
