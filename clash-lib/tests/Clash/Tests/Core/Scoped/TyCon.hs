{-|
  Copyright  :  (C) 2026, QBayLogic B.V.
  License    :  BSD2 (see the file LICENSE)
  Maintainer :  QBayLogic B.V. <devops@qbaylogic.com>

  Tests for type constructors in scoped Clash Core
-}

{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Clash.Tests.Core.Scoped.TyCon (tests) where

import Control.Monad (forM_)
import qualified Data.Char as Char
import qualified Data.List as List
import Test.Tasty
import Test.Tasty.HUnit

import qualified Clash.Core.Name as C
import qualified Clash.Core.TyCon as C
import qualified Clash.Core.Type as C
import qualified Clash.Core.TysPrim as C
import qualified Clash.Data.UniqMap as UniqMap

import Clash.Core.Scoped.Convert hiding (mkDataCon, mkFunTyCon)
import qualified Clash.Core.Scoped.Convert as Convert (mkFunTyCon)
import Clash.Core.Scoped.Syntax
import Clash.Core.Scoped.TyCon

import Test.Clash.Rewrite
  ( intTy, intTyCon, kindedTyVar, mkDataCon, mkFunTyCon, mkNewTyCon, mkTyConName
  , testTyConMap, tyVar )
import Test.Clash.Scoped (assertScopedTyAlphaEq)

aTv, bTv, cTv, kTv :: C.TyVar
aTv = tyVar C.User "a" 50
bTv = tyVar C.User "b" 51
cTv = tyVar C.User "c" 52
kTv = tyVar C.User "k" 53

-- | @Maybe@, of kind @Type -> Type@
maybeTy :: C.Type -> C.Type
maybeTy = C.AppTy (C.ConstTy (C.TyCon (mkTyConName "Maybe" 2001)))

-- | @newtype N a = MkN (Maybe a)@
nTyCon :: C.TyCon
nTyCon = mkNewTyCon nm [aTv] mkN ([aTv], maybeTy (C.VarTy aTv))
 where
  nm = mkTyConName "N" 2002
  mkN = mkDataCon "MkN" 2003 [aTv] [] [maybeTy (C.VarTy aTv)] (C.AppTy (C.ConstTy (C.TyCon nm)) (C.VarTy aTv))

-- | @newtype P a = MkP (forall b. a -> b)@
pTyCon :: C.TyCon
pTyCon = mkNewTyCon nm [aTv] mkP ([aTv], rhs)
 where
  nm = mkTyConName "P" 2004
  rhs = C.ForAllTy bTv (C.mkFunTy (C.VarTy aTv) (C.VarTy bTv))
  mkP = mkDataCon "MkP" 2005 [aTv] [] [rhs] (C.AppTy (C.ConstTy (C.TyCon nm)) (C.VarTy aTv))

-- | @type family F (a :: k) :: Type where F (a :: k) = k@, i.e. with a pattern
-- variable whose kind mentions another one
fTyCon :: C.TyCon
fTyCon = mkFunTyCon (mkTyConName "F" 2006) C.liftedTypeKind 1 [([C.VarTy akTv], C.VarTy kTv)]
 where
  akTv = kindedTyVar C.User "a" 54 (C.VarTy kTv)

tests :: TestTree
tests = testGroup "Clash.Tests.Core.Scoped.TyCon"
  [ testCase "newTyConSig" $
      case newTyConSig nTyCon of
        Just (NewTyConSig (TCons a TNil) rhs) -> do
          case rhs of
            AppTy _ (VarTy _ _ n) -> tyNameRaw n @?= tyNameRaw (tyNameOf a)
            _ -> assertFailure "unexpected representation type"
        _ -> assertFailure "unexpected signature"

  , testCase "newTyConSig of a type that is not a newtype" $
      assertBool "not a newtype" (null (newTyConSig intTyCon))

  , testCase "newTyConInstRhs" $ do
      let int = fromClashTypeClosed emptyScope intTy
      sig <- maybe (assertFailure "no signature") pure (newTyConSig nTyCon)
      case newTyConInstRhs emptyScope sig [int] of
        Just ty -> assertScopedTyAlphaEq (maybeTy intTy) ty
        Nothing -> assertFailure "no instantiation"
      -- More types than type variables are applied to the result
      case newTyConInstRhs emptyScope sig [int, int] of
        Just ty -> assertScopedTyAlphaEq (C.AppTy (maybeTy intTy) intTy) ty
        Nothing -> assertFailure "no instantiation"
      assertBool "too few types" (null (newTyConInstRhs emptyScope sig []))

    -- Instantiating @a := c@, where the free type variable @c@ has the same
    -- name as the @b@ the representation type binds, must rename @b@.
  , testCase "newTyConInstRhs renames binders that would capture" $ do
      sig <- maybe (assertFailure "no signature") pure (newTyConSig pTyCon)
      -- In the empty scope, a gets name 1 and b gets name 2
      let c = tyVar C.User "c" 2
      withConvEnv [c] [] $ \env ->
        case newTyConInstRhs (convScope env) sig [fromClashType env (C.VarTy c)] of
          Just ty ->
            assertScopedTyAlphaEq (C.ForAllTy cTv (C.mkFunTy (C.VarTy c) (C.VarTy cTv))) ty
          Nothing -> assertFailure "no instantiation"

  , testCase "tyFamEqns orders pattern variables by kind dependency" $
      case tyFamEqns fTyCon of
        Just [TyFamEqn (TCons k (TCons a TNil)) [VarTy _ _ an] (VarTy _ _ kn)] -> do
          C.nameOcc (tyBinderOcc k) @?= "k"
          C.nameOcc (tyBinderOcc a) @?= "a"
          -- The kind of a is k
          case tyBinderKind a of
            VarTy _ _ kn' -> tyNameRaw kn' @?= tyNameRaw (tyNameOf k)
            _ -> assertFailure "unexpected kind"
          tyNameRaw an @?= tyNameRaw (tyNameOf a)
          tyNameRaw kn @?= tyNameRaw (tyNameOf k)
        _ -> assertFailure "unexpected equations"

  , testCase "tyFamEqns of a type that is not a type family" $
      assertBool "not a type family" (null (tyFamEqns nTyCon))

    -- The native constructors derive the types of Clash Core, from which the
    -- constructors of Clash Core derive the same scoped types, up to the
    -- uniques of the names of binders, which scoped Core doesn't use
  , testCase "native constructors round trip through Clash Core" $
      forM_ (fTyCon : UniqMap.elems testTyConMap) $ \tc ->
        sigs (fromLegacy (native tc)) @?= sigs tc
  ]
 where
  sigs tc = map withoutUniqs
    [show (C.tyConKind tc), show (newTyConSig tc), show (tyFamEqns tc)]

  -- Drops the uniques of 'C.Name's from 'show'
  withoutUniqs = \case
    [] -> []
    str@(c:cs)
      | Just rest <- List.stripPrefix "nameUniq = " str ->
          withoutUniqs (dropWhile Char.isDigit rest)
      | otherwise -> c : withoutUniqs cs

  native tc = case tc of
    C.AlgTyCon {} -> mkAlgTyCon nm k ar (nativeRhs (C.algTcRhs tc)) (C.isClassTc tc)
    C.PromotedDataCon {} -> mkPromotedDataCon nm k ar (C.tyConData tc)
    C.FunTyCon {} -> Convert.mkFunTyCon nm k ar (C.tyConEqns tc)
    C.PrimTyCon {} -> mkPrimTyCon nm k ar
   where
    (nm, k, ar) = (C.tyConName tc, C.tyConKind tc, C.tyConArity tc)

  nativeRhs = \case
    C.NewTyCon dc sig _ -> mkNewTyConRhs dc sig
    rhs -> rhs

  fromLegacy tc = case tc of
    C.AlgTyCon {} ->
      mkAlgTyConFromClash u nm k ar (legacyRhs (C.algTcRhs tc)) (C.isClassTc tc)
    C.PromotedDataCon {} -> mkPromotedDataConFromClash u nm k ar (C.tyConData tc)
    C.FunTyCon {} -> mkFunTyConFromClash u nm k ar (C.tyConLegacySubst tc)
    C.PrimTyCon {} -> mkPrimTyConFromClash u nm k ar
   where
    (u, nm, k, ar) = (C.tyConUniq tc, C.tyConName tc, C.tyConLegacyKind tc, C.tyConArity tc)

  legacyRhs = \case
    C.NewTyCon dc _ etad -> mkNewTyConRhsFromClash dc etad
    rhs -> rhs
