{-|
  Copyright  :  (C) 2026, QBayLogic B.V.
  License    :  BSD2 (see the file LICENSE)
  Maintainer :  QBayLogic B.V. <devops@qbaylogic.com>

  Tests for solving type equations in scoped Clash Core. Most tests check
  agreement with "Clash.Core.EqSolver".
-}

{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module Clash.Tests.Core.Scoped.EqSolver (tests) where

import Control.Monad (forM_)
import Data.Maybe (fromMaybe, isNothing)

import Test.Tasty
import Test.Tasty.HUnit

import qualified Clash.Core.DataCon as C
import qualified Clash.Core.EqSolver as C
import qualified Clash.Core.Name as C
import qualified Clash.Core.Term as C
import qualified Clash.Core.TyCon as C
import qualified Clash.Core.Type as C
import qualified Clash.Core.TysPrim as C
import qualified Clash.Core.Util as C
import qualified Clash.Core.Var as C
import qualified Clash.Core.VarEnv as C

import Clash.Core.Scoped.Convert
import Clash.Core.Scoped.EqSolver
import Clash.Core.Scoped.NameMap
import Clash.Core.Scoped.Subst (substTm)
import Clash.Core.Scoped.Syntax
import Clash.Core.Scoped.DataCon (withDataConPat)
import Clash.Core.Scoped.Type (mkFunTy)
import Clash.Core.Scoped.TypeEnv (TypeEnv)

import Test.Clash.Rewrite
  ( addTy, boolTy, eqTy, fTy, intTy, kindedTyVar, localId, maybeTy, nTy
  , testTyConMap, vConsDataCon, vNilDataCon, vTy )
import Test.Clash.Scoped

tcm :: C.TyConMap
tcm = testTyConMap

tenv :: TypeEnv
tenv = testTypeEnv

mTv, n0Tv, xTv :: C.TyVar
mTv = kindedTyVar C.User "m" 5000 C.typeNatKind
n0Tv = kindedTyVar C.User "n" 5001 C.typeNatKind
xTv = kindedTyVar C.User "x" 5002 C.liftedTypeKind

m, n0, x :: C.Type
m = C.VarTy mTv
n0 = C.VarTy n0Tv
x = C.VarTy xTv

lit :: Integer -> C.Type
lit = C.LitTy . C.NumTy

-- | Equations, to solve for @m@
equations :: [(C.Type, C.Type)]
equations =
  [ (lit 5, addTy m (lit 1))
  , (addTy (lit 2) m, lit 5)
  , (lit 0, addTy m (lit 1))
  , (m, intTy)
  , (intTy, m)
  , (maybeTy m, maybeTy boolTy)
  , (maybeTy intTy, boolTy)
  , (nTy intTy, maybeTy m)
  , (intTy, boolTy)
  , (lit 3, lit 4)
  , (lit 3, lit 3)
  , (fTy m, intTy)
  , (x, intTy)
  ]

-- | A solution, comparable between Clash Core and scoped Core
type Sol = Either String (Integer, C.Type)

oldSol :: C.TypeEqSolution -> Sol
oldSol = \case
  C.Solution (tv, ty) -> Right (toInteger (C.varUniq tv), ty)
  C.AbsurdSolution -> Left "absurd"
  C.NoSolution -> Left "none"

newSol :: TypeEqSolution n -> Sol
newSol = \case
  Solution tv ty -> Right (toInteger (tyNameRaw tv), toClashType ty)
  AbsurdSolution -> Left "absurd"
  NoSolution -> Left "none"

-- | Convert an equation in the scope of the free type variables of the
-- equations, with the set of type variables to solve for: @m@
withEquation
  :: (C.Type, C.Type)
  -> (forall n. Distinct n => Scope n -> NameSet TyName n -> (Type n, Type n) -> Assertion)
  -> Assertion
withEquation (l, r) k = withFreeTyVarsOf [m, n0, x] $ \env ->
  case fromClashType env m of
    VarTy _ _ mName ->
      k (convScope env) (singletonNameSet mName) (fromClashType env l, fromClashType env r)
    _ -> assertFailure "unexpected conversion"

-- | A pattern of a constructor of @V n@, as Clash Core
oldPat :: C.DataCon -> C.Type -> C.Pat
oldPat dc inst = C.DataPat dc exts ids
 where
  exts = C.dcExtTyVars dc
  argTys = fromMaybe (error "oldPat") (C.dataConInstArgTys dc (inst : map C.VarTy exts))
  ids = zipWith (\i ty -> localId C.User "f" i ty) [5100..] argTys

-- | A telescope of type variable binders and a type, as a Clash type
forAllTele :: Tele TyBinder n l -> Type l -> C.Type
forAllTele tele ty = case tele of
  TNil -> toClashType ty
  TCons b bs -> C.ForAllTy (toClashTyBinder b) (forAllTele bs ty)

-- | The field types, as a single Clash type
fieldsTy :: [C.Type] -> C.Type
fieldsTy = foldr C.mkFunTy intTy

-- | Instantiations of @V n@
vInsts :: [C.Type]
vInsts = [lit 3, lit 0, n0, addTy (lit 1) (lit 1)]

tests :: TestTree
tests = testGroup "Clash.Tests.Core.Scoped.EqSolver"
  [ testGroup "agrees with Clash.Core.EqSolver"
    [ testCase "solveEq" $ forM_ equations $ \eq -> withEquation eq $ \sc set eq' ->
        map newSol (solveEq tenv sc set eq') @?=
          map oldSol (C.solveEq tcm (C.mkVarSet [mTv]) eq)
    , testCase "solveAdd" $ forM_ equations $ \eq -> withEquation eq $ \_ set eq' ->
        newSol (solveAdd set eq') @?= oldSol (C.solveAdd (C.mkVarSet [mTv]) eq)
    , testCase "normalizeAdd" $ forM_ equations $ \eq -> withEquation eq $ \_ _ eq' ->
        fmap (\(a, b, ty) -> (a, b, toClashType ty)) (normalizeAdd eq') @?= C.normalizeAdd eq
    , testCase "solveNonAbsurds" $
        forM_ [equations, take 2 equations, drop 3 equations] $ \eqs ->
          withFreeTyVarsOf [m, n0, x] $ \env -> case fromClashType env m of
            VarTy _ _ mName ->
              map (newSol . uncurry Solution)
                (solveNonAbsurds tenv (convScope env) (singletonNameSet mName)
                  [(fromClashType env l, fromClashType env r) | (l, r) <- eqs])
                @?= map (oldSol . C.Solution) (C.solveNonAbsurds tcm (C.mkVarSet [mTv]) eqs)
            _ -> assertFailure "unexpected conversion"
    , testCase "isAbsurdEq" $ forM_ equations $ \eq -> withEquation eq $ \sc set eq' ->
        isAbsurdEq tenv sc set eq' @?= C.isAbsurdEq tcm (C.mkVarSet [mTv]) eq
    , testCase "typeEq" $
        forM_ [eqTy C.typeNatKind m (lit 1), intTy, nTy intTy] $ \ty ->
          withTypeScopes ty $ \sc ty' ->
            fmap (\(l, r) -> (toClashType l, toClashType r)) (typeEq tenv sc ty')
              @?= C.typeEq tcm ty
    , testCase "isAbsurdPat" $
        forM_ [vNilDataCon, vConsDataCon] $ \dc -> forM_ vInsts $ \inst ->
          withFreeTyVarsOf [inst] $ \env -> do
            let sc = convScope env
                names = map fieldName [1 .. fromIntegral (length (C.dcArgTys dc))]
                r = withDataConPat testTypeEnv sc dc [fromClashType env inst] names $
                      \_ pat _ -> isAbsurdPat tenv sc pat
            r @?= Just (C.isAbsurdPat tcm (oldPat dc inst))
    , testCase "dataConInstArgTysE" $
        forM_ [vNilDataCon, vConsDataCon] $ \dc -> forM_ vInsts $ \inst ->
          withFreeTyVarsOf [inst] $ \env -> do
            let old = C.dataConInstArgTysE (C.mkInScopeSet (C.mkVarSet [n0Tv])) tcm dc [inst]
                new = dataConInstArgTysE tenv (convScope env) dc [fromClashType env inst] $
                        \sc ext tys -> forAllTele ext (foldr mkFunTy (fromClashTypeClosed sc intTy) tys)
                oldTy = fmap (\tys -> foldr C.ForAllTy (fieldsTy tys) (C.dcExtTyVars dc)) old
            assertEqual ("instantiated at " <> show inst) oldTy new
    ]

  , testCase "isAbsurdPat" $
      forM_ [(vNilDataCon, 3, True), (vNilDataCon, 0, False), (vConsDataCon, 0, True), (vConsDataCon, 3, False)] $
        \(dc, i, absurd) -> do
          let names = map fieldName [1 .. fromIntegral (length (C.dcArgTys dc))]
              r = withDataConPat testTypeEnv emptyScope dc [LitTy (C.NumTy i)] names $
                    \_ pat _ -> isAbsurdPat tenv emptyScope pat
          assertEqual (show (C.dcName dc) <> " at " <> show i) (Just absurd) r

  , testCase "solvePatExistentials substitutes in the pattern and the alternative" $ do
      -- case s of VCons @m (eq :: 3 ~ m + 1) (i :: Int) (xs :: V m) -> xs
      let pat0 = oldPat vConsDataCon (lit 3)
          xs = case pat0 of
            C.DataPat _ _ ids -> last ids
            _ -> error "unexpected pattern"
          sId = localId C.User "s" 5200 (vTy (lit 3))
          tm = C.Case (C.Var sId) intTy [(pat0, C.Var xs)]
      withFreeVarsOf [tm] $ \env -> case fromClashTerm env tm of
        Case _ _ [Alt pat body] ->
          case solvePatExistentials tenv (convScope env) pat $ \s pat' ->
                 (lastFieldTy pat', varTy (substTm s body)) of
            Just (Just xsTy, Just ty) -> do
              assertEqual "field type" (vTy (lit 2)) xsTy
              assertEqual "type in the alternative" (vTy (lit 2)) ty
            _ -> assertFailure "not solved"
        _ -> assertFailure "unexpected conversion"

  , testCase "solvePatExistentials without solutions" $
      withFreeTyVarsOf [n0] $ \env -> do
        let sc = convScope env
            r = withDataConPat testTypeEnv sc vConsDataCon [fromClashType env n0] (map fieldName [1..3]) $
                  \_ pat _ -> isNothing (solvePatExistentials tenv sc pat (\_ _ -> ()))
        r @?= Just True
  ]
 where
  fieldName = C.mkUnsafeSystemName "f"

-- | The type of a variable
varTy :: Term n -> Maybe C.Type
varTy = \case
  Var _ ty _ -> Just (toClashType ty)
  _ -> Nothing

-- | The type of the last field of a pattern
lastFieldTy :: Pat n l -> Maybe C.Type
lastFieldTy = \case
  DataPat _ _ ids -> go ids
  _ -> Nothing
 where
  go :: Tele IdBinder x y -> Maybe C.Type
  go TNil = Nothing
  go (TCons b TNil) = Just (toClashType (idBinderType b))
  go (TCons _ bs) = go bs
