{-|
  Copyright  :  (C) 2026, QBayLogic B.V.
  License    :  BSD2 (see the file LICENSE)
  Maintainer :  QBayLogic B.V. <devops@qbaylogic.com>

  Tests for solving type equations in scoped Clash Core.
-}

{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}

module Clash.Tests.Core.Scoped.EqSolver (tests) where

import Control.Monad (forM_)
import Data.Maybe (isNothing)

import Test.Tasty
import Test.Tasty.HUnit

import qualified Clash.Core.DataCon as C
import qualified Clash.Core.Name as C
import qualified Clash.Core.Basic as C (ConstTy (TyCon), LitTy (NumTy))
import Clash.Core.Pretty (PrettyOptions (..), PrettyPrec, showPpr')

import Clash.Core.Scoped.Builder (TyB, appTyB, constTyB, litTyB)
import Clash.Core.Scoped.EqSolver
import Clash.Core.Scoped.NameMap
import Clash.Core.Scoped.Subst (substTm)
import Clash.Core.Scoped.Syntax
import Clash.Core.Scoped.DataCon (withDataConPat)
import Clash.Core.Scoped.Type (mkFunTy)
import Clash.Core.Scoped.TypeEnv (TypeEnv)
import qualified Clash.Core.Scoped.TysPrim as P

import Test.Clash.Rewrite (addTyConName, vConsDataCon, vNilDataCon)
import Test.Clash.Scoped

tenv :: TypeEnv
tenv = testTypeEnv

-- | The free type variables of the equations: @m@ gets name 1, @n@ name 2 and
-- @x@ name 3
decls :: [ScopeDecl]
decls = [kindedTyVarD "m" "Nat", kindedTyVarD "n" "Nat", tyVarD "x"]

litB :: Integer -> TyB
litB = litTyB . C.NumTy

-- | @GHC.TypeNats.+@
addB :: TyB -> TyB -> TyB
addB a b = appTyB (appTyB (constTyB (C.TyCon addTyConName)) a) b

-- | Equations, to solve for @m@
equations :: [(TestType, TestType)]
equations =
  [ ("5", builtType (addB (testTypeB "m") (litB 1)))
  , (builtType (addB (litB 2) (testTypeB "m")), "5")
  , ("0", builtType (addB (testTypeB "m") (litB 1)))
  , ("m", "Int")
  , ("Int", "m")
  , ("Maybe m", "Maybe Bool")
  , ("Maybe Int", "Bool")
  , ("N Int", "Maybe m")
  , ("Int", "Bool")
  , ("3", "4")
  , ("3", "3")
  , ("F m", "Int")
  , ("x", "Int")
  ]

-- | A solution, as the raw name of the variable and the rendered type
type Sol = Either String (Int, String)

sol :: TypeEqSolution n -> Sol
sol = \case
  Solution tv ty -> Right (tyNameRaw tv, pp ty)
  AbsurdSolution -> Left "absurd"
  NoSolution -> Left "none"

-- | Render a value as text
pp :: PrettyPrec p => p -> String
pp = showPpr' (PrettyOptions False False False False)

-- | Run an assertion on each equation, in the scope of 'decls', with the set
-- of type variables to solve for: @m@
forEquations
  :: (forall n. Distinct n => Int -> Scope n -> NameSet TyName n -> (Type n, Type n) -> Assertion)
  -> Assertion
forEquations k = withTestScope decls $ \ts -> case typeIn ts "m" of
  VarTy _ _ mName ->
    sequence_
      [ k i (testScope ts) (singletonNameSet mName) (typeIn ts l, typeIn ts r)
      | (i, (l, r)) <- zip [0 ..] equations ]
  _ -> assertFailure "unexpected type"

-- | Instantiations of @V n@
vInsts :: [TestType]
vInsts = ["3", "0", "n", builtType (addB (litB 1) (litB 1))]

-- | A telescope of type variable binders and a type, as a type
forAllTele :: Tele TyBinder n l -> Type l -> Type n
forAllTele tele ty = case tele of
  TNil -> ty
  TCons b bs -> ForAllTy b (forAllTele bs ty)

-- | The names of the fields of patterns
fieldNames :: Int -> [C.Name a]
fieldNames k = map (C.mkUnsafeSystemName "f") [1 .. fromIntegral k]

tests :: TestTree
tests = testGroup "Clash.Tests.Core.Scoped.EqSolver"
  [ testGroup "the results of Clash.Core.EqSolver"
    [ testCase "solveEq" $ forEquations $ \i sc set eq ->
        assertEqual (show i) (expectedSolveEq !! i) (map sol (solveEq tenv sc set eq))
    , testCase "solveAdd" $ forEquations $ \i _ set eq ->
        assertEqual (show i) (expectedSolveAdd !! i) (sol (solveAdd set eq))
    , testCase "normalizeAdd" $ forEquations $ \i _ _ eq ->
        assertEqual (show i) (expectedNormalizeAdd !! i)
          (fmap (\(a, b, ty) -> (a, b, pp ty)) (normalizeAdd eq))
    , testCase "solveNonAbsurds" $
        forM_ (zip [equations, take 2 equations, drop 3 equations] expectedSolveNonAbsurds) $ \(eqs, expected) ->
          withTestScope decls $ \ts -> case typeIn ts "m" of
            VarTy _ _ mName ->
              map (sol . uncurry Solution)
                (solveNonAbsurds tenv (testScope ts) (singletonNameSet mName)
                  [(typeIn ts l, typeIn ts r) | (l, r) <- eqs])
                @?= expected
            _ -> assertFailure "unexpected type"
    , testCase "isAbsurdEq" $ forEquations $ \i sc set eq ->
        assertEqual (show i) (expectedIsAbsurdEq !! i) (isAbsurdEq tenv sc set eq)
    , testCase "typeEq" $
        forM_ (zip [eqB, "Int", "N Int"] [Just ("m", "1"), Nothing, Nothing]) $ \(ty, expected) ->
          withTestScope decls $ \ts0 -> do
            let check :: Distinct n => TestScope n -> Type n -> Assertion
                check ts ty' = fmap (\(l, r) -> (pp l, pp r)) (typeEq tenv (testScope ts) ty') @?= expected
            check ts0 (typeIn ts0 ty)
            withClashing ts0 [sunkD] (typeIn ts0 ty) check
    , testCase "isAbsurdPat" $
        forM_ (zip [(dc, inst) | dc <- [vNilDataCon, vConsDataCon], inst <- vInsts] expectedIsAbsurdPat) $
          \((dc, inst), expected) -> withTestScope decls $ \ts -> do
            let sc = testScope ts
                r = withDataConPat testTypeEnv sc dc [typeIn ts inst] (fieldNames (C.dcFieldCount dc)) $
                      \_ pat _ -> isAbsurdPat tenv sc pat
            r @?= Just expected
    , testCase "dataConInstArgTysE" $
        forM_ (zip [(dc, inst) | dc <- [vNilDataCon, vConsDataCon], inst <- vInsts] expectedDataConInstArgTysE) $
          \((dc, inst), expected) -> withTestScope decls $ \ts -> do
            let new = dataConInstArgTysE tenv (testScope ts) dc [typeIn ts inst] $
                        \_ ext tys -> pp (forAllTele ext (foldr mkFunTy (sinkClosed (closedType "Int")) tys))
            assertEqual ("instantiated at " <> pp (typeIn ts inst)) (Just expected) new
    ]

  , testCase "isAbsurdPat" $
      forM_ [(vNilDataCon, 3, True), (vNilDataCon, 0, False), (vConsDataCon, 0, True), (vConsDataCon, 3, False)] $
        \(dc, i, absurd) -> do
          let r = withDataConPat testTypeEnv emptyScope dc [LitTy (C.NumTy i)] (fieldNames (C.dcFieldCount dc)) $
                    \_ pat _ -> isAbsurdPat tenv emptyScope pat
          assertEqual (show (C.dcName dc) <> " at " <> show i) (Just absurd) r

  , testCase "solvePatExistentials substitutes in the pattern and the alternative" $
      -- case s of VCons @m (eq :: 3 ~ m + 1) (i :: Int) (xs :: V m) -> xs
      withTestScope [varD "s" "V 3"] $ \ts -> do
        let sc = testScope ts
            r = withDataConPat testTypeEnv sc vConsDataCon [LitTy (C.NumTy 3)] (fieldNames 3) $ \_ pat vars ->
                  case vars of
                    [_, _, xs] -> Case (termIn ts "s") (sinkClosed (closedType "Int")) [Alt pat xs]
                    _ -> error "unexpected fields"
        case r of
          Just (Case _ _ [Alt pat body]) ->
            case solvePatExistentials tenv sc pat $ \s pat' ->
                   (lastFieldTy pat', varTy (substTm s body)) of
              Just (Just xsTy, Just ty) -> do
                assertEqual "field type" "V 2" xsTy
                assertEqual "type in the alternative" "V 2" ty
              _ -> assertFailure "not solved"
          _ -> assertFailure "unexpected term"

  , testCase "solvePatExistentials without solutions" $
      withTestScope decls $ \ts -> do
        let sc = testScope ts
            r = withDataConPat testTypeEnv sc vConsDataCon [typeIn ts "n"] (fieldNames 3) $
                  \_ pat _ -> isNothing (solvePatExistentials tenv sc pat (\_ _ -> ()))
        r @?= Just True
  ]
 where
  -- m ~# 1, of kind Nat
  eqB :: TestType
  eqB = builtType (foldl appTyB eqPrimB [testTypeB "Nat", testTypeB "Nat", testTypeB "m", litB 1])
  eqPrimB = case P.eqPrimTy @'VoidS of
    ConstTy c -> constTyB c
    _ -> error "eqPrimTy: not a type constructor"

-- | The rendered type of a variable
varTy :: Term n -> Maybe String
varTy = \case
  Var _ ty _ -> Just (pp ty)
  _ -> Nothing

-- | The rendered type of the last field of a pattern
lastFieldTy :: Pat n l -> Maybe String
lastFieldTy = \case
  DataPat _ _ ids -> go ids
  _ -> Nothing
 where
  go :: Tele IdBinder x y -> Maybe String
  go TNil = Nothing
  go (TCons b TNil) = Just (pp (idBinderType b))
  go (TCons _ bs) = go bs

-- * The expected results on 'equations'

expectedSolveEq :: [[Sol]]
expectedSolveEq =
  [ [], [], [], [Right (1, "Int")], [Right (1, "Int")], [Right (1, "Bool")], [Left "absurd"]
  , [Right (1, "Int")], [Left "absurd"], [Left "absurd"], [], [Left "absurd"], [] ]

expectedSolveAdd :: [Sol]
expectedSolveAdd =
  [Right (1, "4"), Right (1, "3"), Left "absurd"] <> replicate 10 (Left "none")

expectedNormalizeAdd :: [Maybe (Integer, Integer, String)]
expectedNormalizeAdd = [Just (5, 1, "m"), Just (5, 2, "m"), Just (0, 1, "m")] <> replicate 10 Nothing

expectedIsAbsurdEq :: [Bool]
expectedIsAbsurdEq =
  [False, False, True, False, False, False, True, False, True, True, False, True, False]

expectedSolveNonAbsurds :: [[Sol]]
expectedSolveNonAbsurds =
  [ [Right (1, "4"), Right (1, "3"), Right (1, "Int"), Right (1, "Int"), Right (1, "Bool"), Right (1, "Int")]
  , [Right (1, "4"), Right (1, "3")]
  , [Right (1, "Int"), Right (1, "Int"), Right (1, "Bool"), Right (1, "Int")] ]

-- | For @VNil@ and @VCons@, at each of 'vInsts'
expectedIsAbsurdPat :: [Bool]
expectedIsAbsurdPat = [True, False, False, True, False, True, False, False]

-- | For @VNil@ and @VCons@, at each of 'vInsts'
expectedDataConInstArgTysE :: [String]
expectedDataConInstArgTysE =
  [ "~# Natural Natural 3 0 -> Int"
  , "~# Natural Natural 0 0 -> Int"
  , "~# Natural Natural n 0 -> Int"
  , "~# Natural Natural (+ 1 1) 0 -> Int"
  , "forall m. ~# Natural Natural 3 (+ 2 1) -> Int -> V 2 -> Int"
  , "forall m. ~# Natural Natural 0 (+ m 1) -> Int -> V m -> Int"
  , "forall m. ~# Natural Natural n (+ m 1) -> Int -> V m -> Int"
  , "forall m. ~# Natural Natural (+ 1 1) (+ 1 1) -> Int -> V 1 -> Int"
  ]
