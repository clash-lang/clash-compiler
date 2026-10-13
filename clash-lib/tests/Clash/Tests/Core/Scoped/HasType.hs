{-|
  Copyright  :  (C) 2026, QBayLogic B.V.
  License    :  BSD2 (see the file LICENSE)
  Maintainer :  QBayLogic B.V. <devops@qbaylogic.com>

  Tests for the types of scoped Clash Core terms. Like
  "Clash.Tests.Core.Scoped.Type", most tests check agreement with
  "Clash.Core.HasType", in the scope of the free variables of their input, and
  in a larger scope where all of its binders clash with the scope. The
  expected types are those that "Clash.Core.HasType" gave.
-}

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module Clash.Tests.Core.Scoped.HasType (tests) where

import Control.Monad (forM_)

import Test.Tasty
import Test.Tasty.HUnit

import qualified Clash.Core.Literal as C
import qualified Clash.Core.Name as C
import qualified Clash.Core.Term as C (IsMultiPrim (MultiResult), WorkInfo (WorkNever))
import qualified Clash.Core.Type as C (ConstTy (TyCon), LitTy (CharTy))

import Clash.Core.Scoped.Alpha (aeqTy)
import Clash.Core.Scoped.Builder
  ( TmB, TyB, annTyB, appB, appTyB, castB, constTyB, deDupTickB, forAllTyB, funTyB, litTyB, primB
  , runClosedType, tickB, tyAppB, tyLamB, varTyB)
import Clash.Core.Scoped.Convert (mkPrimInfo)
import Clash.Core.Scoped.HasType
import Clash.Core.Scoped.Syntax
import Clash.Core.Scoped.Type (mkFunTy)
import Clash.Core.Scoped.TypeEnv (TypeEnv)
import qualified Clash.Core.Scoped.TysPrim as P

import Test.Clash.Rewrite (mkTyConName, showPprU)
import Test.Clash.Scoped

tenv :: TypeEnv
tenv = testTypeEnv

-- | The free variables of the terms and types
decls :: [ScopeDecl]
decls =
  [ tyVarD "a", tyVarD "c", varD "x" "Int", varD "y" "N Int", varD "z" "Int", varD "v" "a" ]

-- | Run an assertion in the scope of 'decls', and sunk into a larger scope,
-- where all binders of the values are below the watermark of the scope
inScopes :: (forall n. Distinct n => TestScope n -> Assertion) -> Assertion
inScopes k = withTestScope decls $ \ts0 -> do
  k ts0
  extendTestScope ts0 [sunkD] k

-- | Assert that a type is the expected one: a type built in the scope, or a
-- closed type
data Expected = Ty TestType | Closed (Type 'VoidS)

assertTypeIsE :: Distinct n => TestScope n -> Expected -> Type n -> Assertion
assertTypeIsE ts = \case
  Ty t -> assertTypeIs ts t
  Closed t -> \actual ->
    assertBool ("expected " <> showPprU t <> ", got " <> showPprU actual) (aeqTy (sinkClosed t) actual)

-- | @w :: forall a. a -> W@, where @W@ is a newtype of @Int -> Int@
wPrim :: TmB
wPrim = primB (testPrim "w" "forall a. a -> W")

-- | A multi-result primitive @mr :: forall a. a -> (a, Int)@
mrPrim :: PrimInfo
mrPrim =
  mkPrimInfo "mr" (runClosedType (forallA (\a -> funTyB [a] (tupleB a intB)))) C.WorkNever
    C.MultiResult NoUnfolding

-- | A coercion primitive, as 'Clash.Core.Scoped.Util.primCo' makes them
coPrim :: TmB
coPrim = primB (testPrim "_CO_" "forall a. a")

-- | @forall a. ..@, with the given body
forallA :: (TyB -> TyB) -> TyB
forallA f = forAllTyB aKey (C.mkUnsafeSystemName "a" 0) (testTypeB "Type") (f (varTyB aKey))

aKey :: Int
aKey = 77001

intB :: TyB
intB = testTypeB "Int"

-- | A tuple type, like 'Test.Clash.Rewrite.tupleTy'
tupleB :: TyB -> TyB -> TyB
tupleB t1 t2 = appTyB (appTyB (constTyB (C.TyCon (mkTyConName "GHC.Tuple.(,)" 3036))) t1) t2

-- | A term made with the builders, around the given parsed one
wrapB :: (TmB -> TmB) -> TestTerm -> TestTerm
wrapB f t = t {testTermB = f (testTermB t)}

-- | Terms with all constructors, binders, and applications that look through
-- Signals and newtypes, and their types
terms :: [(TestTerm, Expected)]
terms =
  [ ("x", Ty "Int")
  , ("y", Ty "N Int")
  , ("1", Closed P.intPrimTy)
  , ("Just", Ty "forall (a :: Type). a -> Maybe a")
  , ("Just @Int", Ty "Int -> Maybe Int")
  , ("Just @Int x", Ty "Maybe Int")
  , ("\\(x :: Int) -> x", Ty "Int -> Int")
  , ("\\(z :: Int) -> x", Ty "Int -> Int")
  , ("\\(a :: Type) (v :: a) -> v", Ty "forall (a :: Type). a -> a")
  , ("\\(a :: Type) (b :: Type) (v :: a) -> v", Ty "forall (a :: Type) (b :: Type). a -> a")
  , ("(\\(a :: Type) (v :: a) -> v) @Int", Ty "Int -> Int")
  , ("(\\(a :: Type) (v :: a) -> v) @c", Ty "c -> c")
  , ("(\\(a :: Type) (v :: a) -> v) @Int x", Ty "Int")
  , ("let { !(z :: Int) = x } in z", Ty "Int")
  , ("let { !(z :: Int) = x } in \\(x :: Int) -> z", Ty "Int -> Int")
  , ("let { (z :: Int) = z } in \\(x :: Int) -> z", Ty "Int -> Int")
  , ("(case x of { _ -> Just }) :: Bool", Ty "Bool")
  , (wrapB (\e -> castB e intB (testTypeB "W")) "x", Ty "W")
  , (wrapB (tickB deDupTickB) "(id_G4200 :: forall a. a -> a) @Int x", Ty "Int")
  , ("(id_G4200 :: forall a. a -> a) @Int x", Ty "Int")
  , ("(const_G4201 :: forall a b. a -> b -> a) @Int @(N Int) x y", Ty "Int")
  , ("(const_G4201 :: forall a b. a -> b -> a) @c @Int", Ty "c -> Int -> c")
  , ("(undefined_G4202 :: forall a. a) @(forall b. b -> b) @Int", Ty "Int -> Int")
  , ("(undefined_G4202 :: forall a. a) @(forall b. b -> b) @Int x", Ty "Int")
  , ("(sig_G4203 :: forall a. Signal a (Int -> Int)) @\"System\" x", Ty "Int")
  , (wrapB (\x -> appB (appB (tyAppB wPrim intB) x) x) "x", Ty "Int")
  , ( builtTerm (primB mrPrim)
    , Ty (builtType (forallA (\a -> funTyB [a, a, intB] (tupleB a intB)))) )
  , ( wrapB (\v -> tyLamB aKey (C.mkUnsafeSystemName "a" 0) (testTypeB "Type")
                     (appB (tyAppB (primB mrPrim) (varTyB aKey)) v)) "v"
    , Ty (builtType (forallA (\a -> funTyB [a, intB] (tupleB a intB)))) )
    -- A coercion with an open type
  , ( builtTerm (tyLamB aKey (C.mkUnsafeSystemName "a" 0) (testTypeB "Type")
                  (tyAppB coPrim (appTyB (testTypeB "N") (varTyB aKey))))
    , Ty "forall (a :: Type). N a" )
  ]

tests :: TestTree
tests = testGroup "Clash.Tests.Core.Scoped.HasType"
  [ testCase "inferCoreTypeOf" $
      forM_ terms $ \(tm, expected) -> withTestScope decls $ \ts0 -> do
        let tm0 = termIn ts0 tm
        assertTypeIsE ts0 expected (inferCoreTypeOf tenv (testScope ts0) tm0)
        withClashing ts0 [sunkD] tm0 $ \ts tm1 ->
          assertTypeIsE ts expected (inferCoreTypeOf tenv (testScope ts) tm1)

  , testCase "literalType" $
      forM_ literals $ \(l, expected) ->
        assertTypeIsE emptyTestScope (Closed expected) (literalType l :: Type 'VoidS)

  , testCase "primInfoType of a multi-result primitive" $
      -- forall a. a -> a -> Int -> (a, Int)
      assertTypeIs emptyTestScope
        (builtType (forallA (\a -> funTyB [a, a, intB] (tupleB a intB))))
        (primInfoType mrPrim :: Type 'VoidS)

  , testCase "piResultTys" $
      forM_ piCases $ \(ty, args, expected) -> inScopes $ \ts ->
        assertTypeIs ts expected
          (piResultTys tenv (testScope ts) (typeIn ts ty) (map (typeIn ts) args))

    -- Instantiating @forall a b. a -> b@ with @c@, where @c@ has the same
    -- name as the binder @b@, must rename @b@
  , testCase "piResultTys renames binders that would capture" $
      -- In the empty scope, a gets name 1 and b gets name 2
      withTestScope [atRaw 2 (tyVarD "c")] $ \ts ->
        assertTypeIs ts "forall (b :: Type). c -> b"
          (piResultTys tenv (testScope ts)
            (sinkClosed (closedType "forall (a :: Type) (b :: Type). a -> b"))
            [typeIn ts "c"])

  , testCase "applyTypeToArgs" $
      withTestScope [varD "x" "Int"] $ \ts ->
        let sc = testScope ts
            ty = sinkClosed (closedType "forall a. a -> a -> a")
        in assertTypeIs ts "Int -> Int"
             (applyTypeToArgs (termIn ts "x") tenv sc ty [Right (typeIn ts "Int"), Left (termIn ts "x")])

  , testCase "inferCoreKindOf" $
      forM_ kindCases $ \(ty, expected) -> inScopes $ \ts ->
        assertTypeIsE ts (Closed expected) (inferCoreKindOf tenv (testScope ts) (typeIn ts ty))

    -- With the @debug@ flag, 'piResultTys' checks that the types it drops the
    -- arrows of a function type with match the argument types of the
    -- function type. 'inferCoreKindOf' used it to apply the kind of a type
    -- constructor to its arguments, such as @Type -> Type@ of @Maybe@ to
    -- @Int@. The check then compared @Type@ against @Int@, so
    -- 'inferCoreKindOf' errored.
  , testCase "inferCoreKindOf of a type constructor applied to a type" $
      withTypeAndSunk "Maybe Int" $ \ts ty ->
        assertTypeIs ts "Type" (inferCoreKindOf tenv (testScope ts) ty)
  , testCase "inferCoreKindOf of a type constructor applied to a type variable" $
      withTypeAndSunk "Maybe a" $ \ts ty ->
        assertTypeIs ts "Type" (inferCoreKindOf tenv (testScope ts) ty)
  ]

-- | Literals, and their types
literals :: [(C.Literal, Type 'VoidS)]
literals =
  [ (C.IntegerLiteral 1, P.integerPrimTy), (C.IntLiteral 1, P.intPrimTy)
  , (C.WordLiteral 1, P.wordPrimTy), (C.Int64Literal 1, P.int64PrimTy)
  , (C.Word64Literal 1, P.word64PrimTy), (C.Int8Literal 1, P.int8PrimTy)
  , (C.Int16Literal 1, P.int16PrimTy), (C.Int32Literal 1, P.int32PrimTy)
  , (C.Word8Literal 1, P.word8PrimTy), (C.Word16Literal 1, P.word16PrimTy)
  , (C.Word32Literal 1, P.word32PrimTy), (C.StringLiteral "s", P.stringPrimTy)
  , (C.FloatLiteral 1, P.floatPrimTy), (C.DoubleLiteral 1, P.doublePrimTy)
  , (C.CharLiteral 'c', P.charPrimTy), (C.NaturalLiteral 1, P.naturalPrimTy) ]

-- | Function types, their arguments, and the result
piCases :: [(TestType, [TestType], TestType)]
piCases =
  [ ("forall a. a", ["forall b. b -> b", "Int"], "Int -> Int")
  , ("forall a b. a -> b -> a", ["Int", "Bool"], "Int -> Bool -> Int")
  , ("forall a b. a -> b -> a", ["Int"], "forall b. Int -> b -> Int")
  , ("forall a. a -> a", ["c"], "c -> c")
  , ("Int -> Bool", ["Int"], "Bool")
  , ("forall a. Signal a (forall b. b -> a)", ["Int", "Bool"], "Bool -> Int")
  , ("forall a. P a", ["Int", "Bool"], "Int -> Bool")
  ]

-- | Types whose kinds don't need checking applications of type constructors
-- to their arguments, which 'Clash.Core.HasType.piResultTys' of Clash Core
-- rejected when debugging is on, and their kinds
kindCases :: [(TestType, Kind 'VoidS)]
kindCases =
  [ ("Int", P.liftedTypeKind)
  , ("Int -> Bool", P.liftedTypeKind)
  , ("1", P.typeNatKind)
  , ("\"s\"", P.typeSymbolKind)
  , (builtType (litTyB (C.CharTy 'c')), P.charPrimTy)
  , ("a", P.liftedTypeKind)
  , ("forall a. a", P.liftedTypeKind)
  , ("forall a. a -> a", P.liftedTypeKind)
  , (builtType (annTyB [] intB), P.liftedTypeKind)
    -- The type constructor N itself
  , ("N", mkFunTy P.liftedTypeKind P.liftedTypeKind)
  ]
