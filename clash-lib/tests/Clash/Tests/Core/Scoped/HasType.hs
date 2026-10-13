{-|
  Copyright  :  (C) 2026, QBayLogic B.V.
  License    :  BSD2 (see the file LICENSE)
  Maintainer :  QBayLogic B.V. <devops@qbaylogic.com>

  Tests for the types of scoped Clash Core terms. Like
  "Clash.Tests.Core.Scoped.Type", most tests check agreement with
  "Clash.Core.HasType", in the scope of the free variables of their input, and
  in a larger scope where all of its binders clash with the scope.
-}

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module Clash.Tests.Core.Scoped.HasType (tests) where

import Control.Monad (forM_)

import Test.Tasty
import Test.Tasty.HUnit

import qualified Clash.Core.HasType as C
import qualified Clash.Core.Literal as C
import qualified Clash.Core.Name as C
import qualified Clash.Core.Term as C
import qualified Clash.Core.TyCon as C
import qualified Clash.Core.Type as C
import qualified Clash.Core.TysPrim as C
import qualified Clash.Core.Util as C
import qualified Clash.Core.Var as C

import Clash.Core.Scoped.Convert
import Clash.Core.Scoped.HasType
import Clash.Core.Scoped.Syntax
import Clash.Core.Scoped.TypeEnv (TypeEnv)

import Test.Clash.Rewrite
  ( boolTy, globalId, intTy, justDataCon, localId, maybeTy, nTy, pTy
  , signalTy, testTyConMap, tupleTy, tyVar, wTy )
import Test.Clash.Scoped

tcm :: C.TyConMap
tcm = testTyConMap

tenv :: TypeEnv
tenv = testTypeEnv

aTv, bTv, cTv :: C.TyVar
aTv = tyVar C.User "a" 4000
bTv = tyVar C.User "b" 4001
-- Has the same unique as the name conversion gives the second type variable
-- a closed type binds
cTv = tyVar C.User "c" 2

a, b, c :: C.Type
a = C.VarTy aTv
b = C.VarTy bTv
c = C.VarTy cTv

xId, yId, zId, vId :: C.Id
xId = localId C.User "x" 4100 intTy
yId = localId C.User "y" 4101 (nTy intTy)
zId = localId C.User "z" 4102 intTy
vId = localId C.User "v" 4103 a

x, y, z, v :: C.Term
x = C.Var xId
y = C.Var yId
z = C.Var zId
v = C.Var vId

-- | @id :: forall a. a -> a@
idG :: C.Term
idG = C.Var (globalId C.User "id" 4200 (C.ForAllTy aTv (C.mkFunTy a a)))

-- | @const :: forall a b. a -> b -> a@
constG :: C.Term
constG = C.Var (globalId C.User "const" 4201
  (C.ForAllTy aTv (C.ForAllTy bTv (C.mkFunTy a (C.mkFunTy b a)))))

-- | @undefined :: forall a. a@
undefinedG :: C.Term
undefinedG = C.Var (globalId C.User "undefined" 4202 C.undefinedTy)

-- | @sig :: forall dom. Signal dom (Int -> Int)@
sigG :: C.Term
sigG = C.Var (globalId C.User "sig" 4203
  (C.ForAllTy aTv (signalTy a (C.mkFunTy intTy intTy))))

-- | @w :: forall a. a -> W@, where @W@ is a newtype of @Int -> Int@
wPrim :: C.Term
wPrim = C.Prim (C.PrimInfo "w" (C.ForAllTy aTv (C.mkFunTy a wTy)) C.WorkNever
  C.SingleResult C.NoUnfolding)

-- | A multi-result primitive @mr :: forall a. a -> (a, Int)@
mrPrim :: C.PrimInfo
mrPrim = C.PrimInfo "mr" (C.ForAllTy aTv (C.mkFunTy a (tupleTy [a, intTy]))) C.WorkNever
  C.MultiResult C.NoUnfolding

-- | Terms with all constructors, binders, and applications that look through
-- Signals and newtypes
terms :: [C.Term]
terms =
  [ x, y
  , C.Literal (C.IntLiteral 1)
  , C.Data justDataCon
  , C.TyApp (C.Data justDataCon) intTy
  , C.App (C.TyApp (C.Data justDataCon) intTy) x
  , C.Lam xId x
  , C.Lam zId x
  , C.TyLam aTv (C.Lam vId v)
  , C.TyLam aTv (C.TyLam bTv (C.Lam vId v))
  , C.TyApp (C.TyLam aTv (C.Lam vId v)) intTy
  , C.TyApp (C.TyLam aTv (C.Lam vId v)) c
  , C.App (C.TyApp (C.TyLam aTv (C.Lam vId v)) intTy) x
  , C.Let (C.NonRec zId x) z
  , C.Let (C.NonRec zId x) (C.Lam xId z)
  , C.Let (C.Rec [(zId, z)]) (C.Lam xId z)
  , C.Case x boolTy [(C.DefaultPat, C.Data justDataCon)]
  , C.Cast x intTy wTy
  , C.Tick C.DeDup (C.App (C.TyApp idG intTy) x)
  , C.App (C.TyApp idG intTy) x
  , C.App (C.App (C.TyApp (C.TyApp constG intTy) (nTy intTy)) x) y
  , C.TyApp (C.TyApp constG c) intTy
  , C.TyApp (C.TyApp undefinedG (C.ForAllTy bTv (C.mkFunTy b b))) intTy
  , C.App (C.TyApp (C.TyApp undefinedG (C.ForAllTy bTv (C.mkFunTy b b))) intTy) x
  , C.App (C.TyApp sigG (C.LitTy (C.SymTy "System"))) x
  , C.App (C.App (C.TyApp wPrim intTy) x) x
  , C.Prim mrPrim
  , C.TyLam aTv (C.App (C.TyApp (C.Prim mrPrim) a) v)
    -- A coercion with an open type, see "Clash.Core.Scoped.Convert"
  , C.TyLam aTv (C.primCo (nTy a))
  ]

tests :: TestTree
tests = testGroup "Clash.Tests.Core.Scoped.HasType"
  [ testCase "inferCoreTypeOf agrees with Clash.Core.HasType" $
      forM_ terms $ \tm -> withTermScopes tm $ \sc tm' ->
        assertScopedTyAlphaEq (C.inferCoreTypeOf tcm tm) (inferCoreTypeOf tenv sc tm')

  , testCase "literalType agrees with Clash.Core.HasType" $
      forM_ literals $ \l ->
        assertScopedTyAlphaEq (C.coreTypeOf l) (literalType l :: Type 'VoidS)

  , testCase "primInfoType of a multi-result primitive" $
      -- forall a. a -> a -> Int -> (a, Int)
      assertScopedTyAlphaEq
        (C.ForAllTy aTv (C.mkFunTy a (C.mkFunTy a (C.mkFunTy intTy (tupleTy [a, intTy])))))
        (primInfoType (mkPrimInfoFromClash mrPrim) :: Type 'VoidS)

  , testCase "piResultTys agrees with Clash.Core.HasType" $
      forM_ piCases $ \(ty, args) ->
        withFreeTyVarsOf (ty : args) $ \env ->
          assertScopedTyAlphaEq (C.piResultTys tcm ty args)
            (piResultTys tenv (convScope env) (fromClashType env ty)
              (map (fromClashType env) args))

    -- Instantiating @forall a b. a -> b@ with @c@, where @c@ has the same
    -- name as the name conversion gives @b@, must rename @b@
  , testCase "piResultTys renames binders that would capture" $
      withFreeTyVarsOf [c] $ \env ->
        assertScopedTyAlphaEq (C.ForAllTy bTv (C.mkFunTy c b))
          (piResultTys tenv (convScope env)
            (fromClashType env (C.ForAllTy aTv (C.ForAllTy bTv (C.mkFunTy a b))))
            [fromClashType env c])

  , testCase "applyTypeToArgs" $
      withFreeVarsOf [x] $ \env ->
        let sc = convScope env
            ty = fromClashTypeClosed sc (C.ForAllTy aTv (C.mkFunTy a (C.mkFunTy a a)))
        in assertScopedTyAlphaEq (C.mkFunTy intTy intTy)
             (applyTypeToArgs (fromClashTerm env x) tenv sc ty
               [Right (fromClashTypeClosed sc intTy), Left (fromClashTerm env x)])

  , testCase "inferCoreKindOf agrees with Clash.Core.HasType" $
      forM_ kindCases $ \ty -> withTypeScopes ty $ \sc ty' ->
        assertScopedTyAlphaEq (C.inferCoreKindOf tcm ty) (inferCoreKindOf tenv sc ty')

    -- With the @debug@ flag, 'piResultTys' checks that the types it drops the
    -- arrows of a function type with match the argument types of the
    -- function type. 'inferCoreKindOf' used it to apply the kind of a type
    -- constructor to its arguments, such as @Type -> Type@ of @Maybe@ to
    -- @Int@. The check then compared @Type@ against @Int@, so
    -- 'inferCoreKindOf' errored.
  , testCase "inferCoreKindOf of a type constructor applied to a type" $
      withTypeScopes (maybeTy intTy) $ \sc ty ->
        assertScopedTyAlphaEq C.liftedTypeKind (inferCoreKindOf tenv sc ty)
  , testCase "inferCoreKindOf of a type constructor applied to a type variable" $
      withTypeScopes (maybeTy a) $ \sc ty ->
        assertScopedTyAlphaEq C.liftedTypeKind (inferCoreKindOf tenv sc ty)
  ]

literals :: [C.Literal]
literals =
  [ C.IntegerLiteral 1, C.IntLiteral 1, C.WordLiteral 1, C.Int64Literal 1
  , C.Word64Literal 1, C.Int8Literal 1, C.Int16Literal 1, C.Int32Literal 1
  , C.Word8Literal 1, C.Word16Literal 1, C.Word32Literal 1, C.StringLiteral "s"
  , C.FloatLiteral 1, C.DoubleLiteral 1, C.CharLiteral 'c', C.NaturalLiteral 1 ]

-- | Function types and their arguments
piCases :: [(C.Type, [C.Type])]
piCases =
  [ (C.undefinedTy, [C.ForAllTy bTv (C.mkFunTy b b), intTy])
  , (C.ForAllTy aTv (C.ForAllTy bTv (C.mkFunTy a (C.mkFunTy b a))), [intTy, boolTy])
  , (C.ForAllTy aTv (C.ForAllTy bTv (C.mkFunTy a (C.mkFunTy b a))), [intTy])
  , (C.ForAllTy aTv (C.mkFunTy a a), [c])
  , (C.mkFunTy intTy boolTy, [intTy])
  , (C.ForAllTy aTv (signalTy a (C.ForAllTy bTv (C.mkFunTy b a))), [intTy, boolTy])
  , (C.ForAllTy aTv (pTy a), [intTy, boolTy])
  ]

-- | Types whose kinds don't need checking applications of type constructors
-- to their arguments, which 'C.piResultTys' rejects when debugging is on
kindCases :: [C.Type]
kindCases =
  [ intTy, C.mkFunTy intTy boolTy, C.LitTy (C.NumTy 1), C.LitTy (C.SymTy "s")
  , C.LitTy (C.CharTy 'c'), a, C.ForAllTy aTv a, C.ForAllTy aTv (C.mkFunTy a a)
  , C.AnnType [] intTy, nTyCon
  ]
 where
  -- The type constructor N itself
  nTyCon = case nTy intTy of
    C.AppTy n _ -> n
    _ -> error "kindCases: unexpected N"
