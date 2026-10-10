{-|
  Copyright   :  (C) 2026, QBayLogic B.V.
  License     :  BSD2 (see the file LICENSE)
  Maintainer  :  QBayLogic B.V. <devops@qbaylogic.com>

  Tests for "Clash.Normalize.PrimitiveReductions"
-}

{-# LANGUAGE OverloadedStrings #-}

module Clash.Tests.Normalize.PrimitiveReductions (tests) where

import Data.Default (def)

import Test.Tasty
import Test.Tasty.HUnit

import Clash.Core.DataCon (DataCon (..))
import Clash.Core.Name (NameSort (..), mkUnsafeName, nameUniq)
import Clash.Core.Literal (Literal (..))
import Clash.Core.Term (Term (..))
import Clash.Core.TyCon (AlgTyConRhs (..), TyCon (..), TyConName)
import Clash.Core.Type (LitTy (..), Type (..), mkTyConApp)
import Clash.Core.TysPrim (liftedTypeKind)
import Clash.Core.Var (TyVar)
import qualified Clash.Data.UniqMap as UniqMap
import Clash.Driver.Types (ClashEnv (..))
import Clash.Normalize.PrimitiveReductions
  (mkVecNil, reduceReplace_int, reduceReverse)
import Clash.Normalize.Types (NormalizeSession)
import Clash.Rewrite.Types (RewriteEnv (..), TransformContext)

import Test.Clash.Rewrite
  ( assertAlphaEq, inScopeOf, intId, intTy, localId, mkDataCon
  , runSingleTransformation, tyVar )

-- | The name of @Vec@. The primitive reductions recognize vectors by this name.
vecTcNm :: TyConName
vecTcNm = mkUnsafeName User "Clash.Sized.Vector.Vec" 2000

-- | @Vec n a@
vecTy :: Type -> Type -> Type
vecTy n a = mkTyConApp vecTcNm [n, a]

-- | The universally quantified type variables of the constructors of @Vec@,
-- and the existentially quantified one of 'consDc'
vecN, vecA, vecM :: TyVar
vecN = tyVar User "n" 2001
vecA = tyVar User "a" 2002
vecM = tyVar User "m" 2003

-- | A stand-in for the type of the equality coercions of the constructors of
-- @Vec@. Nothing looks at it.
coTy :: Type -> Type -> Type
coTy n m = mkTyConApp (mkUnsafeName User "~#" 2004) [n, m]

-- | @Nil :: forall n a. (n ~ 0) => Vec n a@
nilDc :: DataCon
nilDc =
  mkDataCon "Nil" 2005 [vecN, vecA] []
    [coTy (VarTy vecN) (LitTy (NumTy 0))]
    (vecTy (VarTy vecN) (VarTy vecA))

-- | @Cons :: forall n a m. (n ~ m + 1) => a -> Vec m a -> Vec n a@
consDc :: DataCon
consDc =
  (mkDataCon "Cons" 2006 [vecN, vecA] [vecM]
    [ coTy (VarTy vecN) (VarTy vecM)
    , VarTy vecA
    , vecTy (VarTy vecM) (VarTy vecA) ]
    (vecTy (VarTy vecN) (VarTy vecA)))
  { dcTag = 2 }

-- | The default rewrite environment, extended with @Vec@
envVec :: RewriteEnv
envVec = def { _clashEnv = env { envTyConMap = tcm } }
 where
  env = _clashEnv def
  tcm = UniqMap.insert vecTcNm vecTc (envTyConMap env)
  vecTc = AlgTyCon
    { tyConUniq = nameUniq vecTcNm
    , tyConName = vecTcNm
    , tyConKind = liftedTypeKind
    , tyConArity = 2
    , algTcRhs = DataTyCon [nilDc, consDc]
    , isClassTc = False
    }

-- | Run a primitive reduction on an empty vector @v :: Vec 0 Int@, given @v@.
-- The reductions of primitives returning a vector usually don't see empty
-- vectors, as 'Clash.Normalize.Transformations.Reduce.reducePrim' replaces a
-- fully applied primitive returning @Vec 0 a@ by @Nil@. They do when the
-- primitive is only partially applied.
reduceEmptyVec :: (Term -> TransformContext -> NormalizeSession Term) -> IO Term
reduceEmptyVec reduction =
  runSingleTransformation envVec def (inScopeOf [v]) (\ctx _ -> reduction (Var v) ctx) (Var v)
 where
  v = localId User "v" 1 (vecTy (LitTy (NumTy 0)) intTy)

tests :: TestTree
tests =
  testGroup
    "Clash.Tests.Normalize.PrimitiveReductions"
    -- 'extractElems' only works on non-empty vectors
    [ testCase "reduceReverse on an empty vector" $
        reduceEmptyVec (reduceReverse 0 intTy)
          >>= assertAlphaEq (mkVecNil nilDc intTy)
    , testCase "reduceReplace_int on an empty vector" $
        reduceEmptyVec
          (\v -> reduceReplace_int 0 intTy (vecTy (LitTy (NumTy 0)) intTy)
                    (Literal (NaturalLiteral 0)) v (Var i) (Var a))
          >>= assertAlphaEq (mkVecNil nilDc intTy)
    ]
 where
  i = intId User "i" 2
  a = intId User "a" 3
