{-|
  Copyright   :  (C) 2026, QBayLogic B.V.
  License     :  BSD2 (see the file LICENSE)
  Maintainer  :  QBayLogic B.V. <devops@qbaylogic.com>

  Helpers for the unit tests of the ported reductions of primitives
  ("Clash.Core.Scoped.Evaluator.Primitives"): evaluating calls with only the
  ported reductions, and building primitives, literals and values of the
  sized types as Clash Core.
-}

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Test.Clash.Scoped.Evaluator.Primitives
  ( -- * Evaluating
    eval
  , evalSubj
  , evalWith
  , allocatedBody
    -- * Primitives
  , prim
  , closedPrimTerm
    -- * Literals
  , intLit
  , natLit
  , integerLit
    -- * Types
  , binTy
  , knownNatTy
  , tvN
  , tvM
  , tvA
    -- * Sized types, with type constructors of their own
  , bitTcNm
  , bvTcNm
  , bitTy
  , bvTy
  , bit
  , bv
  , indexTcNm
  , indexTy
  , index
    -- * Boxed numbers
  , boxedIntTy
  , boxedInt
  , sizedTypeEnv
  ) where

import Data.Text (Text)

import Clash.Core.Literal (Literal (..))
import Clash.Core.Name (mkUnsafeSystemName)
import qualified Clash.Core.Term as C
import Clash.Core.TyCon (TyConName)
import qualified Clash.Core.Type as C
import Clash.Core.TysPrim (liftedTypeKind, typeNatKind)
import qualified Clash.Core.Var as C

import Clash.Core.Scoped.Convert (toClashTerm)
import Clash.Core.Scoped.Evaluator.Primitives.Util (bConPrim, bvConPrim, indexConPrim)
import Clash.Core.Scoped.Syntax (S (VoidS), Term)
import Clash.Core.Scoped.TypeEnv (TypeEnv)

import Test.Clash.Rewrite
  ( hIntTy, heapLookupEvaluator, iHashDc, indexTcNm, indexTy, knownNatTy, mkTyConName
  , numTy, tcApp )
import Test.Clash.Scoped (testTypeEnv)
import Test.Clash.Scoped.Evaluator.Differential (portedEnv, scopedWhnf)

-- * Evaluating

-- | Evaluate with the ported reductions, and nothing for the other
-- primitives
eval :: C.Term -> C.Term
eval = evalWith testTypeEnv False

-- | 'eval', as the subject of a case-expression
evalSubj :: C.Term -> C.Term
evalSubj = evalWith testTypeEnv True

-- | 'eval' with the given types, as the subject of a case-expression or not
evalWith :: TypeEnv -> Bool -> C.Term -> C.Term
evalWith te = scopedWhnf (portedEnv te heapLookupEvaluator) mempty

-- | The body of the bindings the evaluator allocated
allocatedBody :: C.Term -> C.Term
allocatedBody = \case
  C.Letrec _ e -> e
  e -> e

-- * Primitives

-- | A primitive with the given name and type
prim :: Text -> C.Type -> C.Term
prim nm ty = C.Prim (C.PrimInfo nm ty C.WorkNever C.SingleResult C.NoUnfolding)

-- | A closed scoped primitive, as Clash Core
closedPrimTerm :: Term 'VoidS -> C.Term
closedPrimTerm = toClashTerm

-- * Literals

intLit, natLit, integerLit :: Integer -> C.Term
intLit = C.Literal . IntLiteral
natLit = C.Literal . NaturalLiteral
integerLit = C.Literal . IntegerLiteral

-- * Types

-- | The type of a binary operation
binTy :: C.Type -> C.Type -> C.Type
binTy a r = C.mkFunTy a (C.mkFunTy a r)

tvN, tvM, tvA :: C.TyVar
tvN = C.mkTyVar typeNatKind (mkUnsafeSystemName "n" 9001)
tvM = C.mkTyVar typeNatKind (mkUnsafeSystemName "m" 9002)
tvA = C.mkTyVar liftedTypeKind (mkUnsafeSystemName "a" 9003)

-- * Sized types, with type constructors of their own

bitTcNm, bvTcNm :: TyConName
bitTcNm = mkTyConName "Clash.Sized.Internal.BitVector.Bit" 4000
bvTcNm = mkTyConName "Clash.Sized.Internal.BitVector.BitVector" 4001

bitTy :: C.Type
bitTy = tcApp bitTcNm []

bvTy :: C.Type -> C.Type
bvTy n = tcApp bvTcNm [n]

-- | A 'Clash.Sized.Internal.BitVector.Bit' with the given mask and value
bit :: Integer -> Integer -> C.Term
bit msk val =
  C.mkApps (closedPrimTerm (bConPrim bitTcNm))
    [Left (C.Literal (WordLiteral msk)), Left (integerLit val)]

-- | A 'Clash.Sized.Internal.BitVector.BitVector' with the given size, mask
-- and value
bv :: Integer -> Integer -> Integer -> C.Term
bv n msk val =
  C.mkApps (closedPrimTerm (bvConPrim bvTcNm))
    [Right (numTy n), Left (natLit n), Left (natLit msk), Left (integerLit val)]

-- | An 'Clash.Sized.Internal.Index.Index' with the given size and value
index :: Integer -> Integer -> C.Term
index n val =
  C.mkApps (closedPrimTerm (indexConPrim indexTcNm))
    [Right (numTy n), Left (natLit n), Left (integerLit val)]

-- * Boxed numbers

-- | GHC's boxed 'Int', 'hIntTy', unlike the @Int@ of 'intTy'
boxedIntTy :: C.Type
boxedIntTy = hIntTy

-- | An 'Int'
boxedInt :: Integer -> C.Term
boxedInt = C.App (C.Data iHashDc) . intLit

-- | The types of 'testTyConMap', which has 'boxedIntTy'
sizedTypeEnv :: TypeEnv
sizedTypeEnv = testTypeEnv
