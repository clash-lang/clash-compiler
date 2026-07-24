{-|
  Copyright   :  (C) 2013-2016, University of Twente,
                     2016-2017, Myrtle Software Ltd,
                     2017-2022, Google Inc.,
                     2017-2026, QBayLogic B.V.
  License     :  BSD2 (see the file LICENSE)
  Maintainer  :  QBayLogic B.V. <devops@qbaylogic.com>
-}

{-# LANGUAGE CPP #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UnboxedTuples #-}

{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

#if MIN_VERSION_ghc(9,12,0)
-- We'll need to support deprecated primitives too
{-# OPTIONS_GHC -Wno-deprecations #-}
#endif

#include "MachDeps.h"

module Clash.GHC.Evaluator.Primitives.Clash.Sized.Internal.Unsigned
  ( primitives
  ) where

import           Control.DeepSeq            (force)
import           Control.Exception          (ArithException(..), ErrorCall, Exception, tryJust, evaluate)
import qualified Control.Lens               as Lens
import           Control.Monad.State.Strict (State, MonadState)
import qualified Control.Monad.State.Strict as State
import           Control.Monad.Trans.Except (runExcept)
import           Data.Bits
import qualified Data.ByteString.Internal as BS
import           Data.Char           (chr,ord)
import qualified Data.Either         as Either
import qualified Data.HashMap.Strict as HashMap
import           Data.Maybe (fromMaybe, mapMaybe)
import qualified Data.List           as List
import qualified Data.Primitive.ByteArray as BA
import           Data.Proxy          (Proxy)
import           Data.Reflection     (reifyNat)
import           Data.Text           (Text)
import qualified Data.Text           as Text
import           Data.Text.Extra     (showt)
import           GHC.Exts (IsList(..))
import           GHC.Float
import           GHC.Int
import           GHC.Integer
  (decodeDoubleInteger,encodeDoubleInteger,compareInteger,orInteger,andInteger,
   xorInteger,complementInteger,absInteger,signumInteger)
import           GHC.Num.BigNat      (bigNatEq#)
import           GHC.Num.Integer (Integer (..), integerEncodeFloat#)
import           GHC.Num.Natural     (naturalSubUnsafe)
import           GHC.Natural
import           GHC.ForeignPtr
import           GHC.Prim
import           GHC.Real            (Ratio (..))
import           GHC.TypeLits        (KnownNat)
import           GHC.Types           (IO (..))
import           GHC.Word
import           System.IO.Unsafe    (unsafeDupablePerformIO)
import           Data.Bifunctor      (first)
import qualified Data.Text.Array     as Text
import qualified Data.Text.Internal  as Text

import           GHC.Types.Basic     (Boxity (..))
import           GHC.Types.Name      (getSrcSpan, nameOccName, occNameString)
import           GHC.Builtin.Names   (trueDataConKey, falseDataConKey)
import qualified GHC.Core.TyCon      as TyCon
import           GHC.Builtin.Types   (tupleTyCon)

import           Clash.Class.BitPack (pack,unpack)
import           Clash.Core.DataCon  (DataCon (..))
import           Clash.Core.Evaluator.Types
import           Clash.Core.FreeVars (typeFreeVars)
import           Clash.Core.HasType  (piResultTys, applyTypeToArgs)
import           Clash.Core.Literal  (Literal (..))
import           Clash.Core.Name
  (Name (..), NameSort (..), mkUnsafeSystemName)
import           Clash.Core.Pretty   (showPpr)
import           Clash.Core.Subst    (extendTvSubst, mkSubst, substTy)
import           Clash.Core.Term
  (IsMultiPrim (..), Pat (..), PrimInfo (..), Term (..), WorkInfo (..), mkApps,
   PrimUnfolding(..), collectArgs)
import           Clash.Core.Type
  (Type (..), ConstTy (..), LitTy (..), TypeView (..), mkFunTy, mkTyConApp,
   normalizeType, splitFunForallTy, tyView)
import           Clash.Core.TyCon
  (TyConMap, TyConName, tyConDataCons)
import           Clash.Core.TysPrim
import           Clash.Core.Util
  (mkRTree,mkVec,tyNatSize,dataConInstArgTys,primCo, mkSelectorCase,undefinedPrims,
   undefinedXPrims)
import           Clash.Core.Var      (mkLocalId, mkTyVar)
import qualified Clash.Data.UniqMap as UniqMap
import           Clash.Debug
import           Clash.GHC.GHC2Core  (modNameM)
import           Clash.Unique        (fromGhcUnique)
import           Clash.Util
  (MonadUnique (..), clogBase, flogBase, curLoc, textNameLit)
import           Clash.Util.Supply   (Supply,freshId)
import           Clash.Normalize.PrimitiveReductions
  (typeNatMul, typeNatSub, typeNatAdd, vecLastPrim, vecInitPrim, vecHeadPrim,
   vecTailPrim, mkVecCons, mkVecNil)

import qualified Clash.Normalize.Primitives as NP
import Clash.Promoted.Nat.Unsafe (unsafeSNat)
import qualified Clash.Sized.Internal.BitVector as BitVector
import qualified Clash.Sized.Internal.Signed    as Signed
import qualified Clash.Sized.Internal.Unsigned  as Unsigned
import Clash.Sized.Internal.BitVector(BitVector(..), Bit(..))
import Clash.Sized.Internal.Signed   (Signed   (..))
import Clash.Sized.Internal.Unsigned (Unsigned (..))
import Clash.XException (isX)

import {-# SOURCE #-} Clash.GHC.Evaluator

import qualified Clash.Annotations.BitRepresentation.Deriving
import qualified Clash.Class.BitPack.Internal
import qualified Clash.Class.Exp
import qualified Clash.Promoted.Nat
import qualified Clash.Sized.Internal.BitVector
import qualified Clash.Sized.Internal.Index
import qualified Clash.Sized.Internal.Signed
import qualified Clash.Sized.Internal.Unsigned
import qualified Clash.Sized.RTree
import qualified Clash.Sized.Vector
import qualified GHC.Base
import qualified GHC.Classes
import qualified GHC.CString
import qualified GHC.TypeLits
import qualified GHC.TypeNats
import qualified GHC.Types
#if MIN_VERSION_ghc(9,12,0)
import qualified GHC.Magic
#endif
import qualified GHC.Num
import qualified GHC.Num.Integer

#if MIN_VERSION_ghc_prim(0,12,0)
import qualified GHC.PrimopWrappers
#endif

import {-# SOURCE #-} Clash.GHC.Evaluator.Primitive
import Clash.GHC.Evaluator.Primitive.Util

primitives :: [(Text, PrimStep)]
primitives =
-- SaturatingNum
-- No need to manually evaluate Clash.Sized.Internal.Signed.minBoundSym#
-- It is just implemented in terms of other primitives.


-----------
-- Unsigned
-----------
  [ ( $(textNameLit 'Clash.Sized.Internal.Unsigned.size#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..}
            | Just (_, kn) <- extractKnownNat tcm tys
            -> let (_,ty') = splitFunForallTy ty
                   (TyConApp intTcNm _) = tyView ty'
                   (Just intTc) = UniqMap.lookup intTcNm tcm
                   [intCon] = tyConDataCons intTc
               in  reduce (mkApps (Data intCon) [Left (Literal (IntLiteral kn))])
          _ -> Nothing
    )

-- BitPack
  , ( $(textNameLit 'Clash.Sized.Internal.Unsigned.pack#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..}
            | Just (nTy, kn) <- extractKnownNat tcm tys
            , [i] <- unsignedLiterals' args
            -> reduce (mkBitVectorLit ty nTy kn 0 i)
          _ -> Nothing
    )
  , ( $(textNameLit 'Clash.Sized.Internal.Unsigned.unpack#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..}
            | Just (nTy, kn) <- extractKnownNat tcm tys
            , [i] <- bitVectorLiterals' args
            -> let val = reifyNat kn (op (toBV i))
            in reduce (mkUnsignedLit ty nTy kn val)
            where
              op :: KnownNat n => BitVector n -> Proxy n -> Integer
              op u _ = toInteger (Unsigned.unpack# u)
          _ -> Nothing
    )

-- Eq
  , ( $(textNameLit 'Clash.Sized.Internal.Unsigned.eq#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} | Just (i,j) <- unsignedLiterals args
            -> reduce (boolToBoolLiteral tcm ty (i == j))
          _ -> Nothing
    )
  , ( $(textNameLit 'Clash.Sized.Internal.Unsigned.neq#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} | Just (i,j) <- unsignedLiterals args
            -> reduce (boolToBoolLiteral tcm ty (i /= j))
          _ -> Nothing
    )

-- Ord
  , ( $(textNameLit 'Clash.Sized.Internal.Unsigned.lt#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} | Just (i,j) <- unsignedLiterals args
            -> reduce (boolToBoolLiteral tcm ty (i <  j))
          _ -> Nothing
    )
  , ( $(textNameLit 'Clash.Sized.Internal.Unsigned.ge#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} | Just (i,j) <- unsignedLiterals args
            -> reduce (boolToBoolLiteral tcm ty (i >= j))
          _ -> Nothing
    )
  , ( $(textNameLit 'Clash.Sized.Internal.Unsigned.gt#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} | Just (i,j) <- unsignedLiterals args
            -> reduce (boolToBoolLiteral tcm ty (i >  j))
          _ -> Nothing
    )
  , ( $(textNameLit 'Clash.Sized.Internal.Unsigned.le#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} | Just (i,j) <- unsignedLiterals args
            -> reduce (boolToBoolLiteral tcm ty (i <= j))
          _ -> Nothing
    )

-- Enum
  , ( $(textNameLit 'Clash.Sized.Internal.Unsigned.toEnum#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..}
            | [i] <- intCLiterals' args
            , Just (litTy, mb) <- extractKnownNat tcm tys
            -> reduce (mkUnsignedLit ty litTy mb i)
          _ -> Nothing
    )

  , ( $(textNameLit 'Clash.Sized.Internal.Unsigned.fromEnum#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..}
            | [i] <- unsignedLiterals' args
            -> let resTy = getResultTy tcm ty tys
                in reduce (mkIntCLit tcm IntLiteral i resTy)
          _ -> Nothing
    )

-- Bounded
  , ( $(textNameLit 'Clash.Sized.Internal.Unsigned.minBound#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..}
            | Just (nTy,len) <- extractKnownNat tcm tys
            -> reduce (mkUnsignedLit ty nTy len 0)
          _ -> Nothing
    )
  , ( $(textNameLit 'Clash.Sized.Internal.Unsigned.maxBound#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..}
            | Just (litTy,mb) <- extractKnownNat tcm tys
            -> let maxB = (2 ^ mb) - 1
               in  reduce (mkUnsignedLit ty litTy mb maxB)
          _ -> Nothing
    )

-- Num
  , ( $(textNameLit '(Clash.Sized.Internal.Unsigned.+#))
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..}
            | Just (_, kn) <- extractKnownNat tcm tys
            , Just val <- reifyNat kn (liftUnsigned2 (Unsigned.+#) ty tcm tys args)
            -> reduce val
          _ -> Nothing
    )
  , ( $(textNameLit '(Clash.Sized.Internal.Unsigned.-#))
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..}
            | Just (_, kn) <- extractKnownNat tcm tys
            , Just val <- reifyNat kn (liftUnsigned2 (Unsigned.-#) ty tcm tys args)
            -> reduce val
          _ -> Nothing
    )
  , ( $(textNameLit '(Clash.Sized.Internal.Unsigned.*#))
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..}
            | Just (_, kn) <- extractKnownNat tcm tys
            , Just val <- reifyNat kn (liftUnsigned2 (Unsigned.*#) ty tcm tys args)
            -> reduce val
          _ -> Nothing
    )
  , ( $(textNameLit 'Clash.Sized.Internal.Unsigned.negate#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..}
            | Just (nTy, kn) <- extractKnownNat tcm tys
            , [i] <- unsignedLiterals' args
            -> let val = reifyNat kn (op (fromInteger i))
            in reduce (mkUnsignedLit ty nTy kn val)
            where
              op :: KnownNat n => Unsigned n -> Proxy n -> Integer
              op u _ = toInteger (Unsigned.negate# u)
          _ -> Nothing
    )

-- ExtendingNum
  , ( $(textNameLit 'Clash.Sized.Internal.Unsigned.plus#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} -- :: Unsigned m -> Unsigned n -> Unsigned (Max m n + 1)
            | Just (i,j) <- unsignedLiterals args
            -> let ty' = piResultTys tcm ty tys
                   (_,resTy) = splitFunForallTy ty'
                   (TyConApp _ [resSizeTy]) = tyView resTy
                   Right resSizeInt = runExcept (tyNatSize tcm resSizeTy)
               in  reduce (mkUnsignedLit resTy resSizeTy resSizeInt (i+j))
          _ -> Nothing
    )

  , ( $(textNameLit 'Clash.Sized.Internal.Unsigned.minus#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..}
            | [i,j] <- unsignedLiterals' args
            -> let ty' = piResultTys tcm ty tys
                   (_,resTy) = splitFunForallTy ty'
                   (TyConApp _ [resSizeTy]) = tyView resTy
                   Right resSizeInt = runExcept (tyNatSize tcm resSizeTy)
                   val = reifyNat resSizeInt (runSizedF (Unsigned.-#) i j)
              in   reduce (mkUnsignedLit resTy resSizeTy resSizeInt val)
          _ -> Nothing
    )

  , ( $(textNameLit 'Clash.Sized.Internal.Unsigned.times#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..}
            | Just (i,j) <- unsignedLiterals args
            -> let ty' = piResultTys tcm ty tys
                   (_,resTy) = splitFunForallTy ty'
                   (TyConApp _ [resSizeTy]) = tyView resTy
                   Right resSizeInt = runExcept (tyNatSize tcm resSizeTy)
               in  reduce (mkUnsignedLit resTy resSizeTy resSizeInt (i*j))
          _ -> Nothing
    )

-- Integral
  , ( $(textNameLit 'Clash.Sized.Internal.Unsigned.quot#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..}
            | Just (_, kn) <- extractKnownNat tcm tys
            , Just val <- reifyNat kn (liftUnsigned2 (Unsigned.quot#) ty tcm tys args)
            -> reduce $ catchDivByZero val
          _ -> Nothing
    )
  , ( $(textNameLit 'Clash.Sized.Internal.Unsigned.rem#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..}
            | Just (_, kn) <- extractKnownNat tcm tys
            , Just val <- reifyNat kn (liftUnsigned2 (Unsigned.rem#) ty tcm tys args)
            -> reduce $ catchDivByZero val
          _ -> Nothing
    )
  , ( $(textNameLit 'Clash.Sized.Internal.Unsigned.toInteger#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..}
            | [PrimVal p _ [_, Lit (IntegerLiteral i)]] <- args
            , primName p == showt 'Clash.Sized.Internal.Unsigned.fromInteger#
            -> reduce (integerToIntegerLiteral i)
          _ -> Nothing
    )

-- Bits
  , ( $(textNameLit 'Clash.Sized.Internal.Unsigned.and#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..}
            | Just (i,j) <- unsignedLiterals args
            , Just (nTy, kn) <- extractKnownNat tcm tys
            -> reduce (mkUnsignedLit ty nTy kn (i .&. j))
          _ -> Nothing
    )
  , ( $(textNameLit 'Clash.Sized.Internal.Unsigned.or#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..}
            | Just (i,j) <- unsignedLiterals args
            , Just (nTy, kn) <- extractKnownNat tcm tys
            -> reduce (mkUnsignedLit ty nTy kn (i .|. j))
          _ -> Nothing
    )
  , ( $(textNameLit 'Clash.Sized.Internal.Unsigned.xor#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..}
            | Just (i,j) <- unsignedLiterals args
            , Just (nTy, kn) <- extractKnownNat tcm tys
            -> reduce (mkUnsignedLit ty nTy kn (i `xor` j))
          _ -> Nothing
    )

  , ( $(textNameLit 'Clash.Sized.Internal.Unsigned.complement#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..}
            | [i] <- unsignedLiterals' args
            , Just (nTy, kn) <- extractKnownNat tcm tys
            -> let val = reifyNat kn (op (fromInteger i))
            in reduce (mkUnsignedLit ty nTy kn val)
            where
              op :: KnownNat n => Unsigned n -> Proxy n -> Integer
              op u _ = toInteger (Unsigned.complement# u)
          _ -> Nothing
    )

  , ( $(textNameLit 'Clash.Sized.Internal.Unsigned.shiftL#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} -- :: forall n. KnownNat n => Unsigned n -> Int -> Unsigned n
            | Just (nTy,kn,i,j) <- unsignedLitIntLit tcm tys args
              -> let val = reifyNat kn (op (fromInteger i) (fromInteger j))
              in reduce (mkUnsignedLit ty nTy kn val)
              where
                op :: KnownNat n => Unsigned n -> Int -> Proxy n -> Integer
                op u i _ = toInteger (Unsigned.shiftL# u i)
          _ -> Nothing
    )
  , ( $(textNameLit 'Clash.Sized.Internal.Unsigned.shiftR#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} -- :: forall n. KnownNat n => Unsigned n -> Int -> Unsigned n
            | Just (nTy,kn,i,j) <- unsignedLitIntLit tcm tys args
              -> let val = reifyNat kn (op (fromInteger i) (fromInteger j))
              in reduce (mkUnsignedLit ty nTy kn val)
              where
                op :: KnownNat n => Unsigned n -> Int -> Proxy n -> Integer
                op u i _ = toInteger (Unsigned.shiftR# u i)
          _ -> Nothing
    )
  , ( $(textNameLit 'Clash.Sized.Internal.Unsigned.rotateL#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} -- :: forall n. KnownNat n => Unsigned n -> Int -> Unsigned n
            | Just (nTy,kn,i,j) <- unsignedLitIntLit tcm tys args
              -> let val = reifyNat kn (op (fromInteger i) (fromInteger j))
              in reduce (mkUnsignedLit ty nTy kn val)
              where
                op :: KnownNat n => Unsigned n -> Int -> Proxy n -> Integer
                op u i _ = toInteger (Unsigned.rotateL# u i)
          _ -> Nothing
    )
  , ( $(textNameLit 'Clash.Sized.Internal.Unsigned.rotateR#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} -- :: forall n. KnownNat n => Unsigned n -> Int -> Unsigned n
            | Just (nTy,kn,i,j) <- unsignedLitIntLit tcm tys args
              -> let val = reifyNat kn (op (fromInteger i) (fromInteger j))
              in reduce (mkUnsignedLit ty nTy kn val)
              where
                op :: KnownNat n => Unsigned n -> Int -> Proxy n -> Integer
                op u i _ = toInteger (Unsigned.rotateR# u i)
          _ -> Nothing
    )

-- Resize
  , ( $(textNameLit 'Clash.Sized.Internal.Unsigned.resize#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} -- forall n m . KnownNat m => Unsigned n -> Unsigned m
            | _ : mTy : _ <- tys
            , Right km <- runExcept (tyNatSize tcm mTy)
            , [i] <- unsignedLiterals' args
            -> let bitsKeep = (bit (fromInteger km)) - 1
                   val = i .&. bitsKeep
            in reduce (mkUnsignedLit ty mTy km val)
          _ -> Nothing
    )

-- Conversions
  , ( $(textNameLit 'Clash.Sized.Internal.Unsigned.unsignedToWord)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..}
            | isSubj
            , [a] <- unsignedLiterals' args
            -> let b = Unsigned.unsignedToWord (U (fromInteger a))
                   (_,tyView -> TyConApp wordTcNm []) = splitFunForallTy ty
                   (Just wordTc) = UniqMap.lookup wordTcNm tcm
                   [wordDc] = tyConDataCons wordTc
               in  reduce (mkApps (Data wordDc) [Left (Literal (WordLiteral (toInteger b)))])
          _ -> Nothing
    )

  , ( $(textNameLit 'Clash.Sized.Internal.Unsigned.unsigned8toWord8)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..}
            | isSubj
            , [a] <- unsignedLiterals' args
            -> let b = Unsigned.unsigned8toWord8 (U (fromInteger a))
                   (_,tyView -> TyConApp wordTcNm []) = splitFunForallTy ty
                   (Just wordTc) = UniqMap.lookup wordTcNm tcm
                   [wordDc] = tyConDataCons wordTc
               in  reduce (mkApps (Data wordDc) [Left (Literal (WordLiteral (toInteger b)))])
          _ -> Nothing
    )

  , ( $(textNameLit 'Clash.Sized.Internal.Unsigned.unsigned16toWord16)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..}
            | isSubj
            , [a] <- unsignedLiterals' args
            -> let b = Unsigned.unsigned16toWord16 (U (fromInteger a))
                   (_,tyView -> TyConApp wordTcNm []) = splitFunForallTy ty
                   (Just wordTc) = UniqMap.lookup wordTcNm tcm
                   [wordDc] = tyConDataCons wordTc
               in  reduce (mkApps (Data wordDc) [Left (Literal (WordLiteral (toInteger b)))])
          _ -> Nothing
    )

  , ( $(textNameLit 'Clash.Sized.Internal.Unsigned.unsigned32toWord32)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..}
            | isSubj
            , [a] <- unsignedLiterals' args
            -> let b = Unsigned.unsigned32toWord32 (U (fromInteger a))
                   (_,tyView -> TyConApp wordTcNm []) = splitFunForallTy ty
                   (Just wordTc) = UniqMap.lookup wordTcNm tcm
                   [wordDc] = tyConDataCons wordTc
               in  reduce (mkApps (Data wordDc) [Left (Literal (WordLiteral (toInteger b)))])
          _ -> Nothing
    )
  ]
