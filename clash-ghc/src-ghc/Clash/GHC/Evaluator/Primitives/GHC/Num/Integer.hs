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

module Clash.GHC.Evaluator.Primitives.GHC.Num.Integer
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
  [ ( $(textNameLit 'GHC.Num.Integer.integerLogBase#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..}
            | Just (a,b) <- integerLiterals args
            , Just c <- flogBase a b
            -> (reduce . Literal . WordLiteral . toInteger) c
          _ -> Nothing
    )


  , ( $(textNameLit 'GHC.Num.Integer.integerToInt#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..}
            | [i] <- integerLiterals' args
            -> reduce (integerToIntLiteral i)
          _ -> Nothing
    )

  , ( $(textNameLit 'GHC.Num.Integer.integerDecodeDouble#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} -- :: Double# -> (#Integer, Int##)
            | [Lit (DoubleLiteral i)] <- args
            -> let (_,tyView -> TyConApp tupTcNm tyArgs) = splitFunForallTy ty
                   (Just tupTc) = UniqMap.lookup tupTcNm tcm
                   [tupDc] = tyConDataCons tupTc
                   !(D# a)  = castWord64ToDouble i
                   !(# b, c #) = decodeDoubleInteger a
            in reduce $
               mkApps (Data tupDc) (map Right tyArgs ++
                        [ Left (integerToIntegerLiteral b)
                        , Left (integerToIntLiteral . toInteger $ I# c)])
          _ -> Nothing
    )

  , ( $(textNameLit 'GHC.Num.Integer.integerEncodeDouble#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} -- :: Integer -> Int# -> Double
            | [iV, Lit (IntLiteral j)] <- args
            , [i] <- integerLiterals' [iV]
            -> let !(I# k') = fromInteger j
                   r = encodeDoubleInteger i k'
            in  reduce . Literal . DoubleLiteral . castDoubleToWord64 $ D# r
          _ -> Nothing
    )

  , ( $(textNameLit 'GHC.Num.Integer.integerEncodeFloat#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..}
            | [iV, Lit (IntLiteral j)] <- args
            , [i] <- integerLiterals' [iV]
            -> let !(I# k') = fromInteger j
                   r = integerEncodeFloat# i k'
                in reduce . Literal . FloatLiteral . castFloatToWord32 $ F# r
          _ -> Nothing
    )

  , ( $(textNameLit 'GHC.Num.Integer.integerQuotRem#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} -- :: Integer -> Integer -> (#Integer, Integer#)
            | [i, j] <- integerLiterals' args
            -> let (_,tyView -> TyConApp tupTcNm tyArgs) = splitFunForallTy ty
                   (Just tupTc) = UniqMap.lookup tupTcNm tcm
                   [tupDc] = tyConDataCons tupTc
                   (q,r) = quotRem i j
            in reduce $
                 mkApps (Data tupDc) (map Right tyArgs ++
                        [ Left $ catchDivByZero (integerToIntegerLiteral q)
                        , Left $ catchDivByZero (integerToIntegerLiteral r)])
          _ -> Nothing
    )

  , ( $(textNameLit 'GHC.Num.Integer.integerAdd)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..}
            | Just (i,j) <- integerLiterals args
            -> reduce (integerToIntegerLiteral (i+j))
          _ -> Nothing
    )

  , ( $(textNameLit 'GHC.Num.Integer.integerSub)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..}
            | Just (i,j) <- integerLiterals args
            -> reduce (integerToIntegerLiteral (i-j))
          _ -> Nothing
    )

  , ( $(textNameLit 'GHC.Num.Integer.integerMul)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..}
            | Just (i,j) <- integerLiterals args
            -> reduce (integerToIntegerLiteral (i*j))
          _ -> Nothing
    )

  , ( $(textNameLit 'GHC.Num.Integer.integerNegate)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..}
            | [i] <- integerLiterals' args
            -> reduce (integerToIntegerLiteral (negate i))
          _ -> Nothing
    )

  , ( $(textNameLit 'GHC.Num.Integer.integerDiv)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..}
            | Just (i,j) <- integerLiterals args
            -> reduce $ catchDivByZero (integerToIntegerLiteral (i `div` j))
          _ -> Nothing
    )

  , ( $(textNameLit 'GHC.Num.Integer.integerMod)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..}
            | Just (i,j) <- integerLiterals args
            -> reduce $ catchDivByZero (integerToIntegerLiteral (i `mod` j))
          _ -> Nothing
    )

  , ( $(textNameLit 'GHC.Num.Integer.integerQuot)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..}
            | Just (i,j) <- integerLiterals args
            -> reduce $ catchDivByZero (integerToIntegerLiteral (i `quot` j))
          _ -> Nothing
    )

  , ( $(textNameLit 'GHC.Num.Integer.integerRem)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..}
            | Just (i,j) <- integerLiterals args
            -> reduce $ catchDivByZero (integerToIntegerLiteral (i `rem` j))
          _ -> Nothing
    )

  , ( $(textNameLit 'GHC.Num.Integer.integerDivMod#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..}
            | Just (i,j) <- integerLiterals args
            -> let (_,tyView -> TyConApp ubTupTcNm [liftedKi,_,intTy,_]) = splitFunForallTy ty
                   (Just ubTupTc) = UniqMap.lookup ubTupTcNm tcm
                   [ubTupDc] = tyConDataCons ubTupTc
                   (d,m) = divMod i j
               in  reduce $
                   mkApps (Data ubTupDc) [ Right liftedKi, Right liftedKi
                                         , Right intTy,    Right intTy
                                         , Left $ catchDivByZero (Literal (IntegerLiteral d))
                                         , Left $ catchDivByZero (Literal (IntegerLiteral m))
                                         ]
          _ -> Nothing
    )

  , ( $(textNameLit 'GHC.Num.Integer.integerGt)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..}
            | Just (i,j) <- integerLiterals args
            -> reduce (boolToBoolLiteral tcm ty (i > j))
          _ -> Nothing
    )

  , ( $(textNameLit 'GHC.Num.Integer.integerGe)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..}
            | Just (i,j) <- integerLiterals args
            -> reduce (boolToBoolLiteral tcm ty (i >= j))
          _ -> Nothing
    )

  , ( $(textNameLit 'GHC.Num.Integer.integerEq)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..}
            | Just (i,j) <- integerLiterals args
            -> reduce (boolToBoolLiteral tcm ty (i == j))
          _ -> Nothing
    )

  , ( $(textNameLit 'GHC.Num.Integer.integerNe)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..}
            | Just (i,j) <- integerLiterals args
            -> reduce (boolToBoolLiteral tcm ty (i /= j))
          _ -> Nothing
    )

  , ( $(textNameLit 'GHC.Num.Integer.integerLt)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..}
            | Just (i,j) <- integerLiterals args
            -> reduce (boolToBoolLiteral tcm ty (i < j))
          _ -> Nothing
    )

  , ( $(textNameLit 'GHC.Num.Integer.integerLe)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..}
            | Just (i,j) <- integerLiterals args
            -> reduce (boolToBoolLiteral tcm ty (i <= j))
          _ -> Nothing
    )

  , ( $(textNameLit 'GHC.Num.Integer.integerGt#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..}
            | Just (i,j) <- integerLiterals args
            -> reduce (boolToIntLiteral (i > j))
          _ -> Nothing
    )

  , ( $(textNameLit 'GHC.Num.Integer.integerGe#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..}
            | Just (i,j) <- integerLiterals args
            -> reduce (boolToIntLiteral (i >= j))
          _ -> Nothing
    )

  , ( $(textNameLit 'GHC.Num.Integer.integerEq#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..}
            | Just (i,j) <- integerLiterals args
            -> reduce (boolToIntLiteral (i == j))
          _ -> Nothing
    )

  , ( $(textNameLit 'GHC.Num.Integer.integerNe#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..}
            | Just (i,j) <- integerLiterals args
            -> reduce (boolToIntLiteral (i /= j))
          _ -> Nothing
    )

  , ( $(textNameLit 'GHC.Num.Integer.integerLt#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..}
            | Just (i,j) <- integerLiterals args
            -> reduce (boolToIntLiteral (i < j))
          _ -> Nothing
    )

  , ( $(textNameLit 'GHC.Num.Integer.integerLe#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..}
            | Just (i,j) <- integerLiterals args
            -> reduce (boolToIntLiteral (i <= j))
          _ -> Nothing
    )

  , ( $(textNameLit 'GHC.Num.Integer.integerCompare)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..}
            | [i, j] <- integerLiterals' args
            -> let -- Get the required result type (viewed as an applied type constructor name)
                   (_,tyView -> TyConApp tupTcNm []) = splitFunForallTy ty
                   -- Find the type constructor from the name
                   (Just tupTc) = UniqMap.lookup tupTcNm tcm
                   -- Get the data constructors of that type
                   -- The type is 'Ordering', so they are: 'LT', 'EQ', 'GT'
                   [ltDc, eqDc, gtDc] = tyConDataCons tupTc
                   -- Do the actual compile-time evaluation
                   ordVal = compareInteger i j
                in reduce $ case ordVal of
                    LT -> Data ltDc
                    EQ -> Data eqDc
                    GT -> Data gtDc
          _ -> Nothing
    )

  , ( $(textNameLit 'GHC.Num.Integer.integerShiftR#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..}
            | [iV, Lit (WordLiteral j)] <- args
            , [i] <- integerLiterals' [iV]
            -> reduce (integerToIntegerLiteral (i `shiftR` fromInteger j))
          _ -> Nothing
    )

  , ( $(textNameLit 'GHC.Num.Integer.integerShiftL#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..}
            | [iV, Lit (WordLiteral j)] <- args
            , [i] <- integerLiterals' [iV]
            -> reduce (integerToIntegerLiteral (i `shiftL` fromInteger j))
          _ -> Nothing
    )

  , ( $(textNameLit 'GHC.Num.Integer.integerFromWord#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..}
            | [Lit (WordLiteral w)] <- args
            -> reduce (Literal (IntegerLiteral w))
          _ -> Nothing
    )

  , ( $(textNameLit 'GHC.Num.Integer.integerToWord#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..}
            | [i] <- integerLiterals' args
            -> reduce (integerToWordLiteral i)
          _ -> Nothing
    )

  , ( $(textNameLit 'GHC.Num.Integer.integerTestBit#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} -- :: Integer -> Int# -> Int#
            | [Lit (IntegerLiteral i), Lit (WordLiteral j)] <- args
            -> reduce (boolToIntLiteral (testBit i (fromInteger j)))
          _ -> Nothing
    )
  , ( $(textNameLit 'GHC.Num.Integer.IS)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..}
            | [Lit (IntLiteral i)] <- args
            -> reduce (Literal (IntegerLiteral i))
          _ -> Nothing
    )
  , ( $(textNameLit 'GHC.Num.Integer.IP)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..}
            | [Lit (ByteArrayLiteral (BA.ByteArray ba))] <- args
            -> reduce (Literal (IntegerLiteral (IP ba)))
            | [Lit l] <- args
            -> error ("IP: " <> show l)
          _ -> Nothing
    )
  , ( $(textNameLit 'GHC.Num.Integer.IN)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..}
            | [Lit (ByteArrayLiteral (BA.ByteArray ba))] <- args
            -> reduce (Literal (IntegerLiteral (IN ba)))
            | [Lit l] <- args
            -> error ("IN: " <> show l)
          _ -> Nothing
    )

  , ( $(textNameLit 'GHC.Num.Integer.integerFromNatural)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..}
            | [i] <- naturalLiterals' args
            -> reduce (Literal (IntegerLiteral (toInteger i)))
          _ -> Nothing
    )

  , ( $(textNameLit 'GHC.Num.Integer.integerToNatural)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..}
            | [i] <- integerLiterals' args
            ->
             let nTy = snd (splitFunForallTy ty) in
             reduce (checkNaturalRange1 nTy i id)
          _ -> Nothing
    )

  , ( $(textNameLit 'GHC.Num.Integer.integerToNaturalClamp)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..}
            | [i] <- integerLiterals' args
            -> if i < 0 then
                 reduce (naturalToNaturalLiteral 0)
               else
                 reduce (naturalToNaturalLiteral (fromInteger i))
          _ -> Nothing
    )

  , ( $(textNameLit 'GHC.Num.Integer.integerToNaturalThrow)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..}
            | [i] <- integerLiterals' args
            -> let nTy = snd (splitFunForallTy ty) in
               reduce (checkNaturalRange1 nTy i id)
          _ -> Nothing
    )

  , ( $(textNameLit 'GHC.Num.Integer.integerToInt64#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..}
            | [i] <- integerLiterals' args
            -> reduce (integerToInt64Literal i)
          _ -> Nothing
    )

  , ( $(textNameLit 'GHC.Num.Integer.integerToWord64#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..}
            | [i] <- integerLiterals' args
            -> reduce (integerToWord64Literal i)
          _ -> Nothing
    )

  , ( $(textNameLit 'GHC.Num.Integer.integerFromWord64#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..}
            | [w] <- word64Literals' args
            -> reduce (Literal (IntegerLiteral w))
          _ -> Nothing
    )

  , ( $(textNameLit 'GHC.Num.Integer.integerSignum#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..}
            | [i] <- integerLiterals' args
            -> reduce (Literal (IntLiteral (signum i)))
          _ -> Nothing
    )

  , ( $(textNameLit 'GHC.Num.Integer.integerSignum)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..}
            | [i] <- integerLiterals' args
            -> reduce (Literal (IntegerLiteral (signumInteger i)))
          _ -> Nothing
    )

  , ( "GHC.Num.Integer.$wintegerSignum"
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..}
            | [i] <- integerLiterals' args
            -> reduce (Literal (IntLiteral (signum i)))
          _ -> Nothing
    )

  , ( $(textNameLit 'GHC.Num.Integer.integerAbs)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..}
            | [i] <- integerLiterals' args
            -> reduce (Literal (IntegerLiteral (absInteger i)))
          _ -> Nothing
    )

  , ( $(textNameLit 'GHC.Num.Integer.integerBit#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..}
            | [i] <- wordLiterals' args
            -> reduce (Literal (IntegerLiteral (bit (fromInteger i))))
          _ -> Nothing
    )

  , ( $(textNameLit 'GHC.Num.Integer.integerComplement)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..}
            | [i] <- integerLiterals' args
            -> reduce (Literal (IntegerLiteral (complementInteger i)))
          _ -> Nothing
    )

  , ( $(textNameLit 'GHC.Num.Integer.integerOr)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..}
            | [i, j] <- integerLiterals' args
            -> reduce (Literal (IntegerLiteral (orInteger i j)))
          _ -> Nothing
    )

  , ( $(textNameLit 'GHC.Num.Integer.integerXor)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..}
            | [i, j] <- integerLiterals' args
            -> reduce (Literal (IntegerLiteral (xorInteger i j)))
          _ -> Nothing
    )

  , ( $(textNameLit 'GHC.Num.Integer.integerAnd)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..}
            | [i, j] <- integerLiterals' args
            -> reduce (Literal (IntegerLiteral (andInteger i j)))
          _ -> Nothing
    )

  , ( "GHC.Num.Integer.$wintegerFromInt64#"
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..}
            | [i] <- int64Literals' args
            -> reduce . Literal $ IntLiteral i
          _ -> Nothing
    )
  ]
