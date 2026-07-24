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

module Clash.GHC.Evaluator.Primitives.GHC.Num
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
  [ ( $(textNameLit 'GHC.Num.naturalLogBase#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..}
            | Just (a,b) <- naturalLiterals args
            , Just c <- flogBase a b
            -> (reduce . Literal . WordLiteral . toInteger) c
          _ -> Nothing
    )

  , ( $(textNameLit 'GHC.Num.NS)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..}
            | [Lit (WordLiteral w)] <- args
            -> reduce (Literal (NaturalLiteral w))
          _ -> Nothing
    )
  , ( $(textNameLit 'GHC.Num.NB)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..}
            | [Lit (ByteArrayLiteral (BA.ByteArray ba))] <- args
            -> reduce (Literal (NaturalLiteral (IP ba)))
            | [Lit l] <- args
            -> error ("NB: " <> show l)
          _ -> Nothing
    )

  , ( $(textNameLit 'GHC.Num.naturalAdd)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..}
            | Just (i,j) <- naturalLiterals args
            ->
             let nTy = snd (splitFunForallTy ty) in
             reduce (checkNaturalRange2 nTy i j (+))
          _ -> Nothing
    )

  , ( $(textNameLit 'GHC.Num.naturalMul)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..}
            | Just (i,j) <- naturalLiterals args
            ->
             let nTy = snd (splitFunForallTy ty) in
             reduce (checkNaturalRange2 nTy i j (*))
          _ -> Nothing
    )

  , ( $(textNameLit 'GHC.Num.naturalSubThrow)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..}
            | Just (i,j) <- naturalLiterals args
            ->
             let nTy = snd (splitFunForallTy ty) in
             reduce (checkNaturalRange nTy [i, j] (\[i', j'] ->
                        case minusNaturalMaybe i' j' of
                          Nothing -> checkNaturalRange1 nTy (-1) id
                          Just n -> naturalToNaturalLiteral n))
          _ -> Nothing
    )

  , ( $(textNameLit 'GHC.Num.naturalFromWord#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..}
            | [Lit (WordLiteral w)] <- args
            ->
             let nTy = snd (splitFunForallTy ty) in
             reduce (checkNaturalRange1 nTy w id)
          _ -> Nothing
    )

  , ( $(textNameLit 'GHC.Num.naturalToWord#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..}
            | [i] <- naturalLiterals' args
            -> reduce (integerToWordLiteral i)
          _ -> Nothing
    )

  , ( $(textNameLit 'GHC.Num.naturalQuot)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..}
            | Just (i,j) <- naturalLiterals args
            ->
             let nTy = snd (splitFunForallTy ty) in
             reduce (checkNaturalRange2 nTy i j quot)
          _ -> Nothing
    )

  , ( $(textNameLit 'GHC.Num.naturalRem)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..}
            | Just (i,j) <- naturalLiterals args
            ->
             let nTy = snd (splitFunForallTy ty) in
             reduce (checkNaturalRange2 nTy i j rem)
          _ -> Nothing
    )

  , ( $(textNameLit 'GHC.Num.naturalQuotRem#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} -- :: Natural -> Natural -> (#Natural, Natural#)
            | [i, j] <- naturalLiterals' args
            -> let (_,tyView -> TyConApp tupTcNm tyArgs) = splitFunForallTy ty
                   (Just tupTc) = UniqMap.lookup tupTcNm tcm
                   [tupDc] = tyConDataCons tupTc
                   (q,r) = quotRem (fromInteger i) (fromInteger j)
            in reduce $
                 mkApps (Data tupDc) (map Right tyArgs ++
                        [ Left $ catchDivByZero (naturalToNaturalLiteral q)
                        , Left $ catchDivByZero (naturalToNaturalLiteral r)])
          _ -> Nothing
    )

  , ( $(textNameLit 'GHC.Num.naturalGcd)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..}
            | Just (i,j) <- naturalLiterals args
            ->
             let nTy = snd (splitFunForallTy ty) in
             reduce (checkNaturalRange2 nTy i j gcd)
          _ -> Nothing
    )

  , ( $(textNameLit 'GHC.Num.naturalLcm)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..}
            | Just (i,j) <- naturalLiterals args
            ->
             let nTy = snd (splitFunForallTy ty) in
             reduce (checkNaturalRange2 nTy i j lcm)
          _ -> Nothing
    )

  , ( $(textNameLit 'GHC.Num.naturalGt#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..}
            | Just (i,j) <- naturalLiterals args
            -> reduce (boolToIntLiteral (i > j))
          _ -> Nothing
    )

  , ( $(textNameLit 'GHC.Num.naturalGe#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..}
            | Just (i,j) <- naturalLiterals args
            -> reduce (boolToIntLiteral (i >= j))
          _ -> Nothing
    )

  , ( $(textNameLit 'GHC.Num.naturalEq#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..}
            | Just (i,j) <- naturalLiterals args
            -> reduce (boolToIntLiteral (i == j))
          _ -> Nothing
    )

  , ( $(textNameLit 'GHC.Num.naturalNe#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..}
            | Just (i,j) <- naturalLiterals args
            -> reduce (boolToIntLiteral (i /= j))
          _ -> Nothing
    )

  , ( $(textNameLit 'GHC.Num.naturalLt#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..}
            | Just (i,j) <- naturalLiterals args
            -> reduce (boolToIntLiteral (i < j))
          _ -> Nothing
    )

  , ( $(textNameLit 'GHC.Num.naturalLe#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..}
            | Just (i,j) <- naturalLiterals args
            -> reduce (boolToIntLiteral (i <= j))
          _ -> Nothing
    )

  , ( $(textNameLit 'GHC.Num.naturalShiftL#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..}
            | [iV, Lit (WordLiteral j)] <- args
            , [i] <- naturalLiterals' [iV]
            -> reduce (naturalToNaturalLiteral (fromInteger (i `shiftL` fromInteger j)))
          _ -> Nothing
    )

  , ( $(textNameLit 'GHC.Num.naturalShiftR#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..}
            | [iV, Lit (WordLiteral j)] <- args
            , [i] <- naturalLiterals' [iV]
            -> reduce (naturalToNaturalLiteral (fromInteger (i `shiftR` fromInteger j)))
          _ -> Nothing
    )

  , ( $(textNameLit 'GHC.Num.naturalCompare)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..}
            | [i, j] <- naturalLiterals' args
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

  , ( $(textNameLit 'GHC.Num.naturalSignum)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..}
            | [i] <- naturalLiterals' args
            -> reduce (Literal (NaturalLiteral (signum i)))
          _ -> Nothing
    )
  ]
