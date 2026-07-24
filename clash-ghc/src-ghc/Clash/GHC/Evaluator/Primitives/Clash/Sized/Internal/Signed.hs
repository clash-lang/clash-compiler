{-|
  Copyright   :  (C) 2013-2016, University of Twente,
                     2016-2017, Myrtle Software Ltd,
                     2017-2022, Google Inc.,
                     2017-2026, QBayLogic B.V.
  License     :  BSD2 (see the file LICENSE)
  Maintainer  :  QBayLogic B.V. <devops@qbaylogic.com>
-}

{-# LANGUAGE MagicHash #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UnboxedTuples #-}

{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Clash.GHC.Evaluator.Primitives.Clash.Sized.Internal.Signed
  ( primitives
  ) where

import           Control.Monad.Trans.Except (runExcept)
import           Data.Bits
import           Data.Proxy          (Proxy)
import           Data.Reflection     (reifyNat)
import           Data.Text           (Text)
import           Data.Text.Extra     (showt)
import           GHC.TypeLits        (KnownNat)

import           Clash.Core.Evaluator.Types
import Clash.Core.HasType (piResultTys)
import           Clash.Core.Literal  (Literal (..))
import Clash.Core.Term (PrimInfo (..), Term (..), mkApps)
import Clash.Core.Type (TypeView (..), splitFunForallTy, tyView)
import Clash.Core.TyCon (tyConDataCons)
import Clash.Core.Util (tyNatSize)
import qualified Clash.Data.UniqMap as UniqMap
import Clash.Util (textNameLit)

import qualified Clash.Sized.Internal.Signed    as Signed
import Clash.Sized.Internal.BitVector (BitVector(..))
import Clash.Sized.Internal.Signed   (Signed   (..))

import qualified Clash.Sized.Internal.Signed

import Clash.GHC.Evaluator.Primitive.Util

primitives :: [(Text, PrimStep)]
primitives =
---------
-- Signed
---------
  [ ( $(textNameLit 'Clash.Sized.Internal.Signed.size#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..}
            | Just (_, kn) <- extractKnownNat tcm tys
            -> let (_,tyView -> TyConApp intTcNm _) = splitFunForallTy ty
                   (Just intTc) = UniqMap.lookup intTcNm tcm
                   [intCon] = tyConDataCons intTc
               in  reduce (mkApps (Data intCon) [Left (Literal (IntLiteral kn))])
          _ -> Nothing
    )

-- BitPack
  , ( $(textNameLit 'Clash.Sized.Internal.Signed.pack#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..}
            | Just (nTy, kn) <- extractKnownNat tcm tys
            , [i] <- signedLiterals' args
            -> let val = reifyNat kn (op (fromInteger i))
               in reduce (mkBitVectorLit ty nTy kn 0 val)
            where
                op :: KnownNat n => Signed n -> Proxy n -> Integer
                op s _ = toInteger (Signed.pack# s)
          _ -> Nothing
    )
  , ( $(textNameLit 'Clash.Sized.Internal.Signed.unpack#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..}
            | Just (nTy, kn) <- extractKnownNat tcm tys
            , [(0,i)] <- bitVectorLiterals' args
            -> let val = reifyNat kn (op (fromInteger i))
               in reduce (mkSignedLit ty nTy kn val)
            where
                op :: KnownNat n => BitVector n -> Proxy n -> Integer
                op s _ = toInteger (Signed.unpack# s)
          _ -> Nothing
    )

-- Eq
  , ( $(textNameLit 'Clash.Sized.Internal.Signed.eq#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} | Just (i,j) <- signedLiterals args
            -> reduce (boolToBoolLiteral tcm ty (i == j))
          _ -> Nothing
    )
  , ( $(textNameLit 'Clash.Sized.Internal.Signed.neq#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} | Just (i,j) <- signedLiterals args
            -> reduce (boolToBoolLiteral tcm ty (i /= j))
          _ -> Nothing
    )

-- Ord
  , ( $(textNameLit 'Clash.Sized.Internal.Signed.lt#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} | Just (i,j) <- signedLiterals args
            -> reduce (boolToBoolLiteral tcm ty (i <  j))
          _ -> Nothing
    )
  , ( $(textNameLit 'Clash.Sized.Internal.Signed.ge#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} | Just (i,j) <- signedLiterals args
            -> reduce (boolToBoolLiteral tcm ty (i >= j))
          _ -> Nothing
    )
  , ( $(textNameLit 'Clash.Sized.Internal.Signed.gt#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} | Just (i,j) <- signedLiterals args
            -> reduce (boolToBoolLiteral tcm ty (i >  j))
          _ -> Nothing
    )
  , ( $(textNameLit 'Clash.Sized.Internal.Signed.le#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} | Just (i,j) <- signedLiterals args
            -> reduce (boolToBoolLiteral tcm ty (i <= j))
          _ -> Nothing
    )

-- Enum
  , ( $(textNameLit 'Clash.Sized.Internal.Signed.toEnum#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..}
            | [i] <- intCLiterals' args
            , Just (litTy, mb) <- extractKnownNat tcm tys
            -> reduce (mkSignedLit ty litTy mb i)
          _ -> Nothing
    )

  , ( $(textNameLit 'Clash.Sized.Internal.Signed.fromEnum#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..}
            | [i] <- signedLiterals' args
            -> let resTy = getResultTy tcm ty tys
                in reduce (mkIntCLit tcm IntLiteral i resTy)
          _ -> Nothing
    )

-- Bounded
  , ( $(textNameLit 'Clash.Sized.Internal.Signed.minBound#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..}
            | Just (litTy,mb) <- extractKnownNat tcm tys
            -> let minB = negate (2 ^ (mb - 1))
               in  reduce (mkSignedLit ty litTy mb minB)
          _ -> Nothing
    )
  , ( $(textNameLit 'Clash.Sized.Internal.Signed.maxBound#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..}
            | Just (litTy,mb) <- extractKnownNat tcm tys
            -> let maxB = (2 ^ (mb - 1)) - 1
               in reduce (mkSignedLit ty litTy mb maxB)
          _ -> Nothing
    )

-- Num
  , ( $(textNameLit '(Clash.Sized.Internal.Signed.+#))
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..}
            | Just (_, kn) <- extractKnownNat tcm tys
            , Just val <- reifyNat kn (liftSigned2 (Signed.+#) ty tcm tys args)
            -> reduce (val)
          _ -> Nothing
    )
  , ( $(textNameLit '(Clash.Sized.Internal.Signed.-#))
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..}
            | Just (_, kn) <- extractKnownNat tcm tys
            , Just val <- reifyNat kn (liftSigned2 (Signed.-#) ty tcm tys args)
            -> reduce (val)
          _ -> Nothing
    )
  , ( $(textNameLit '(Clash.Sized.Internal.Signed.*#))
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..}
            | Just (_, kn) <- extractKnownNat tcm tys
            , Just val <- reifyNat kn (liftSigned2 (Signed.*#) ty tcm tys args)
            -> reduce (val)
          _ -> Nothing
    )
  , ( $(textNameLit 'Clash.Sized.Internal.Signed.negate#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..}
            | Just (nTy, kn) <- extractKnownNat tcm tys
            , [i] <- signedLiterals' args
            -> let val = reifyNat kn (op (fromInteger i))
            in reduce (mkSignedLit ty nTy kn val)
            where
              op :: KnownNat n => Signed n -> Proxy n -> Integer
              op s _ = toInteger (Signed.negate# s)
          _ -> Nothing
    )
  , ( $(textNameLit 'Clash.Sized.Internal.Signed.abs#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..}
            | Just (nTy, kn) <- extractKnownNat tcm tys
            , [i] <- signedLiterals' args
            -> let val = reifyNat kn (op (fromInteger i))
            in reduce (mkSignedLit ty nTy kn val)
            where
              op :: KnownNat n => Signed n -> Proxy n -> Integer
              op s _ = toInteger (Signed.abs# s)
          _ -> Nothing
    )

-- ExtendingNum
  , ( $(textNameLit 'Clash.Sized.Internal.Signed.plus#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..}
            | Just (i,j) <- signedLiterals args
            -> let ty' = piResultTys tcm ty tys
                   (_,resTy) = splitFunForallTy ty'
                   (TyConApp _ [resSizeTy]) = tyView resTy
                   Right resSizeInt = runExcept (tyNatSize tcm resSizeTy)
               in  reduce (mkSignedLit resTy resSizeTy resSizeInt (i+j))
          _ -> Nothing
    )

  , ( $(textNameLit 'Clash.Sized.Internal.Signed.minus#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..}
            | Just (i,j) <- signedLiterals args
            -> let ty' = piResultTys tcm ty tys
                   (_,resTy) = splitFunForallTy ty'
                   (TyConApp _ [resSizeTy]) = tyView resTy
                   Right resSizeInt = runExcept (tyNatSize tcm resSizeTy)
               in  reduce (mkSignedLit resTy resSizeTy resSizeInt (i-j))
          _ -> Nothing
    )

  , ( $(textNameLit 'Clash.Sized.Internal.Signed.times#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..}
            | Just (i,j) <- signedLiterals args
            -> let ty' = piResultTys tcm ty tys
                   (_,resTy) = splitFunForallTy ty'
                   (TyConApp _ [resSizeTy]) = tyView resTy
                   Right resSizeInt = runExcept (tyNatSize tcm resSizeTy)
               in  reduce (mkSignedLit resTy resSizeTy resSizeInt (i*j))
          _ -> Nothing
    )

-- Integral
  , ( $(textNameLit 'Clash.Sized.Internal.Signed.quot#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..}
            | Just (_, kn) <- extractKnownNat tcm tys
            , Just val <- reifyNat kn (liftSigned2 (Signed.quot#) ty tcm tys args)
            -> reduce $ catchDivByZero val
          _ -> Nothing
    )
  , ( $(textNameLit 'Clash.Sized.Internal.Signed.rem#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..}
            | Just (_, kn) <- extractKnownNat tcm tys
            , Just val <- reifyNat kn (liftSigned2 (Signed.rem#) ty tcm tys args)
            -> reduce $ catchDivByZero val
          _ -> Nothing
    )
  , ( $(textNameLit 'Clash.Sized.Internal.Signed.div#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..}
            | Just (_, kn) <- extractKnownNat tcm tys
            , Just val <- reifyNat kn (liftSigned2 (Signed.div#) ty tcm tys args)
            -> reduce $ catchDivByZero val
          _ -> Nothing
    )
  , ( $(textNameLit 'Clash.Sized.Internal.Signed.mod#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..}
            | Just (_, kn) <- extractKnownNat tcm tys
            , Just val <- reifyNat kn (liftSigned2 (Signed.mod#) ty tcm tys args)
            -> reduce $ catchDivByZero val
          _ -> Nothing
    )
  , ( $(textNameLit 'Clash.Sized.Internal.Signed.toInteger#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..}
            | [PrimVal p _ [_, Lit (IntegerLiteral i)]] <- args
            , primName p == showt 'Clash.Sized.Internal.Signed.fromInteger#
            -> reduce (integerToIntegerLiteral i)
          _ -> Nothing
    )

-- Bits
  , ( $(textNameLit 'Clash.Sized.Internal.Signed.and#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..}
            | [i,j] <- signedLiterals' args
            , Just (nTy, kn) <- extractKnownNat tcm tys
            -> reduce (mkSignedLit ty nTy kn (i .&. j))
          _ -> Nothing
    )
  , ( $(textNameLit 'Clash.Sized.Internal.Signed.or#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..}
            | [i,j] <- signedLiterals' args
            , Just (nTy, kn) <- extractKnownNat tcm tys
            -> reduce (mkSignedLit ty nTy kn (i .|. j))
          _ -> Nothing
    )
  , ( $(textNameLit 'Clash.Sized.Internal.Signed.xor#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..}
            | [i,j] <- signedLiterals' args
            , Just (nTy, kn) <- extractKnownNat tcm tys
            -> reduce (mkSignedLit ty nTy kn (i `xor` j))
          _ -> Nothing
    )

  , ( $(textNameLit 'Clash.Sized.Internal.Signed.complement#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..}
            | [i] <- signedLiterals' args
            , Just (nTy, kn) <- extractKnownNat tcm tys
            -> let val = reifyNat kn (op (fromInteger i))
            in reduce (mkSignedLit ty nTy kn val)
            where
              op :: KnownNat n => Signed n -> Proxy n -> Integer
              op u _ = toInteger (Signed.complement# u)
          _ -> Nothing
    )

  , ( $(textNameLit 'Clash.Sized.Internal.Signed.shiftL#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..}
            | Just (nTy,kn,i,j) <- signedLitIntLit tcm tys args
              -> let val = reifyNat kn (op (fromInteger i) (fromInteger j))
              in reduce (mkSignedLit ty nTy kn val)
              where
                op :: KnownNat n => Signed n -> Int -> Proxy n -> Integer
                op u i _ = toInteger (Signed.shiftL# u i)
          _ -> Nothing
    )
  , ( $(textNameLit 'Clash.Sized.Internal.Signed.shiftR#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..}
            | Just (nTy,kn,i,j) <- signedLitIntLit tcm tys args
              -> let val = reifyNat kn (op (fromInteger i) (fromInteger j))
              in reduce (mkSignedLit ty nTy kn val)
              where
                op :: KnownNat n => Signed n -> Int -> Proxy n -> Integer
                op u i _ = toInteger (Signed.shiftR# u i)
          _ -> Nothing
    )
  , ( $(textNameLit 'Clash.Sized.Internal.Signed.rotateL#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..}
            | Just (nTy,kn,i,j) <- signedLitIntLit tcm tys args
              -> let val = reifyNat kn (op (fromInteger i) (fromInteger j))
              in reduce (mkSignedLit ty nTy kn val)
              where
                op :: KnownNat n => Signed n -> Int -> Proxy n -> Integer
                op u i _ = toInteger (Signed.rotateL# u i)
          _ -> Nothing
    )
  , ( $(textNameLit 'Clash.Sized.Internal.Signed.rotateR#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..}
            | Just (nTy,kn,i,j) <- signedLitIntLit tcm tys args
              -> let val = reifyNat kn (op (fromInteger i) (fromInteger j))
              in reduce (mkSignedLit ty nTy kn val)
              where
                op :: KnownNat n => Signed n -> Int -> Proxy n -> Integer
                op u i _ = toInteger (Signed.rotateR# u i)
          _ -> Nothing
    )

-- Resize
  , ( $(textNameLit 'Clash.Sized.Internal.Signed.resize#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} -- forall m n. (KnownNat n, KnownNat m) => Signed n -> Signed m
            | mTy : nTy : _ <- tys
            , Right mInt <- runExcept (tyNatSize tcm mTy)
            , Right nInt <- runExcept (tyNatSize tcm nTy)
            , [i] <- signedLiterals' args
            -> let val | nInt <= mInt = extended
                       | otherwise    = truncated
                   extended  = i
                   mask      = 1 `shiftL` fromInteger (mInt - 1)
                   i'        = i `mod` mask
                   truncated = if testBit i (fromInteger nInt - 1)
                                  then (i' - mask)
                                  else i'
               in reduce (mkSignedLit ty mTy mInt val)
          _ -> Nothing
    )
  , ( $(textNameLit 'Clash.Sized.Internal.Signed.truncateB#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} -- KnownNat m => Signed (m + n) -> Signed m
            | Just (mTy, km) <- extractKnownNat tcm tys
            , [i] <- signedLiterals' args
            -> let bitsKeep = (bit (fromInteger km)) - 1
                   val = i .&. bitsKeep
            in reduce (mkSignedLit ty mTy km val)
          _ -> Nothing
    )
  ]
