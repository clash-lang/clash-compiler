{-|
  Copyright   :  (C) 2013-2016, University of Twente,
                     2016-2017, Myrtle Software Ltd,
                     2017-2022, Google Inc.,
                     2017-2026, QBayLogic B.V.
  License     :  BSD2 (see the file LICENSE)
  Maintainer  :  QBayLogic B.V. <devops@qbaylogic.com>
-}

{-# LANGUAGE MagicHash #-}
{-# LANGUAGE LambdaCase #-}
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
  [ primStepEntry $(textNameLit 'Clash.Sized.Internal.Signed.size#) $ \case
      PrimStepContext{..}
        | Just (_, kn) <- extractKnownNat tcm tys
        -> let (_,tyView -> TyConApp intTcNm _) = splitFunForallTy ty
               (Just intTc) = UniqMap.lookup intTcNm tcm
               [intCon] = tyConDataCons intTc
           in  reduce (mkApps (Data intCon) [Left (Literal (IntLiteral kn))])
      _ -> Nothing


-- BitPack
  , primStepEntry $(textNameLit 'Clash.Sized.Internal.Signed.pack#) $ \case
      PrimStepContext{..}
        | Just (nTy, kn) <- extractKnownNat tcm tys
        , [i] <- signedLiterals' args
        -> let val = reifyNat kn (op (fromInteger i))
           in reduce (mkBitVectorLit ty nTy kn 0 val)
        where
            op :: KnownNat n => Signed n -> Proxy n -> Integer
            op s _ = toInteger (Signed.pack# s)
      _ -> Nothing

  , primStepEntry $(textNameLit 'Clash.Sized.Internal.Signed.unpack#) $ \case
      PrimStepContext{..}
        | Just (nTy, kn) <- extractKnownNat tcm tys
        , [(0,i)] <- bitVectorLiterals' args
        -> let val = reifyNat kn (op (fromInteger i))
           in reduce (mkSignedLit ty nTy kn val)
        where
            op :: KnownNat n => BitVector n -> Proxy n -> Integer
            op s _ = toInteger (Signed.unpack# s)
      _ -> Nothing


-- Eq
  , primStepEntry $(textNameLit 'Clash.Sized.Internal.Signed.eq#) $ \case
      PrimStepContext{..} | Just (i,j) <- signedLiterals args
        -> reduce (boolToBoolLiteral tcm ty (i == j))
      _ -> Nothing

  , primStepEntry $(textNameLit 'Clash.Sized.Internal.Signed.neq#) $ \case
      PrimStepContext{..} | Just (i,j) <- signedLiterals args
        -> reduce (boolToBoolLiteral tcm ty (i /= j))
      _ -> Nothing


-- Ord
  , primStepEntry $(textNameLit 'Clash.Sized.Internal.Signed.lt#) $ \case
      PrimStepContext{..} | Just (i,j) <- signedLiterals args
        -> reduce (boolToBoolLiteral tcm ty (i <  j))
      _ -> Nothing

  , primStepEntry $(textNameLit 'Clash.Sized.Internal.Signed.ge#) $ \case
      PrimStepContext{..} | Just (i,j) <- signedLiterals args
        -> reduce (boolToBoolLiteral tcm ty (i >= j))
      _ -> Nothing

  , primStepEntry $(textNameLit 'Clash.Sized.Internal.Signed.gt#) $ \case
      PrimStepContext{..} | Just (i,j) <- signedLiterals args
        -> reduce (boolToBoolLiteral tcm ty (i >  j))
      _ -> Nothing

  , primStepEntry $(textNameLit 'Clash.Sized.Internal.Signed.le#) $ \case
      PrimStepContext{..} | Just (i,j) <- signedLiterals args
        -> reduce (boolToBoolLiteral tcm ty (i <= j))
      _ -> Nothing


-- Enum
  , primStepEntry $(textNameLit 'Clash.Sized.Internal.Signed.toEnum#) $ \case
      PrimStepContext{..}
        | [i] <- intCLiterals' args
        , Just (litTy, mb) <- extractKnownNat tcm tys
        -> reduce (mkSignedLit ty litTy mb i)
      _ -> Nothing

  , primStepEntry $(textNameLit 'Clash.Sized.Internal.Signed.fromEnum#) $ \case
      PrimStepContext{..}
        | [i] <- signedLiterals' args
        -> let resTy = getResultTy tcm ty tys
            in reduce (mkIntCLit tcm IntLiteral i resTy)
      _ -> Nothing


-- Bounded
  , primStepEntry $(textNameLit 'Clash.Sized.Internal.Signed.minBound#) $ \case
      PrimStepContext{..}
        | Just (litTy,mb) <- extractKnownNat tcm tys
        -> let minB = negate (2 ^ (mb - 1))
           in  reduce (mkSignedLit ty litTy mb minB)
      _ -> Nothing

  , primStepEntry $(textNameLit 'Clash.Sized.Internal.Signed.maxBound#) $ \case
      PrimStepContext{..}
        | Just (litTy,mb) <- extractKnownNat tcm tys
        -> let maxB = (2 ^ (mb - 1)) - 1
           in reduce (mkSignedLit ty litTy mb maxB)
      _ -> Nothing


-- Num
  , primStepEntry $(textNameLit '(Clash.Sized.Internal.Signed.+#)) $ \case
      PrimStepContext{..}
        | Just (_, kn) <- extractKnownNat tcm tys
        , Just val <- reifyNat kn (liftSigned2 (Signed.+#) ty tcm tys args)
        -> reduce (val)
      _ -> Nothing

  , primStepEntry $(textNameLit '(Clash.Sized.Internal.Signed.-#)) $ \case
      PrimStepContext{..}
        | Just (_, kn) <- extractKnownNat tcm tys
        , Just val <- reifyNat kn (liftSigned2 (Signed.-#) ty tcm tys args)
        -> reduce (val)
      _ -> Nothing

  , primStepEntry $(textNameLit '(Clash.Sized.Internal.Signed.*#)) $ \case
      PrimStepContext{..}
        | Just (_, kn) <- extractKnownNat tcm tys
        , Just val <- reifyNat kn (liftSigned2 (Signed.*#) ty tcm tys args)
        -> reduce (val)
      _ -> Nothing

  , primStepEntry $(textNameLit 'Clash.Sized.Internal.Signed.negate#) $ \case
      PrimStepContext{..}
        | Just (nTy, kn) <- extractKnownNat tcm tys
        , [i] <- signedLiterals' args
        -> let val = reifyNat kn (op (fromInteger i))
        in reduce (mkSignedLit ty nTy kn val)
        where
          op :: KnownNat n => Signed n -> Proxy n -> Integer
          op s _ = toInteger (Signed.negate# s)
      _ -> Nothing

  , primStepEntry $(textNameLit 'Clash.Sized.Internal.Signed.abs#) $ \case
      PrimStepContext{..}
        | Just (nTy, kn) <- extractKnownNat tcm tys
        , [i] <- signedLiterals' args
        -> let val = reifyNat kn (op (fromInteger i))
        in reduce (mkSignedLit ty nTy kn val)
        where
          op :: KnownNat n => Signed n -> Proxy n -> Integer
          op s _ = toInteger (Signed.abs# s)
      _ -> Nothing


-- ExtendingNum
  , primStepEntry $(textNameLit 'Clash.Sized.Internal.Signed.plus#) $ \case
      PrimStepContext{..}
        | Just (i,j) <- signedLiterals args
        -> let ty' = piResultTys tcm ty tys
               (_,resTy) = splitFunForallTy ty'
               (TyConApp _ [resSizeTy]) = tyView resTy
               Right resSizeInt = runExcept (tyNatSize tcm resSizeTy)
           in  reduce (mkSignedLit resTy resSizeTy resSizeInt (i+j))
      _ -> Nothing

  , primStepEntry $(textNameLit 'Clash.Sized.Internal.Signed.minus#) $ \case
      PrimStepContext{..}
        | Just (i,j) <- signedLiterals args
        -> let ty' = piResultTys tcm ty tys
               (_,resTy) = splitFunForallTy ty'
               (TyConApp _ [resSizeTy]) = tyView resTy
               Right resSizeInt = runExcept (tyNatSize tcm resSizeTy)
           in  reduce (mkSignedLit resTy resSizeTy resSizeInt (i-j))
      _ -> Nothing

  , primStepEntry $(textNameLit 'Clash.Sized.Internal.Signed.times#) $ \case
      PrimStepContext{..}
        | Just (i,j) <- signedLiterals args
        -> let ty' = piResultTys tcm ty tys
               (_,resTy) = splitFunForallTy ty'
               (TyConApp _ [resSizeTy]) = tyView resTy
               Right resSizeInt = runExcept (tyNatSize tcm resSizeTy)
           in  reduce (mkSignedLit resTy resSizeTy resSizeInt (i*j))
      _ -> Nothing


-- Integral
  , primStepEntry $(textNameLit 'Clash.Sized.Internal.Signed.quot#) $ \case
      PrimStepContext{..}
        | Just (_, kn) <- extractKnownNat tcm tys
        , Just val <- reifyNat kn (liftSigned2 (Signed.quot#) ty tcm tys args)
        -> reduce $ catchDivByZero val
      _ -> Nothing

  , primStepEntry $(textNameLit 'Clash.Sized.Internal.Signed.rem#) $ \case
      PrimStepContext{..}
        | Just (_, kn) <- extractKnownNat tcm tys
        , Just val <- reifyNat kn (liftSigned2 (Signed.rem#) ty tcm tys args)
        -> reduce $ catchDivByZero val
      _ -> Nothing

  , primStepEntry $(textNameLit 'Clash.Sized.Internal.Signed.div#) $ \case
      PrimStepContext{..}
        | Just (_, kn) <- extractKnownNat tcm tys
        , Just val <- reifyNat kn (liftSigned2 (Signed.div#) ty tcm tys args)
        -> reduce $ catchDivByZero val
      _ -> Nothing

  , primStepEntry $(textNameLit 'Clash.Sized.Internal.Signed.mod#) $ \case
      PrimStepContext{..}
        | Just (_, kn) <- extractKnownNat tcm tys
        , Just val <- reifyNat kn (liftSigned2 (Signed.mod#) ty tcm tys args)
        -> reduce $ catchDivByZero val
      _ -> Nothing

  , primStepEntry $(textNameLit 'Clash.Sized.Internal.Signed.toInteger#) $ \case
      PrimStepContext{..}
        | [PrimVal p _ [_, Lit (IntegerLiteral i)]] <- args
        , primName p == showt 'Clash.Sized.Internal.Signed.fromInteger#
        -> reduce (integerToIntegerLiteral i)
      _ -> Nothing


-- Bits
  , primStepEntry $(textNameLit 'Clash.Sized.Internal.Signed.and#) $ \case
      PrimStepContext{..}
        | [i,j] <- signedLiterals' args
        , Just (nTy, kn) <- extractKnownNat tcm tys
        -> reduce (mkSignedLit ty nTy kn (i .&. j))
      _ -> Nothing

  , primStepEntry $(textNameLit 'Clash.Sized.Internal.Signed.or#) $ \case
      PrimStepContext{..}
        | [i,j] <- signedLiterals' args
        , Just (nTy, kn) <- extractKnownNat tcm tys
        -> reduce (mkSignedLit ty nTy kn (i .|. j))
      _ -> Nothing

  , primStepEntry $(textNameLit 'Clash.Sized.Internal.Signed.xor#) $ \case
      PrimStepContext{..}
        | [i,j] <- signedLiterals' args
        , Just (nTy, kn) <- extractKnownNat tcm tys
        -> reduce (mkSignedLit ty nTy kn (i `xor` j))
      _ -> Nothing

  , primStepEntry $(textNameLit 'Clash.Sized.Internal.Signed.complement#) $ \case
      PrimStepContext{..}
        | [i] <- signedLiterals' args
        , Just (nTy, kn) <- extractKnownNat tcm tys
        -> let val = reifyNat kn (op (fromInteger i))
        in reduce (mkSignedLit ty nTy kn val)
        where
          op :: KnownNat n => Signed n -> Proxy n -> Integer
          op u _ = toInteger (Signed.complement# u)
      _ -> Nothing

  , primStepEntry $(textNameLit 'Clash.Sized.Internal.Signed.shiftL#) $ \case
      PrimStepContext{..}
        | Just (nTy,kn,i,j) <- signedLitIntLit tcm tys args
          -> let val = reifyNat kn (op (fromInteger i) (fromInteger j))
          in reduce (mkSignedLit ty nTy kn val)
          where
            op :: KnownNat n => Signed n -> Int -> Proxy n -> Integer
            op u i _ = toInteger (Signed.shiftL# u i)
      _ -> Nothing

  , primStepEntry $(textNameLit 'Clash.Sized.Internal.Signed.shiftR#) $ \case
      PrimStepContext{..}
        | Just (nTy,kn,i,j) <- signedLitIntLit tcm tys args
          -> let val = reifyNat kn (op (fromInteger i) (fromInteger j))
          in reduce (mkSignedLit ty nTy kn val)
          where
            op :: KnownNat n => Signed n -> Int -> Proxy n -> Integer
            op u i _ = toInteger (Signed.shiftR# u i)
      _ -> Nothing

  , primStepEntry $(textNameLit 'Clash.Sized.Internal.Signed.rotateL#) $ \case
      PrimStepContext{..}
        | Just (nTy,kn,i,j) <- signedLitIntLit tcm tys args
          -> let val = reifyNat kn (op (fromInteger i) (fromInteger j))
          in reduce (mkSignedLit ty nTy kn val)
          where
            op :: KnownNat n => Signed n -> Int -> Proxy n -> Integer
            op u i _ = toInteger (Signed.rotateL# u i)
      _ -> Nothing

  , primStepEntry $(textNameLit 'Clash.Sized.Internal.Signed.rotateR#) $ \case
      PrimStepContext{..}
        | Just (nTy,kn,i,j) <- signedLitIntLit tcm tys args
          -> let val = reifyNat kn (op (fromInteger i) (fromInteger j))
          in reduce (mkSignedLit ty nTy kn val)
          where
            op :: KnownNat n => Signed n -> Int -> Proxy n -> Integer
            op u i _ = toInteger (Signed.rotateR# u i)
      _ -> Nothing


-- Resize
  , primStepEntry $(textNameLit 'Clash.Sized.Internal.Signed.resize#) $ \case
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

  , primStepEntry $(textNameLit 'Clash.Sized.Internal.Signed.truncateB#) $ \case
      PrimStepContext{..} -- KnownNat m => Signed (m + n) -> Signed m
        | Just (mTy, km) <- extractKnownNat tcm tys
        , [i] <- signedLiterals' args
        -> let bitsKeep = (bit (fromInteger km)) - 1
               val = i .&. bitsKeep
        in reduce (mkSignedLit ty mTy km val)
      _ -> Nothing

  ]
