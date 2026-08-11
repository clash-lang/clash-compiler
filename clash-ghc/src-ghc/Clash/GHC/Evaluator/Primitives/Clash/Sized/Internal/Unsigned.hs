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

module Clash.GHC.Evaluator.Primitives.Clash.Sized.Internal.Unsigned
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

import qualified Clash.Sized.Internal.Unsigned  as Unsigned
import Clash.Sized.Internal.BitVector (BitVector(..))
import Clash.Sized.Internal.Unsigned (Unsigned (..))

import qualified Clash.Sized.Internal.Unsigned

import Clash.GHC.Evaluator.Primitive.Util

primitives :: [(Text, PrimStep)]
primitives =
-- SaturatingNum
-- No need to manually evaluate Clash.Sized.Internal.Signed.minBoundSym#
-- It is just implemented in terms of other primitives.


-----------
-- Unsigned
-----------
  [ primStepEntry $(textNameLit 'Clash.Sized.Internal.Unsigned.size#) $ \case
      PrimStepContext{..}
        | Just (_, kn) <- extractKnownNat tcm tys
        -> let (_,ty') = splitFunForallTy ty
               (TyConApp intTcNm _) = tyView ty'
               (Just intTc) = UniqMap.lookup intTcNm tcm
               [intCon] = tyConDataCons intTc
           in  reduce (mkApps (Data intCon) [Left (Literal (IntLiteral kn))])
      _ -> Nothing


-- BitPack
  , primStepEntry $(textNameLit 'Clash.Sized.Internal.Unsigned.pack#) $ \case
      PrimStepContext{..}
        | Just (nTy, kn) <- extractKnownNat tcm tys
        , [i] <- unsignedLiterals' args
        -> reduce (mkBitVectorLit ty nTy kn 0 i)
      _ -> Nothing

  , primStepEntry $(textNameLit 'Clash.Sized.Internal.Unsigned.unpack#) $ \case
      PrimStepContext{..}
        | Just (nTy, kn) <- extractKnownNat tcm tys
        , [i] <- bitVectorLiterals' args
        -> let val = reifyNat kn (op (toBV i))
        in reduce (mkUnsignedLit ty nTy kn val)
        where
          op :: KnownNat n => BitVector n -> Proxy n -> Integer
          op u _ = toInteger (Unsigned.unpack# u)
      _ -> Nothing


-- Eq
  , primStepEntry $(textNameLit 'Clash.Sized.Internal.Unsigned.eq#) $ \case
      PrimStepContext{..} | Just (i,j) <- unsignedLiterals args
        -> reduce (boolToBoolLiteral tcm ty (i == j))
      _ -> Nothing

  , primStepEntry $(textNameLit 'Clash.Sized.Internal.Unsigned.neq#) $ \case
      PrimStepContext{..} | Just (i,j) <- unsignedLiterals args
        -> reduce (boolToBoolLiteral tcm ty (i /= j))
      _ -> Nothing


-- Ord
  , primStepEntry $(textNameLit 'Clash.Sized.Internal.Unsigned.lt#) $ \case
      PrimStepContext{..} | Just (i,j) <- unsignedLiterals args
        -> reduce (boolToBoolLiteral tcm ty (i <  j))
      _ -> Nothing

  , primStepEntry $(textNameLit 'Clash.Sized.Internal.Unsigned.ge#) $ \case
      PrimStepContext{..} | Just (i,j) <- unsignedLiterals args
        -> reduce (boolToBoolLiteral tcm ty (i >= j))
      _ -> Nothing

  , primStepEntry $(textNameLit 'Clash.Sized.Internal.Unsigned.gt#) $ \case
      PrimStepContext{..} | Just (i,j) <- unsignedLiterals args
        -> reduce (boolToBoolLiteral tcm ty (i >  j))
      _ -> Nothing

  , primStepEntry $(textNameLit 'Clash.Sized.Internal.Unsigned.le#) $ \case
      PrimStepContext{..} | Just (i,j) <- unsignedLiterals args
        -> reduce (boolToBoolLiteral tcm ty (i <= j))
      _ -> Nothing


-- Enum
  , primStepEntry $(textNameLit 'Clash.Sized.Internal.Unsigned.toEnum#) $ \case
      PrimStepContext{..}
        | [i] <- intCLiterals' args
        , Just (litTy, mb) <- extractKnownNat tcm tys
        -> reduce (mkUnsignedLit ty litTy mb i)
      _ -> Nothing

  , primStepEntry $(textNameLit 'Clash.Sized.Internal.Unsigned.fromEnum#) $ \case
      PrimStepContext{..}
        | [i] <- unsignedLiterals' args
        -> let resTy = getResultTy tcm ty tys
            in reduce (mkIntCLit tcm IntLiteral i resTy)
      _ -> Nothing


-- Bounded
  , primStepEntry $(textNameLit 'Clash.Sized.Internal.Unsigned.minBound#) $ \case
      PrimStepContext{..}
        | Just (nTy,len) <- extractKnownNat tcm tys
        -> reduce (mkUnsignedLit ty nTy len 0)
      _ -> Nothing

  , primStepEntry $(textNameLit 'Clash.Sized.Internal.Unsigned.maxBound#) $ \case
      PrimStepContext{..}
        | Just (litTy,mb) <- extractKnownNat tcm tys
        -> let maxB = (2 ^ mb) - 1
           in  reduce (mkUnsignedLit ty litTy mb maxB)
      _ -> Nothing


-- Num
  , primStepEntry $(textNameLit '(Clash.Sized.Internal.Unsigned.+#)) $ \case
      PrimStepContext{..}
        | Just (_, kn) <- extractKnownNat tcm tys
        , Just val <- reifyNat kn (liftUnsigned2 (Unsigned.+#) ty tcm tys args)
        -> reduce val
      _ -> Nothing

  , primStepEntry $(textNameLit '(Clash.Sized.Internal.Unsigned.-#)) $ \case
      PrimStepContext{..}
        | Just (_, kn) <- extractKnownNat tcm tys
        , Just val <- reifyNat kn (liftUnsigned2 (Unsigned.-#) ty tcm tys args)
        -> reduce val
      _ -> Nothing

  , primStepEntry $(textNameLit '(Clash.Sized.Internal.Unsigned.*#)) $ \case
      PrimStepContext{..}
        | Just (_, kn) <- extractKnownNat tcm tys
        , Just val <- reifyNat kn (liftUnsigned2 (Unsigned.*#) ty tcm tys args)
        -> reduce val
      _ -> Nothing

  , primStepEntry $(textNameLit 'Clash.Sized.Internal.Unsigned.negate#) $ \case
      PrimStepContext{..}
        | Just (nTy, kn) <- extractKnownNat tcm tys
        , [i] <- unsignedLiterals' args
        -> let val = reifyNat kn (op (fromInteger i))
        in reduce (mkUnsignedLit ty nTy kn val)
        where
          op :: KnownNat n => Unsigned n -> Proxy n -> Integer
          op u _ = toInteger (Unsigned.negate# u)
      _ -> Nothing


-- ExtendingNum
  , primStepEntry $(textNameLit 'Clash.Sized.Internal.Unsigned.plus#) $ \case
      PrimStepContext{..} -- :: Unsigned m -> Unsigned n -> Unsigned (Max m n + 1)
        | Just (i,j) <- unsignedLiterals args
        -> let ty' = piResultTys tcm ty tys
               (_,resTy) = splitFunForallTy ty'
               (TyConApp _ [resSizeTy]) = tyView resTy
               Right resSizeInt = runExcept (tyNatSize tcm resSizeTy)
           in  reduce (mkUnsignedLit resTy resSizeTy resSizeInt (i+j))
      _ -> Nothing

  , primStepEntry $(textNameLit 'Clash.Sized.Internal.Unsigned.minus#) $ \case
      PrimStepContext{..}
        | [i,j] <- unsignedLiterals' args
        -> let ty' = piResultTys tcm ty tys
               (_,resTy) = splitFunForallTy ty'
               (TyConApp _ [resSizeTy]) = tyView resTy
               Right resSizeInt = runExcept (tyNatSize tcm resSizeTy)
               val = reifyNat resSizeInt (runSizedF (Unsigned.-#) i j)
          in   reduce (mkUnsignedLit resTy resSizeTy resSizeInt val)
      _ -> Nothing

  , primStepEntry $(textNameLit 'Clash.Sized.Internal.Unsigned.times#) $ \case
      PrimStepContext{..}
        | Just (i,j) <- unsignedLiterals args
        -> let ty' = piResultTys tcm ty tys
               (_,resTy) = splitFunForallTy ty'
               (TyConApp _ [resSizeTy]) = tyView resTy
               Right resSizeInt = runExcept (tyNatSize tcm resSizeTy)
           in  reduce (mkUnsignedLit resTy resSizeTy resSizeInt (i*j))
      _ -> Nothing


-- Integral
  , primStepEntry $(textNameLit 'Clash.Sized.Internal.Unsigned.quot#) $ \case
      PrimStepContext{..}
        | Just (_, kn) <- extractKnownNat tcm tys
        , Just val <- reifyNat kn (liftUnsigned2 (Unsigned.quot#) ty tcm tys args)
        -> reduce $ catchDivByZero val
      _ -> Nothing

  , primStepEntry $(textNameLit 'Clash.Sized.Internal.Unsigned.rem#) $ \case
      PrimStepContext{..}
        | Just (_, kn) <- extractKnownNat tcm tys
        , Just val <- reifyNat kn (liftUnsigned2 (Unsigned.rem#) ty tcm tys args)
        -> reduce $ catchDivByZero val
      _ -> Nothing

  , primStepEntry $(textNameLit 'Clash.Sized.Internal.Unsigned.toInteger#) $ \case
      PrimStepContext{..}
        | [PrimVal p _ [_, Lit (IntegerLiteral i)]] <- args
        , primName p == showt 'Clash.Sized.Internal.Unsigned.fromInteger#
        -> reduce (integerToIntegerLiteral i)
      _ -> Nothing


-- Bits
  , primStepEntry $(textNameLit 'Clash.Sized.Internal.Unsigned.and#) $ \case
      PrimStepContext{..}
        | Just (i,j) <- unsignedLiterals args
        , Just (nTy, kn) <- extractKnownNat tcm tys
        -> reduce (mkUnsignedLit ty nTy kn (i .&. j))
      _ -> Nothing

  , primStepEntry $(textNameLit 'Clash.Sized.Internal.Unsigned.or#) $ \case
      PrimStepContext{..}
        | Just (i,j) <- unsignedLiterals args
        , Just (nTy, kn) <- extractKnownNat tcm tys
        -> reduce (mkUnsignedLit ty nTy kn (i .|. j))
      _ -> Nothing

  , primStepEntry $(textNameLit 'Clash.Sized.Internal.Unsigned.xor#) $ \case
      PrimStepContext{..}
        | Just (i,j) <- unsignedLiterals args
        , Just (nTy, kn) <- extractKnownNat tcm tys
        -> reduce (mkUnsignedLit ty nTy kn (i `xor` j))
      _ -> Nothing

  , primStepEntry $(textNameLit 'Clash.Sized.Internal.Unsigned.complement#) $ \case
      PrimStepContext{..}
        | [i] <- unsignedLiterals' args
        , Just (nTy, kn) <- extractKnownNat tcm tys
        -> let val = reifyNat kn (op (fromInteger i))
        in reduce (mkUnsignedLit ty nTy kn val)
        where
          op :: KnownNat n => Unsigned n -> Proxy n -> Integer
          op u _ = toInteger (Unsigned.complement# u)
      _ -> Nothing

  , primStepEntry $(textNameLit 'Clash.Sized.Internal.Unsigned.shiftL#) $ \case
      PrimStepContext{..} -- :: forall n. KnownNat n => Unsigned n -> Int -> Unsigned n
        | Just (nTy,kn,i,j) <- unsignedLitIntLit tcm tys args
          -> let val = reifyNat kn (op (fromInteger i) (fromInteger j))
          in reduce (mkUnsignedLit ty nTy kn val)
          where
            op :: KnownNat n => Unsigned n -> Int -> Proxy n -> Integer
            op u i _ = toInteger (Unsigned.shiftL# u i)
      _ -> Nothing

  , primStepEntry $(textNameLit 'Clash.Sized.Internal.Unsigned.shiftR#) $ \case
      PrimStepContext{..} -- :: forall n. KnownNat n => Unsigned n -> Int -> Unsigned n
        | Just (nTy,kn,i,j) <- unsignedLitIntLit tcm tys args
          -> let val = reifyNat kn (op (fromInteger i) (fromInteger j))
          in reduce (mkUnsignedLit ty nTy kn val)
          where
            op :: KnownNat n => Unsigned n -> Int -> Proxy n -> Integer
            op u i _ = toInteger (Unsigned.shiftR# u i)
      _ -> Nothing

  , primStepEntry $(textNameLit 'Clash.Sized.Internal.Unsigned.rotateL#) $ \case
      PrimStepContext{..} -- :: forall n. KnownNat n => Unsigned n -> Int -> Unsigned n
        | Just (nTy,kn,i,j) <- unsignedLitIntLit tcm tys args
          -> let val = reifyNat kn (op (fromInteger i) (fromInteger j))
          in reduce (mkUnsignedLit ty nTy kn val)
          where
            op :: KnownNat n => Unsigned n -> Int -> Proxy n -> Integer
            op u i _ = toInteger (Unsigned.rotateL# u i)
      _ -> Nothing

  , primStepEntry $(textNameLit 'Clash.Sized.Internal.Unsigned.rotateR#) $ \case
      PrimStepContext{..} -- :: forall n. KnownNat n => Unsigned n -> Int -> Unsigned n
        | Just (nTy,kn,i,j) <- unsignedLitIntLit tcm tys args
          -> let val = reifyNat kn (op (fromInteger i) (fromInteger j))
          in reduce (mkUnsignedLit ty nTy kn val)
          where
            op :: KnownNat n => Unsigned n -> Int -> Proxy n -> Integer
            op u i _ = toInteger (Unsigned.rotateR# u i)
      _ -> Nothing


-- Resize
  , primStepEntry $(textNameLit 'Clash.Sized.Internal.Unsigned.resize#) $ \case
      PrimStepContext{..} -- forall n m . KnownNat m => Unsigned n -> Unsigned m
        | _ : mTy : _ <- tys
        , Right km <- runExcept (tyNatSize tcm mTy)
        , [i] <- unsignedLiterals' args
        -> let bitsKeep = (bit (fromInteger km)) - 1
               val = i .&. bitsKeep
        in reduce (mkUnsignedLit ty mTy km val)
      _ -> Nothing


-- Conversions
  , primStepEntry $(textNameLit 'Clash.Sized.Internal.Unsigned.unsignedToWord) $ \case
      PrimStepContext{..}
        | isSubj
        , [a] <- unsignedLiterals' args
        -> let b = Unsigned.unsignedToWord (U (fromInteger a))
               (_,tyView -> TyConApp wordTcNm []) = splitFunForallTy ty
               (Just wordTc) = UniqMap.lookup wordTcNm tcm
               [wordDc] = tyConDataCons wordTc
           in  reduce (mkApps (Data wordDc) [Left (Literal (WordLiteral (toInteger b)))])
      _ -> Nothing

  , primStepEntry $(textNameLit 'Clash.Sized.Internal.Unsigned.unsigned8toWord8) $ \case
      PrimStepContext{..}
        | isSubj
        , [a] <- unsignedLiterals' args
        -> let b = Unsigned.unsigned8toWord8 (U (fromInteger a))
               (_,tyView -> TyConApp wordTcNm []) = splitFunForallTy ty
               (Just wordTc) = UniqMap.lookup wordTcNm tcm
               [wordDc] = tyConDataCons wordTc
           in  reduce (mkApps (Data wordDc) [Left (Literal (WordLiteral (toInteger b)))])
      _ -> Nothing

  , primStepEntry $(textNameLit 'Clash.Sized.Internal.Unsigned.unsigned16toWord16) $ \case
      PrimStepContext{..}
        | isSubj
        , [a] <- unsignedLiterals' args
        -> let b = Unsigned.unsigned16toWord16 (U (fromInteger a))
               (_,tyView -> TyConApp wordTcNm []) = splitFunForallTy ty
               (Just wordTc) = UniqMap.lookup wordTcNm tcm
               [wordDc] = tyConDataCons wordTc
           in  reduce (mkApps (Data wordDc) [Left (Literal (WordLiteral (toInteger b)))])
      _ -> Nothing

  , primStepEntry $(textNameLit 'Clash.Sized.Internal.Unsigned.unsigned32toWord32) $ \case
      PrimStepContext{..}
        | isSubj
        , [a] <- unsignedLiterals' args
        -> let b = Unsigned.unsigned32toWord32 (U (fromInteger a))
               (_,tyView -> TyConApp wordTcNm []) = splitFunForallTy ty
               (Just wordTc) = UniqMap.lookup wordTcNm tcm
               [wordDc] = tyConDataCons wordTc
           in  reduce (mkApps (Data wordDc) [Left (Literal (WordLiteral (toInteger b)))])
      _ -> Nothing

  ]
