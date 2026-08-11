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
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UnboxedTuples #-}

module Clash.GHC.Evaluator.Primitives.Clash.Sized.Internal.Index
  ( primitives
  ) where

import           Control.Monad.Trans.Except (runExcept)
import           Data.Text           (Text)
import           Data.Text.Extra     (showt)

import           Clash.Core.Evaluator.Types
import           Clash.Core.Literal  (Literal (..))
import Clash.Core.Term (PrimInfo (..))
import Clash.Core.Util (tyNatSize)
import Clash.Util (textNameLit)

import qualified Clash.Sized.Internal.Index

import Clash.GHC.Evaluator.Primitive.Util

primitives :: [(Text, PrimStep)]
primitives =
--------
-- Index
--------
-- BitPack
  [ primStepEntry $(textNameLit 'Clash.Sized.Internal.Index.pack#) $ \case
      PrimStepContext{..}
        | nTy : _ <- tys
        , Right _ <- runExcept (tyNatSize tcm nTy)
        , [i] <- indexLiterals' args
        -> let resTyInfo = extractTySizeInfo tcm ty tys
           in  reduce (mkBitVectorLit' resTyInfo 0 i)
      _ -> Nothing

  , primStepEntry $(textNameLit 'Clash.Sized.Internal.Index.unpack#) $ \case
      PrimStepContext{..}
        | Just (nTy,kn) <- extractKnownNat tcm tys
        , [(0,i)] <- bitVectorLiterals' args
        -> reduce (mkIndexLit ty nTy kn i)
      _ -> Nothing


-- Eq
  , primStepEntry $(textNameLit 'Clash.Sized.Internal.Index.eq#) $ \case
      PrimStepContext{..} | Just (i,j) <- indexLiterals args
        -> reduce (boolToBoolLiteral tcm ty (i == j))
      _ -> Nothing

  , primStepEntry $(textNameLit 'Clash.Sized.Internal.Index.neq#) $ \case
      PrimStepContext{..} | Just (i,j) <- indexLiterals args
        -> reduce (boolToBoolLiteral tcm ty (i /= j))
      _ -> Nothing


-- Ord
  , primStepEntry $(textNameLit 'Clash.Sized.Internal.Index.lt#) $ \case
      PrimStepContext{..}
        | Just (i,j) <- indexLiterals args
        -> reduce (boolToBoolLiteral tcm ty (i < j))
      _ -> Nothing

  , primStepEntry $(textNameLit 'Clash.Sized.Internal.Index.ge#) $ \case
      PrimStepContext{..}
        | Just (i,j) <- indexLiterals args
        -> reduce (boolToBoolLiteral tcm ty (i >= j))
      _ -> Nothing

  , primStepEntry $(textNameLit 'Clash.Sized.Internal.Index.gt#) $ \case
      PrimStepContext{..}
        | Just (i,j) <- indexLiterals args
        -> reduce (boolToBoolLiteral tcm ty (i > j))
      _ -> Nothing

  , primStepEntry $(textNameLit 'Clash.Sized.Internal.Index.le#) $ \case
      PrimStepContext{..}
        | Just (i,j) <- indexLiterals args
        -> reduce (boolToBoolLiteral tcm ty (i <= j))
      _ -> Nothing


-- Enum
  , primStepEntry $(textNameLit 'Clash.Sized.Internal.Index.toEnum#) $ \case
      PrimStepContext{..}
        | [i] <- intCLiterals' args
        , Just (nTy, mb) <- extractKnownNat tcm tys
        -> reduce (mkIndexLit ty nTy mb i)
      _ -> Nothing

  , primStepEntry $(textNameLit 'Clash.Sized.Internal.Index.fromEnum#) $ \case
      PrimStepContext{..}
        | [i] <- indexLiterals' args
        -> let resTy = getResultTy tcm ty tys
            in reduce (mkIntCLit tcm IntLiteral i resTy)
      _ -> Nothing


-- Bounded
  , primStepEntry $(textNameLit 'Clash.Sized.Internal.Index.maxBound#) $ \case
      PrimStepContext{..}
        | Just (nTy,mb) <- extractKnownNat tcm tys
        -> reduce (mkIndexLit ty nTy mb (mb - 1))
      _ -> Nothing


-- Num
  , primStepEntry $(textNameLit '(Clash.Sized.Internal.Index.+#)) $ \case
      PrimStepContext{..}
        | Just (nTy,kn) <- extractKnownNat tcm tys
        , [i,j] <- indexLiterals' args
        -> reduce (mkIndexLit ty nTy kn (i + j))
      _ -> Nothing

  , primStepEntry $(textNameLit '(Clash.Sized.Internal.Index.-#)) $ \case
      PrimStepContext{..}
        | Just (nTy,kn) <- extractKnownNat tcm tys
        , [i,j] <- indexLiterals' args
        -> reduce (mkIndexLit ty nTy kn (i - j))
      _ -> Nothing

  , primStepEntry $(textNameLit '(Clash.Sized.Internal.Index.*#)) $ \case
      PrimStepContext{..}
        | Just (nTy,kn) <- extractKnownNat tcm tys
        , [i,j] <- indexLiterals' args
        -> reduce (mkIndexLit ty nTy kn (i * j))
      _ -> Nothing


-- ExtendingNum
  , primStepEntry $(textNameLit 'Clash.Sized.Internal.Index.plus#) $ \case
      PrimStepContext{..}
        | mTy : nTy : _ <- tys
        , Right _ <- runExcept (tyNatSize tcm mTy)
        , Right _ <- runExcept (tyNatSize tcm nTy)
        , Just (i,j) <- indexLiterals args
        -> let resTyInfo = extractTySizeInfo tcm ty tys
           in  reduce (mkIndexLit' resTyInfo (i + j))
      _ -> Nothing

  , primStepEntry $(textNameLit 'Clash.Sized.Internal.Index.minus#) $ \case
      PrimStepContext{..}
        | mTy : nTy : _ <- tys
        , Right _ <- runExcept (tyNatSize tcm mTy)
        , Right _ <- runExcept (tyNatSize tcm nTy)
        , Just (i,j) <- indexLiterals args
        -> let resTyInfo = extractTySizeInfo tcm ty tys
           in  reduce (mkIndexLit' resTyInfo (i - j))
      _ -> Nothing

  , primStepEntry $(textNameLit 'Clash.Sized.Internal.Index.times#) $ \case
      PrimStepContext{..}
        | mTy : nTy : _ <- tys
        , Right _ <- runExcept (tyNatSize tcm mTy)
        , Right _ <- runExcept (tyNatSize tcm nTy)
        , Just (i,j) <- indexLiterals args
        -> let resTyInfo = extractTySizeInfo tcm ty tys
           in  reduce (mkIndexLit' resTyInfo (i * j))
      _ -> Nothing


-- Integral
  , primStepEntry $(textNameLit 'Clash.Sized.Internal.Index.quot#) $ \case
      PrimStepContext{..}
        | Just (nTy,kn) <- extractKnownNat tcm tys
        , Just (i,j) <- indexLiterals args
        -> reduce $ catchDivByZero (mkIndexLit ty nTy kn (i `quot` j))
      _ -> Nothing

  , primStepEntry $(textNameLit 'Clash.Sized.Internal.Index.rem#) $ \case
      PrimStepContext{..}
        | Just (nTy,kn) <- extractKnownNat tcm tys
        , Just (i,j) <- indexLiterals args
        -> reduce $ catchDivByZero (mkIndexLit ty nTy kn (i `rem` j))
      _ -> Nothing

  , primStepEntry $(textNameLit 'Clash.Sized.Internal.Index.toInteger#) $ \case
      PrimStepContext{..}
        | [PrimVal p _ [_, Lit (IntegerLiteral i)]] <- args
        , primName p == showt 'Clash.Sized.Internal.Index.fromInteger#
        -> reduce (integerToIntegerLiteral i)
      _ -> Nothing


-- Resize
  , primStepEntry $(textNameLit 'Clash.Sized.Internal.Index.resize#) $ \case
      PrimStepContext{..}
        | Just (mTy,m) <- extractKnownNat tcm tys
        , [i] <- indexLiterals' args
        -> reduce (mkIndexLit ty mTy m i)
      _ -> Nothing

  ]
