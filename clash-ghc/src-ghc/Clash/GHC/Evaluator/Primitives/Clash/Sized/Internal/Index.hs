{-|
  Copyright   :  (C) 2013-2016, University of Twente,
                     2016-2017, Myrtle Software Ltd,
                     2017-2022, Google Inc.,
                     2017-2026, QBayLogic B.V.
  License     :  BSD2 (see the file LICENSE)
  Maintainer  :  QBayLogic B.V. <devops@qbaylogic.com>
-}

{-# LANGUAGE MagicHash #-}
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
  [ ( $(textNameLit 'Clash.Sized.Internal.Index.pack#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..}
            | nTy : _ <- tys
            , Right _ <- runExcept (tyNatSize tcm nTy)
            , [i] <- indexLiterals' args
            -> let resTyInfo = extractTySizeInfo tcm ty tys
               in  reduce (mkBitVectorLit' resTyInfo 0 i)
          _ -> Nothing
    )
  , ( $(textNameLit 'Clash.Sized.Internal.Index.unpack#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..}
            | Just (nTy,kn) <- extractKnownNat tcm tys
            , [(0,i)] <- bitVectorLiterals' args
            -> reduce (mkIndexLit ty nTy kn i)
          _ -> Nothing
    )

-- Eq
  , ( $(textNameLit 'Clash.Sized.Internal.Index.eq#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} | Just (i,j) <- indexLiterals args
            -> reduce (boolToBoolLiteral tcm ty (i == j))
          _ -> Nothing
    )
  , ( $(textNameLit 'Clash.Sized.Internal.Index.neq#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} | Just (i,j) <- indexLiterals args
            -> reduce (boolToBoolLiteral tcm ty (i /= j))
          _ -> Nothing
    )

-- Ord
  , ( $(textNameLit 'Clash.Sized.Internal.Index.lt#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..}
            | Just (i,j) <- indexLiterals args
            -> reduce (boolToBoolLiteral tcm ty (i < j))
          _ -> Nothing
    )
  , ( $(textNameLit 'Clash.Sized.Internal.Index.ge#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..}
            | Just (i,j) <- indexLiterals args
            -> reduce (boolToBoolLiteral tcm ty (i >= j))
          _ -> Nothing
    )
  , ( $(textNameLit 'Clash.Sized.Internal.Index.gt#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..}
            | Just (i,j) <- indexLiterals args
            -> reduce (boolToBoolLiteral tcm ty (i > j))
          _ -> Nothing
    )
  , ( $(textNameLit 'Clash.Sized.Internal.Index.le#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..}
            | Just (i,j) <- indexLiterals args
            -> reduce (boolToBoolLiteral tcm ty (i <= j))
          _ -> Nothing
    )

-- Enum
  , ( $(textNameLit 'Clash.Sized.Internal.Index.toEnum#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..}
            | [i] <- intCLiterals' args
            , Just (nTy, mb) <- extractKnownNat tcm tys
            -> reduce (mkIndexLit ty nTy mb i)
          _ -> Nothing
    )

  , ( $(textNameLit 'Clash.Sized.Internal.Index.fromEnum#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..}
            | [i] <- indexLiterals' args
            -> let resTy = getResultTy tcm ty tys
                in reduce (mkIntCLit tcm IntLiteral i resTy)
          _ -> Nothing
    )

-- Bounded
  , ( $(textNameLit 'Clash.Sized.Internal.Index.maxBound#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..}
            | Just (nTy,mb) <- extractKnownNat tcm tys
            -> reduce (mkIndexLit ty nTy mb (mb - 1))
          _ -> Nothing
    )

-- Num
  , ( $(textNameLit '(Clash.Sized.Internal.Index.+#))
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..}
            | Just (nTy,kn) <- extractKnownNat tcm tys
            , [i,j] <- indexLiterals' args
            -> reduce (mkIndexLit ty nTy kn (i + j))
          _ -> Nothing
    )
  , ( $(textNameLit '(Clash.Sized.Internal.Index.-#))
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..}
            | Just (nTy,kn) <- extractKnownNat tcm tys
            , [i,j] <- indexLiterals' args
            -> reduce (mkIndexLit ty nTy kn (i - j))
          _ -> Nothing
    )
  , ( $(textNameLit '(Clash.Sized.Internal.Index.*#))
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..}
            | Just (nTy,kn) <- extractKnownNat tcm tys
            , [i,j] <- indexLiterals' args
            -> reduce (mkIndexLit ty nTy kn (i * j))
          _ -> Nothing
    )

-- ExtendingNum
  , ( $(textNameLit 'Clash.Sized.Internal.Index.plus#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..}
            | mTy : nTy : _ <- tys
            , Right _ <- runExcept (tyNatSize tcm mTy)
            , Right _ <- runExcept (tyNatSize tcm nTy)
            , Just (i,j) <- indexLiterals args
            -> let resTyInfo = extractTySizeInfo tcm ty tys
               in  reduce (mkIndexLit' resTyInfo (i + j))
          _ -> Nothing
    )
  , ( $(textNameLit 'Clash.Sized.Internal.Index.minus#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..}
            | mTy : nTy : _ <- tys
            , Right _ <- runExcept (tyNatSize tcm mTy)
            , Right _ <- runExcept (tyNatSize tcm nTy)
            , Just (i,j) <- indexLiterals args
            -> let resTyInfo = extractTySizeInfo tcm ty tys
               in  reduce (mkIndexLit' resTyInfo (i - j))
          _ -> Nothing
    )
  , ( $(textNameLit 'Clash.Sized.Internal.Index.times#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..}
            | mTy : nTy : _ <- tys
            , Right _ <- runExcept (tyNatSize tcm mTy)
            , Right _ <- runExcept (tyNatSize tcm nTy)
            , Just (i,j) <- indexLiterals args
            -> let resTyInfo = extractTySizeInfo tcm ty tys
               in  reduce (mkIndexLit' resTyInfo (i * j))
          _ -> Nothing
    )

-- Integral
  , ( $(textNameLit 'Clash.Sized.Internal.Index.quot#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..}
            | Just (nTy,kn) <- extractKnownNat tcm tys
            , Just (i,j) <- indexLiterals args
            -> reduce $ catchDivByZero (mkIndexLit ty nTy kn (i `quot` j))
          _ -> Nothing
    )
  , ( $(textNameLit 'Clash.Sized.Internal.Index.rem#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..}
            | Just (nTy,kn) <- extractKnownNat tcm tys
            , Just (i,j) <- indexLiterals args
            -> reduce $ catchDivByZero (mkIndexLit ty nTy kn (i `rem` j))
          _ -> Nothing
    )
  , ( $(textNameLit 'Clash.Sized.Internal.Index.toInteger#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..}
            | [PrimVal p _ [_, Lit (IntegerLiteral i)]] <- args
            , primName p == showt 'Clash.Sized.Internal.Index.fromInteger#
            -> reduce (integerToIntegerLiteral i)
          _ -> Nothing
    )

-- Resize
  , ( $(textNameLit 'Clash.Sized.Internal.Index.resize#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..}
            | Just (mTy,m) <- extractKnownNat tcm tys
            , [i] <- indexLiterals' args
            -> reduce (mkIndexLit ty mTy m i)
          _ -> Nothing
    )
  ]
