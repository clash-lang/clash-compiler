{-|
  Copyright   :  (C) 2013-2016, University of Twente,
                     2016-2017, Myrtle Software Ltd,
                     2017-2022, Google Inc.,
                     2017-2026, QBayLogic B.V.
  License     :  BSD2 (see the file LICENSE)
  Maintainer  :  QBayLogic B.V. <devops@qbaylogic.com>

  Reductions of the primitives of "GHC.Num.Integer" in the scoped evaluator:
  ports of @Clash.GHC.Evaluator.Primitives.GHC.Num.Integer@. See
  "Clash.Core.Scoped.Evaluator.Primitives" for how to port them.
-}

{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UnboxedTuples #-}

module Clash.Core.Scoped.Evaluator.Primitives.GHC.Num.Integer
  ( primitives
  ) where

import Data.Bits
import qualified Data.Primitive.ByteArray as BA
import Data.Text (Text)
import GHC.Float
import GHC.Int
import GHC.Integer
  (decodeDoubleInteger,encodeDoubleInteger,compareInteger,orInteger,andInteger,
   xorInteger,complementInteger,absInteger,signumInteger)
import GHC.Num.Integer (Integer (..), integerEncodeFloat#)

import Clash.Core.Literal (Literal (..))
import Clash.Util (flogBase, textNameLit)

import qualified GHC.Num.Integer

import Clash.Core.Scoped.Evaluator.Primitives.Util
import Clash.Core.Scoped.Evaluator.Types
import Clash.Core.Scoped.Syntax

primitives :: [(Text, PrimReduction)]
primitives =
  [ primStepEntry $(textNameLit 'GHC.Num.Integer.integerLogBase#) $ \case
      PrimStepContext{..}
        | Just (a,b) <- integerLiterals args
        , Just c <- flogBase a b
        -> (reduce . Literal . WordLiteral . toInteger) c
      _ -> Nothing


  , primStepEntry $(textNameLit 'GHC.Num.Integer.integerToInt#) $ \case
      PrimStepContext{..}
        | [i] <- integerLiterals' args
        -> reduce (integerToIntLiteral i)
      _ -> Nothing

  , primStepEntry $(textNameLit 'GHC.Num.Integer.integerDecodeDouble#) $ \case
      PrimStepContext{..} -- :: Double# -> (#Integer, Int##)
        | [Lit (DoubleLiteral i)] <- args
        -> let !(D# a)  = castWord64ToDouble i
               !(# b, c #) = decodeDoubleInteger a
        in reduce $ mkResultCon te sc ty tys
             [ integerToIntegerLiteral b
             , integerToIntLiteral . toInteger $ I# c ]
      _ -> Nothing

  , primStepEntry $(textNameLit 'GHC.Num.Integer.integerEncodeDouble#) $ \case
      PrimStepContext{..} -- :: Integer -> Int# -> Double
        | [iV, Lit (IntLiteral j)] <- args
        , [i] <- integerLiterals' [iV]
        -> let !(I# k') = fromInteger j
               r = encodeDoubleInteger i k'
        in  reduce . Literal . DoubleLiteral . castDoubleToWord64 $ D# r
      _ -> Nothing

  , primStepEntry $(textNameLit 'GHC.Num.Integer.integerEncodeFloat#) $ \case
      PrimStepContext{..}
        | [iV, Lit (IntLiteral j)] <- args
        , [i] <- integerLiterals' [iV]
        -> let !(I# k') = fromInteger j
               r = integerEncodeFloat# i k'
            in reduce . Literal . FloatLiteral . castFloatToWord32 $ F# r
      _ -> Nothing

  , primStepEntry $(textNameLit 'GHC.Num.Integer.integerQuotRem#) $ \case
      PrimStepContext{..} -- :: Integer -> Integer -> (#Integer, Integer#)
        | [i, j] <- integerLiterals' args
        -> let (q,r) = quotRem i j
        in reduce $ mkResultCon te sc ty tys
             [ catchDivByZero (integerToIntegerLiteral q)
             , catchDivByZero (integerToIntegerLiteral r) ]
      _ -> Nothing

  , primStepEntry $(textNameLit 'GHC.Num.Integer.integerAdd) $ \case
      PrimStepContext{..}
        | Just (i,j) <- integerLiterals args
        -> reduce (integerToIntegerLiteral (i+j))
      _ -> Nothing

  , primStepEntry $(textNameLit 'GHC.Num.Integer.integerSub) $ \case
      PrimStepContext{..}
        | Just (i,j) <- integerLiterals args
        -> reduce (integerToIntegerLiteral (i-j))
      _ -> Nothing

  , primStepEntry $(textNameLit 'GHC.Num.Integer.integerMul) $ \case
      PrimStepContext{..}
        | Just (i,j) <- integerLiterals args
        -> reduce (integerToIntegerLiteral (i*j))
      _ -> Nothing

  , primStepEntry $(textNameLit 'GHC.Num.Integer.integerNegate) $ \case
      PrimStepContext{..}
        | [i] <- integerLiterals' args
        -> reduce (integerToIntegerLiteral (negate i))
      _ -> Nothing

  , primStepEntry $(textNameLit 'GHC.Num.Integer.integerDiv) $ \case
      PrimStepContext{..}
        | Just (i,j) <- integerLiterals args
        -> reduce $ catchDivByZero (integerToIntegerLiteral (i `div` j))
      _ -> Nothing

  , primStepEntry $(textNameLit 'GHC.Num.Integer.integerMod) $ \case
      PrimStepContext{..}
        | Just (i,j) <- integerLiterals args
        -> reduce $ catchDivByZero (integerToIntegerLiteral (i `mod` j))
      _ -> Nothing

  , primStepEntry $(textNameLit 'GHC.Num.Integer.integerQuot) $ \case
      PrimStepContext{..}
        | Just (i,j) <- integerLiterals args
        -> reduce $ catchDivByZero (integerToIntegerLiteral (i `quot` j))
      _ -> Nothing

  , primStepEntry $(textNameLit 'GHC.Num.Integer.integerRem) $ \case
      PrimStepContext{..}
        | Just (i,j) <- integerLiterals args
        -> reduce $ catchDivByZero (integerToIntegerLiteral (i `rem` j))
      _ -> Nothing

  , primStepEntry $(textNameLit 'GHC.Num.Integer.integerDivMod#) $ \case
      PrimStepContext{..}
        | Just (i,j) <- integerLiterals args
        -> let (d,m) = divMod i j
           in  reduce $ mkResultCon te sc ty tys
                 [ catchDivByZero (Literal (IntegerLiteral d))
                 , catchDivByZero (Literal (IntegerLiteral m)) ]
      _ -> Nothing

  , primStepEntry $(textNameLit 'GHC.Num.Integer.integerGt) $ \case
      PrimStepContext{..}
        | Just (i,j) <- integerLiterals args
        -> reduce (boolToBoolLiteral te ty (i > j))
      _ -> Nothing

  , primStepEntry $(textNameLit 'GHC.Num.Integer.integerGe) $ \case
      PrimStepContext{..}
        | Just (i,j) <- integerLiterals args
        -> reduce (boolToBoolLiteral te ty (i >= j))
      _ -> Nothing

  , primStepEntry $(textNameLit 'GHC.Num.Integer.integerEq) $ \case
      PrimStepContext{..}
        | Just (i,j) <- integerLiterals args
        -> reduce (boolToBoolLiteral te ty (i == j))
      _ -> Nothing

  , primStepEntry $(textNameLit 'GHC.Num.Integer.integerNe) $ \case
      PrimStepContext{..}
        | Just (i,j) <- integerLiterals args
        -> reduce (boolToBoolLiteral te ty (i /= j))
      _ -> Nothing

  , primStepEntry $(textNameLit 'GHC.Num.Integer.integerLt) $ \case
      PrimStepContext{..}
        | Just (i,j) <- integerLiterals args
        -> reduce (boolToBoolLiteral te ty (i < j))
      _ -> Nothing

  , primStepEntry $(textNameLit 'GHC.Num.Integer.integerLe) $ \case
      PrimStepContext{..}
        | Just (i,j) <- integerLiterals args
        -> reduce (boolToBoolLiteral te ty (i <= j))
      _ -> Nothing

  , primStepEntry $(textNameLit 'GHC.Num.Integer.integerGt#) $ \case
      PrimStepContext{..}
        | Just (i,j) <- integerLiterals args
        -> reduce (boolToIntLiteral (i > j))
      _ -> Nothing

  , primStepEntry $(textNameLit 'GHC.Num.Integer.integerGe#) $ \case
      PrimStepContext{..}
        | Just (i,j) <- integerLiterals args
        -> reduce (boolToIntLiteral (i >= j))
      _ -> Nothing

  , primStepEntry $(textNameLit 'GHC.Num.Integer.integerEq#) $ \case
      PrimStepContext{..}
        | Just (i,j) <- integerLiterals args
        -> reduce (boolToIntLiteral (i == j))
      _ -> Nothing

  , primStepEntry $(textNameLit 'GHC.Num.Integer.integerNe#) $ \case
      PrimStepContext{..}
        | Just (i,j) <- integerLiterals args
        -> reduce (boolToIntLiteral (i /= j))
      _ -> Nothing

  , primStepEntry $(textNameLit 'GHC.Num.Integer.integerLt#) $ \case
      PrimStepContext{..}
        | Just (i,j) <- integerLiterals args
        -> reduce (boolToIntLiteral (i < j))
      _ -> Nothing

  , primStepEntry $(textNameLit 'GHC.Num.Integer.integerLe#) $ \case
      PrimStepContext{..}
        | Just (i,j) <- integerLiterals args
        -> reduce (boolToIntLiteral (i <= j))
      _ -> Nothing

  , primStepEntry $(textNameLit 'GHC.Num.Integer.integerCompare) $ \case
      PrimStepContext{..}
        | [i, j] <- integerLiterals' args
        -> reduce (orderingToOrderingLiteral te ty (compareInteger i j))
      _ -> Nothing

  , primStepEntry $(textNameLit 'GHC.Num.Integer.integerShiftR#) $ \case
      PrimStepContext{..}
        | [iV, Lit (WordLiteral j)] <- args
        , [i] <- integerLiterals' [iV]
        -> reduce (integerToIntegerLiteral (i `shiftR` fromInteger j))
      _ -> Nothing

  , primStepEntry $(textNameLit 'GHC.Num.Integer.integerShiftL#) $ \case
      PrimStepContext{..}
        | [iV, Lit (WordLiteral j)] <- args
        , [i] <- integerLiterals' [iV]
        -> reduce (integerToIntegerLiteral (i `shiftL` fromInteger j))
      _ -> Nothing

  , primStepEntry $(textNameLit 'GHC.Num.Integer.integerFromWord#) $ \case
      PrimStepContext{..}
        | [Lit (WordLiteral w)] <- args
        -> reduce (Literal (IntegerLiteral w))
      _ -> Nothing

  , primStepEntry $(textNameLit 'GHC.Num.Integer.integerToWord#) $ \case
      PrimStepContext{..}
        | [i] <- integerLiterals' args
        -> reduce (integerToWordLiteral i)
      _ -> Nothing

  , primStepEntry $(textNameLit 'GHC.Num.Integer.integerTestBit#) $ \case
      PrimStepContext{..} -- :: Integer -> Int# -> Int#
        | [Lit (IntegerLiteral i), Lit (WordLiteral j)] <- args
        -> reduce (boolToIntLiteral (testBit i (fromInteger j)))
      _ -> Nothing

  , primStepEntry $(textNameLit 'GHC.Num.Integer.IS) $ \case
      PrimStepContext{..}
        | [Lit (IntLiteral i)] <- args
        -> reduce (Literal (IntegerLiteral i))
      _ -> Nothing

  , primStepEntry $(textNameLit 'GHC.Num.Integer.IP) $ \case
      PrimStepContext{..}
        | [Lit (ByteArrayLiteral (BA.ByteArray ba))] <- args
        -> reduce (Literal (IntegerLiteral (IP ba)))
        | [Lit l] <- args
        -> error ("IP: " <> show l)
      _ -> Nothing

  , primStepEntry $(textNameLit 'GHC.Num.Integer.IN) $ \case
      PrimStepContext{..}
        | [Lit (ByteArrayLiteral (BA.ByteArray ba))] <- args
        -> reduce (Literal (IntegerLiteral (IN ba)))
        | [Lit l] <- args
        -> error ("IN: " <> show l)
      _ -> Nothing

  , primStepEntry $(textNameLit 'GHC.Num.Integer.integerFromNatural) $ \case
      PrimStepContext{..}
        | [i] <- naturalLiterals' args
        -> reduce (Literal (IntegerLiteral (toInteger i)))
      _ -> Nothing

  , primStepEntry $(textNameLit 'GHC.Num.Integer.integerToNatural) $ \case
      PrimStepContext{..}
        | [i] <- integerLiterals' args
        ->
         let nTy = getResultTy te sc ty tys in
         reduce (checkNaturalRange1 nTy i id)
      _ -> Nothing

  , primStepEntry $(textNameLit 'GHC.Num.Integer.integerToNaturalClamp) $ \case
      PrimStepContext{..}
        | [i] <- integerLiterals' args
        -> if i < 0 then
             reduce (naturalToNaturalLiteral 0)
           else
             reduce (naturalToNaturalLiteral (fromInteger i))
      _ -> Nothing

  , primStepEntry $(textNameLit 'GHC.Num.Integer.integerToNaturalThrow) $ \case
      PrimStepContext{..}
        | [i] <- integerLiterals' args
        -> let nTy = getResultTy te sc ty tys in
           reduce (checkNaturalRange1 nTy i id)
      _ -> Nothing

  , primStepEntry $(textNameLit 'GHC.Num.Integer.integerToInt64#) $ \case
      PrimStepContext{..}
        | [i] <- integerLiterals' args
        -> reduce (integerToInt64Literal i)
      _ -> Nothing

  , primStepEntry $(textNameLit 'GHC.Num.Integer.integerToWord64#) $ \case
      PrimStepContext{..}
        | [i] <- integerLiterals' args
        -> reduce (integerToWord64Literal i)
      _ -> Nothing

  , primStepEntry $(textNameLit 'GHC.Num.Integer.integerFromWord64#) $ \case
      PrimStepContext{..}
        | [w] <- word64Literals' args
        -> reduce (Literal (IntegerLiteral w))
      _ -> Nothing

  , primStepEntry $(textNameLit 'GHC.Num.Integer.integerSignum#) $ \case
      PrimStepContext{..}
        | [i] <- integerLiterals' args
        -> reduce (Literal (IntLiteral (signum i)))
      _ -> Nothing

  , primStepEntry $(textNameLit 'GHC.Num.Integer.integerSignum) $ \case
      PrimStepContext{..}
        | [i] <- integerLiterals' args
        -> reduce (Literal (IntegerLiteral (signumInteger i)))
      _ -> Nothing

  , primStepEntry "GHC.Num.Integer.$wintegerSignum" $ \case
      PrimStepContext{..}
        | [i] <- integerLiterals' args
        -> reduce (Literal (IntLiteral (signum i)))
      _ -> Nothing

  , primStepEntry $(textNameLit 'GHC.Num.Integer.integerAbs) $ \case
      PrimStepContext{..}
        | [i] <- integerLiterals' args
        -> reduce (Literal (IntegerLiteral (absInteger i)))
      _ -> Nothing

  , primStepEntry $(textNameLit 'GHC.Num.Integer.integerBit#) $ \case
      PrimStepContext{..}
        | [i] <- wordLiterals' args
        -> reduce (Literal (IntegerLiteral (bit (fromInteger i))))
      _ -> Nothing

  , primStepEntry $(textNameLit 'GHC.Num.Integer.integerComplement) $ \case
      PrimStepContext{..}
        | [i] <- integerLiterals' args
        -> reduce (Literal (IntegerLiteral (complementInteger i)))
      _ -> Nothing

  , primStepEntry $(textNameLit 'GHC.Num.Integer.integerOr) $ \case
      PrimStepContext{..}
        | [i, j] <- integerLiterals' args
        -> reduce (Literal (IntegerLiteral (orInteger i j)))
      _ -> Nothing

  , primStepEntry $(textNameLit 'GHC.Num.Integer.integerXor) $ \case
      PrimStepContext{..}
        | [i, j] <- integerLiterals' args
        -> reduce (Literal (IntegerLiteral (xorInteger i j)))
      _ -> Nothing

  , primStepEntry $(textNameLit 'GHC.Num.Integer.integerAnd) $ \case
      PrimStepContext{..}
        | [i, j] <- integerLiterals' args
        -> reduce (Literal (IntegerLiteral (andInteger i j)))
      _ -> Nothing

  -- On GHC >= 9.14 the bignum module moved into ghc-internal; the worker
  -- name follows.
  , primStepEntry "GHC.Num.Integer.$wintegerFromInt64#" $ \case
      PrimStepContext{..}
        | [i] <- int64Literals' args
        -> reduce . Literal $ IntLiteral i
      _ -> Nothing

  , primStepEntry "GHC.Internal.Bignum.Integer.$wintegerFromInt64#" $ \case
      PrimStepContext{..}
        | [i] <- int64Literals' args
        -> reduce . Literal $ IntLiteral i
      _ -> Nothing

  ]
