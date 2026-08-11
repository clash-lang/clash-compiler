{-|
  Copyright   :  (C) 2013-2016, University of Twente,
                     2016-2017, Myrtle Software Ltd,
                     2017-2022, Google Inc.,
                     2017-2026, QBayLogic B.V.
  License     :  BSD2 (see the file LICENSE)
  Maintainer  :  QBayLogic B.V. <devops@qbaylogic.com>
-}

{-# LANGUAGE CPP #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UnboxedTuples #-}

{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

#if MIN_VERSION_ghc(9,12,0)
-- We'll need to support deprecated primitives too
{-# OPTIONS_GHC -Wno-deprecations #-}
#endif

module Clash.GHC.Evaluator.Primitives.GHC.Prim
  ( primitives
  ) where

import           Data.Bits
import qualified Data.ByteString.Internal as BS
import           Data.Char           (chr,ord)
import qualified Data.List           as List
import qualified Data.Primitive.ByteArray as BA
import           Data.Text           (Text)
import           Data.Text.Extra     (showt)
import           GHC.Exts (IsList(..))
import           GHC.Float
import           GHC.Int
import           GHC.ForeignPtr
import           GHC.Prim
import           GHC.Types           (IO (..))
import           GHC.Word
import           System.IO.Unsafe    (unsafeDupablePerformIO)

import           Clash.Core.DataCon  (DataCon (..))
import           Clash.Core.Evaluator.Types
import           Clash.Core.Literal  (Literal (..))
import Clash.Core.Term
  (IsMultiPrim (..),
   PrimInfo (..),
   Term (..),
   WorkInfo (..),
   mkApps,
   PrimUnfolding(..))
import Clash.Core.Type
  (Type (..),
   ConstTy (..),
   TypeView (..),
   mkFunTy,
   splitFunForallTy,
   tyView)
import Clash.Core.TyCon (tyConDataCons)
import           Clash.Core.TysPrim
import qualified Clash.Data.UniqMap as UniqMap
import Clash.Util (textNameLit)

import Clash.GHC.Evaluator.Primitive.Util

primitives :: [(Text, PrimStep)]
primitives =
-----------------
-- GHC.Prim.Char#
-----------------
  [ primStepEntry $(textNameLit 'GHC.Prim.gtChar#) $ \case
      PrimStepContext{..} | Just (i,j) <- charLiterals args
        -> reduce (boolToIntLiteral (i > j))
      _ -> Nothing

  , primStepEntry $(textNameLit 'GHC.Prim.geChar#) $ \case
      PrimStepContext{..} | Just (i,j) <- charLiterals args
        -> reduce (boolToIntLiteral (i >= j))
      _ -> Nothing

  , primStepEntry $(textNameLit 'GHC.Prim.eqChar#) $ \case
      PrimStepContext{..} | Just (i,j) <- charLiterals args
        -> reduce (boolToIntLiteral (i == j))
      _ -> Nothing

  , primStepEntry $(textNameLit 'GHC.Prim.neChar#) $ \case
      PrimStepContext{..} | Just (i,j) <- charLiterals args
        -> reduce (boolToIntLiteral (i /= j))
      _ -> Nothing

  , primStepEntry $(textNameLit 'GHC.Prim.ltChar#) $ \case
      PrimStepContext{..} | Just (i,j) <- charLiterals args
        -> reduce (boolToIntLiteral (i < j))
      _ -> Nothing

  , primStepEntry $(textNameLit 'GHC.Prim.leChar#) $ \case
      PrimStepContext{..} | Just (i,j) <- charLiterals args
        -> reduce (boolToIntLiteral (i <= j))
      _ -> Nothing

  , primStepEntry $(textNameLit 'GHC.Prim.ord#) $ \case
      PrimStepContext{..} | [i] <- charLiterals' args
        -> reduce (integerToIntLiteral (toInteger $ ord i))
      _ -> Nothing


----------------
-- GHC.Prim.Int#
----------------
  , primStepEntry $(textNameLit '(GHC.Prim.+#)) $ \case
      PrimStepContext{..} | Just (i,j) <- intLiterals args
        -> reduce (integerToIntLiteral (i+j))
      _ -> Nothing

  , primStepEntry $(textNameLit '(GHC.Prim.-#)) $ \case
      PrimStepContext{..} | Just (i,j) <- intLiterals args
        -> reduce (integerToIntLiteral (i-j))
      _ -> Nothing

  , primStepEntry $(textNameLit '(GHC.Prim.*#)) $ \case
      PrimStepContext{..} | Just (i,j) <- intLiterals args
        -> reduce (integerToIntLiteral (i*j))
      _ -> Nothing

  , primStepEntry $(textNameLit 'GHC.Prim.mulIntMayOflo#) $ \case
      PrimStepContext{..} | Just (i,j) <- intLiterals  args
        -> let !(I# a)  = fromInteger i
               !(I# b)  = fromInteger j
               c :: Int#
               c = mulIntMayOflo# a b
           in  reduce (integerToIntLiteral (toInteger $ I# c))
      _ -> Nothing

  , primStepEntry $(textNameLit 'GHC.Prim.quotInt#) $ \case
      PrimStepContext{..} | Just (i,j) <- intLiterals args
        -> reduce $ catchDivByZero (integerToIntLiteral (i `quot` j))
      _ -> Nothing

  , primStepEntry $(textNameLit 'GHC.Prim.remInt#) $ \case
      PrimStepContext{..} | Just (i,j) <- intLiterals args
        -> reduce $ catchDivByZero (integerToIntLiteral (i `rem` j))
      _ -> Nothing

  , primStepEntry $(textNameLit 'GHC.Prim.quotRemInt#) $ \case
      PrimStepContext{..} | Just (i,j) <- intLiterals args
        -> let (_,tyView -> TyConApp tupTcNm tyArgs) = splitFunForallTy ty
               (Just tupTc) = UniqMap.lookup tupTcNm tcm
               [tupDc] = tyConDataCons tupTc
               (q,r)   = quotRem i j
               ret     = mkApps (Data tupDc) (map Right tyArgs ++
                        [Left $ catchDivByZero (integerToIntLiteral q)
                        ,Left $ catchDivByZero (integerToIntLiteral r)])
           in  reduce ret
      _ -> Nothing

  , primStepEntry $(textNameLit 'GHC.Prim.andI#) $ \case
      PrimStepContext{..} | Just (i,j) <- intLiterals args
        -> reduce (integerToIntLiteral (i .&. j))
      _ -> Nothing

  , primStepEntry $(textNameLit 'GHC.Prim.orI#) $ \case
      PrimStepContext{..} | Just (i,j) <- intLiterals args
        -> reduce (integerToIntLiteral (i .|. j))
      _ -> Nothing

  , primStepEntry $(textNameLit 'GHC.Prim.xorI#) $ \case
      PrimStepContext{..} | Just (i,j) <- intLiterals args
        -> reduce (integerToIntLiteral (i `xor` j))
      _ -> Nothing

  , primStepEntry $(textNameLit 'GHC.Prim.notI#) $ \case
      PrimStepContext{..} | [i] <- intLiterals' args
        -> reduce (integerToIntLiteral (complement i))
      _ -> Nothing

  , primStepEntry $(textNameLit 'GHC.Prim.negateInt#) $ \case
      PrimStepContext{..}
        | [Lit (IntLiteral i)] <- args
        -> reduce (integerToIntLiteral (negate i))
      _ -> Nothing

  , primStepEntry $(textNameLit 'GHC.Prim.addIntC#) $ \case
      PrimStepContext{..} | Just (i,j) <- intLiterals args
        -> let (_,tyView -> TyConApp tupTcNm tyArgs) = splitFunForallTy ty
               (Just tupTc) = UniqMap.lookup tupTcNm tcm
               [tupDc] = tyConDataCons tupTc
               !(I# a)  = fromInteger i
               !(I# b)  = fromInteger j
               !(# d, c #) = addIntC# a b
           in  reduce $
               mkApps (Data tupDc) (map Right tyArgs ++
                       [ Left (Literal . IntLiteral . toInteger $ I# d)
                       , Left (Literal . IntLiteral . toInteger $ I# c)])
      _ -> Nothing

  , primStepEntry $(textNameLit 'GHC.Prim.subIntC#) $ \case
      PrimStepContext{..} | Just (i,j) <- intLiterals args
        -> let (_,tyView -> TyConApp tupTcNm tyArgs) = splitFunForallTy ty
               (Just tupTc) = UniqMap.lookup tupTcNm tcm
               [tupDc] = tyConDataCons tupTc
               !(I# a)  = fromInteger i
               !(I# b)  = fromInteger j
               !(# d, c #) = subIntC# a b
           in  reduce $
               mkApps (Data tupDc) (map Right tyArgs ++
                       [ Left (Literal . IntLiteral . toInteger $ I# d)
                       , Left (Literal . IntLiteral . toInteger $ I# c)])
      _ -> Nothing

  , primStepEntry $(textNameLit '(GHC.Prim.>#)) $ \case
      PrimStepContext{..} | Just (i,j) <- intLiterals args
        -> reduce (boolToIntLiteral (i > j))
      _ -> Nothing

  , primStepEntry $(textNameLit '(GHC.Prim.>=#)) $ \case
      PrimStepContext{..} | Just (i,j) <- intLiterals args
        -> reduce (boolToIntLiteral (i >= j))
      _ -> Nothing

  , primStepEntry $(textNameLit '(GHC.Prim.==#)) $ \case
      PrimStepContext{..} | Just (i,j) <- intLiterals args
        -> reduce (boolToIntLiteral (i == j))
      _ -> Nothing

  , primStepEntry $(textNameLit '(GHC.Prim./=#)) $ \case
      PrimStepContext{..} | Just (i,j) <- intLiterals args
        -> reduce (boolToIntLiteral (i /= j))
      _ -> Nothing

  , primStepEntry $(textNameLit '(GHC.Prim.<#)) $ \case
      PrimStepContext{..}| Just (i,j) <- intLiterals args
        -> reduce (boolToIntLiteral (i < j))
      _ -> Nothing

  , primStepEntry $(textNameLit '(GHC.Prim.<=#)) $ \case
      PrimStepContext{..} | Just (i,j) <- intLiterals args
        -> reduce (boolToIntLiteral (i <= j))
      _ -> Nothing

  , primStepEntry $(textNameLit 'GHC.Prim.chr#) $ \case
      PrimStepContext{..} | [i] <- intLiterals' args
        -> reduce (catchErrorCall (charToCharLiteral (chr $ fromInteger i)))
      _ -> Nothing

  , primStepEntry $(textNameLit 'GHC.Prim.int2Word#) $ \case
      PrimStepContext{..}
        | [Lit (IntLiteral i)] <- args
        -> reduce . Literal . WordLiteral . toInteger $ (fromInteger :: Integer -> Word) i -- for overflow behavior
      _ -> Nothing

  , primStepEntry $(textNameLit 'GHC.Prim.int2Float#) $ \case
      PrimStepContext{..}
        | [Lit (IntLiteral i)] <- args
        -> reduce . Literal . FloatLiteral  . castFloatToWord32 $ fromInteger i
      _ -> Nothing

  , primStepEntry $(textNameLit 'GHC.Prim.int2Double#) $ \case
      PrimStepContext{..}
        | [Lit (IntLiteral i)] <- args
        -> reduce . Literal . DoubleLiteral . castDoubleToWord64 $ fromInteger i
      _ -> Nothing

  , primStepEntry $(textNameLit 'GHC.Prim.word2Float#) $ \case
      PrimStepContext{..}
        | [Lit (WordLiteral i)] <- args
        -> reduce . Literal . FloatLiteral  . castFloatToWord32 $ fromInteger i
      _ -> Nothing

  , primStepEntry $(textNameLit 'GHC.Prim.word2Double#) $ \case
      PrimStepContext{..}
        | [Lit (WordLiteral i)] <- args
        -> reduce . Literal . DoubleLiteral . castDoubleToWord64 $ fromInteger i
      _ -> Nothing

  , primStepEntry $(textNameLit 'GHC.Prim.uncheckedIShiftL#) $ \case
      PrimStepContext{..}
        | [ Lit (IntLiteral i)
          , Lit (IntLiteral s)
          ] <- args
        -> reduce (integerToIntLiteral (i `shiftL` fromInteger s))
      _ -> Nothing

  , primStepEntry $(textNameLit 'GHC.Prim.uncheckedIShiftRA#) $ \case
      PrimStepContext{..}
        | [ Lit (IntLiteral i)
          , Lit (IntLiteral s)
          ] <- args
        -> reduce (integerToIntLiteral (i `shiftR` fromInteger s))
      _ -> Nothing

  , primStepEntry $(textNameLit 'GHC.Prim.uncheckedIShiftRL#) $ \case
      PrimStepContext{..} | Just (i,j) <- intLiterals args
        -> let !(I# a)  = fromInteger i
               !(I# b)  = fromInteger j
               c :: Int#
               c = uncheckedIShiftRL# a b
           in  reduce (integerToIntLiteral (toInteger $ I# c))
      _ -> Nothing


-----------------
-- GHC.Prim.Word#
-----------------
  , primStepEntry $(textNameLit 'GHC.Prim.plusWord#) $ \case
      PrimStepContext{..} | Just (i,j) <- wordLiterals args
        -> reduce (integerToWordLiteral (i+j))
      _ -> Nothing

  , primStepEntry $(textNameLit 'GHC.Prim.subWordC#) $ \case
      PrimStepContext{..} | Just (i,j) <- wordLiterals args
        -> let (_,tyView -> TyConApp tupTcNm tyArgs) = splitFunForallTy ty
               (Just tupTc) = UniqMap.lookup tupTcNm tcm
               [tupDc] = tyConDataCons tupTc
               !(W# a)  = fromInteger i
               !(W# b)  = fromInteger j
               !(# d, c #) = subWordC# a b
           in  reduce $
               mkApps (Data tupDc) (map Right tyArgs ++
                       [ Left (Literal . WordLiteral . toInteger $ W# d)
                       , Left (Literal . IntLiteral . toInteger $ I# c)])
      _ -> Nothing

  , primStepEntry $(textNameLit 'GHC.Prim.plusWord2#) $ \case
      PrimStepContext{..} | Just (i,j) <- wordLiterals args
        -> let (_,tyView -> TyConApp tupTcNm tyArgs) = splitFunForallTy ty
               (Just tupTc) = UniqMap.lookup tupTcNm tcm
               [tupDc] = tyConDataCons tupTc
               !(W# a)  = fromInteger i
               !(W# b)  = fromInteger j
               !(# h', l #) = plusWord2# a b
           in  reduce $
               mkApps (Data tupDc) (map Right tyArgs ++
                       [ Left (Literal . WordLiteral . toInteger $ W# h')
                       , Left (Literal . WordLiteral . toInteger $ W# l)])
      _ -> Nothing

  , primStepEntry $(textNameLit 'GHC.Prim.minusWord#) $ \case
      PrimStepContext{..} | Just (i,j) <- wordLiterals args
        -> reduce (integerToWordLiteral (i-j))
      _ -> Nothing

  , primStepEntry $(textNameLit 'GHC.Prim.timesWord#) $ \case
      PrimStepContext{..} | Just (i,j) <- wordLiterals args
        -> reduce (integerToWordLiteral (i*j))
      _ -> Nothing

  , primStepEntry $(textNameLit 'GHC.Prim.timesWord2#) $ \case
      PrimStepContext{..} | Just (i,j) <- wordLiterals args
        -> let (_,tyView -> TyConApp tupTcNm tyArgs) = splitFunForallTy ty
               (Just tupTc) = UniqMap.lookup tupTcNm tcm
               [tupDc] = tyConDataCons tupTc
               !(W# a)  = fromInteger i
               !(W# b)  = fromInteger j
               !(# h', l #) = timesWord2# a b
           in  reduce $
               mkApps (Data tupDc) (map Right tyArgs ++
                       [ Left (Literal . WordLiteral . toInteger $ W# h')
                       , Left (Literal . WordLiteral . toInteger $ W# l)])
      _ -> Nothing

  , primStepEntry $(textNameLit 'GHC.Prim.quotWord#) $ \case
      PrimStepContext{..} | Just (i,j) <- wordLiterals args
        -> reduce $ catchDivByZero (integerToWordLiteral (i `quot` j))
      _ -> Nothing

  , primStepEntry $(textNameLit 'GHC.Prim.remWord#) $ \case
      PrimStepContext{..} | Just (i,j) <- wordLiterals args
        -> reduce $ catchDivByZero (integerToWordLiteral (i `rem` j))
      _ -> Nothing

  , primStepEntry $(textNameLit 'GHC.Prim.quotRemWord#) $ \case
      PrimStepContext{..} | Just (i,j) <- wordLiterals args
        -> let (_,tyView -> TyConApp tupTcNm tyArgs) = splitFunForallTy ty
               (Just tupTc) = UniqMap.lookup tupTcNm tcm
               [tupDc] = tyConDataCons tupTc
               (q,r)   = quotRem i j
               ret     = mkApps (Data tupDc) (map Right tyArgs ++
                        [Left $ catchDivByZero (integerToWordLiteral q)
                        ,Left $ catchDivByZero (integerToWordLiteral r)])
           in  reduce ret
      _ -> Nothing

  , primStepEntry $(textNameLit 'GHC.Prim.quotRemWord2#) $ \case
      PrimStepContext{..} | [i,j,k'] <- wordLiterals' args
        -> let (_,tyView -> TyConApp tupTcNm tyArgs) = splitFunForallTy ty
               (Just tupTc) = UniqMap.lookup tupTcNm tcm
               [tupDc] = tyConDataCons tupTc
               !(W# a)  = fromInteger i
               !(W# b)  = fromInteger j
               !(W# c)  = fromInteger k'
               !(# x, y #) = quotRemWord2# a b c
           in  reduce $
               mkApps (Data tupDc) (map Right tyArgs ++
                       [ Left $ catchDivByZero (Literal . WordLiteral . toInteger $ W# x)
                       , Left $ catchDivByZero (Literal . WordLiteral . toInteger $ W# y)])
      _ -> Nothing

  , primStepEntry $(textNameLit 'GHC.Prim.and#) $ \case
      PrimStepContext{..} | Just (i,j) <- wordLiterals args
        -> reduce (integerToWordLiteral (i .&. j))
      _ -> Nothing

  , primStepEntry $(textNameLit 'GHC.Prim.or#) $ \case
      PrimStepContext{..} | Just (i,j) <- wordLiterals args
        -> reduce (integerToWordLiteral (i .|. j))
      _ -> Nothing

  , primStepEntry $(textNameLit 'GHC.Prim.xor#) $ \case
      PrimStepContext{..} | Just (i,j) <- wordLiterals args
        -> reduce (integerToWordLiteral (i `xor` j))
      _ -> Nothing

  , primStepEntry $(textNameLit 'GHC.Prim.not#) $ \case
      PrimStepContext{..} | [i] <- wordLiterals' args
        -> reduce (integerToWordLiteral (complement i))
      _ -> Nothing

  , primStepEntry $(textNameLit 'GHC.Prim.uncheckedShiftL#) $ \case
      PrimStepContext{..}
        | [ Lit (WordLiteral w)
          , Lit (IntLiteral  i)
          ] <- args
        -> reduce (Literal (WordLiteral (w `shiftL` fromInteger i)))
      _ -> Nothing

  , primStepEntry $(textNameLit 'GHC.Prim.uncheckedShiftRL#) $ \case
      PrimStepContext{..}
        | [ Lit (WordLiteral w)
          , Lit (IntLiteral  i)
          ] <- args
        -> reduce (Literal (WordLiteral (w `shiftR` fromInteger i)))
      _ -> Nothing

  , primStepEntry $(textNameLit 'GHC.Prim.word2Int#) $ \case
      PrimStepContext{..}
        | [Lit (WordLiteral i)] <- args
        -> reduce . Literal . IntLiteral . toInteger $ (fromInteger :: Integer -> Int) i -- for overflow behavior
      _ -> Nothing

  , primStepEntry $(textNameLit 'GHC.Prim.gtWord#) $ \case
      PrimStepContext{..} | Just (i,j) <- wordLiterals args
        -> reduce (boolToIntLiteral (i > j))
      _ -> Nothing

  , primStepEntry $(textNameLit 'GHC.Prim.geWord#) $ \case
      PrimStepContext{..} | Just (i,j) <- wordLiterals args
        -> reduce (boolToIntLiteral (i >= j))
      _ -> Nothing

  , primStepEntry $(textNameLit 'GHC.Prim.eqWord#) $ \case
      PrimStepContext{..} | Just (i,j) <- wordLiterals args
        -> reduce (boolToIntLiteral (i == j))
      _ -> Nothing

  , primStepEntry $(textNameLit 'GHC.Prim.neWord#) $ \case
      PrimStepContext{..} | Just (i,j) <- wordLiterals args
        -> reduce (boolToIntLiteral (i /= j))
      _ -> Nothing

  , primStepEntry $(textNameLit 'GHC.Prim.ltWord#) $ \case
      PrimStepContext{..} | Just (i,j) <- wordLiterals args
        -> reduce (boolToIntLiteral (i < j))
      _ -> Nothing

  , primStepEntry $(textNameLit 'GHC.Prim.leWord#) $ \case
      PrimStepContext{..} | Just (i,j) <- wordLiterals args
        -> reduce (boolToIntLiteral (i <= j))
      _ -> Nothing

  , primStepEntry $(textNameLit 'GHC.Prim.popCnt8#) $ \case
      PrimStepContext{..} | [i] <- wordLiterals' args
        -> reduce . integerToWordLiteral . toInteger . popCount . (fromInteger :: Integer -> Word8) $ i
      _ -> Nothing

  , primStepEntry $(textNameLit 'GHC.Prim.popCnt16#) $ \case
      PrimStepContext{..} | [i] <- wordLiterals' args
        -> reduce . integerToWordLiteral . toInteger . popCount . (fromInteger :: Integer -> Word16) $ i
      _ -> Nothing

  , primStepEntry $(textNameLit 'GHC.Prim.popCnt32#) $ \case
      PrimStepContext{..} | [i] <- wordLiterals' args
        -> reduce . integerToWordLiteral . toInteger . popCount . (fromInteger :: Integer -> Word32) $ i
      _ -> Nothing

  , primStepEntry $(textNameLit 'GHC.Prim.popCnt64#) $ \case
      PrimStepContext{..} | [i] <- word64Literals' args
        -> reduce . integerToWordLiteral . toInteger . popCount . (fromInteger :: Integer -> Word64) $ i
      _ -> Nothing

  , primStepEntry $(textNameLit 'GHC.Prim.popCnt#) $ \case
      PrimStepContext{..} | [i] <- wordLiterals' args
        -> reduce . integerToWordLiteral . toInteger . popCount . (fromInteger :: Integer -> Word) $ i
      _ -> Nothing

  , primStepEntry $(textNameLit 'GHC.Prim.clz8#) $ \case
      PrimStepContext{..} | [i] <- wordLiterals' args
        -> reduce . integerToWordLiteral . toInteger . countLeadingZeros . (fromInteger :: Integer -> Word8) $ i
      _ -> Nothing

  , primStepEntry $(textNameLit 'GHC.Prim.clz16#) $ \case
      PrimStepContext{..} | [i] <- wordLiterals' args
        -> reduce . integerToWordLiteral . toInteger . countLeadingZeros . (fromInteger :: Integer -> Word16) $ i
      _ -> Nothing

  , primStepEntry $(textNameLit 'GHC.Prim.clz32#) $ \case
      PrimStepContext{..} | [i] <- wordLiterals' args
        -> reduce . integerToWordLiteral . toInteger . countLeadingZeros . (fromInteger :: Integer -> Word32) $ i
      _ -> Nothing

  , primStepEntry $(textNameLit 'GHC.Prim.clz64#) $ \case
      PrimStepContext{..} | [i] <- word64Literals' args
        -> reduce . integerToWordLiteral . toInteger . countLeadingZeros . (fromInteger :: Integer -> Word64) $ i
      _ -> Nothing

  , primStepEntry $(textNameLit 'GHC.Prim.clz#) $ \case
      PrimStepContext{..} | [i] <- wordLiterals' args
        -> reduce . integerToWordLiteral . toInteger . countLeadingZeros . (fromInteger :: Integer -> Word) $ i
      _ -> Nothing

  , primStepEntry $(textNameLit 'GHC.Prim.ctz8#) $ \case
      PrimStepContext{..} | [i] <- wordLiterals' args
        -> reduce . integerToWordLiteral . toInteger . countTrailingZeros . (fromInteger :: Integer -> Word) $ i .&. (bit 8 - 1)
      _ -> Nothing

  , primStepEntry $(textNameLit 'GHC.Prim.ctz16#) $ \case
      PrimStepContext{..} | [i] <- wordLiterals' args
        -> reduce . integerToWordLiteral . toInteger . countTrailingZeros . (fromInteger :: Integer -> Word) $ i .&. (bit 16 - 1)
      _ -> Nothing

  , primStepEntry $(textNameLit 'GHC.Prim.ctz32#) $ \case
      PrimStepContext{..} | [i] <- wordLiterals' args
        -> reduce . integerToWordLiteral . toInteger . countTrailingZeros . (fromInteger :: Integer -> Word) $ i .&. (bit 32 - 1)
      _ -> Nothing

  , primStepEntry $(textNameLit 'GHC.Prim.ctz64#) $ \case
      PrimStepContext{..} | [i] <- word64Literals' args
        -> reduce . integerToWordLiteral . toInteger . countTrailingZeros . (fromInteger :: Integer -> Word64) $ i .&. (bit 64 - 1)
      _ -> Nothing

  , primStepEntry $(textNameLit 'GHC.Prim.ctz#) $ \case
      PrimStepContext{..} | [i] <- wordLiterals' args
        -> reduce . integerToWordLiteral . toInteger . countTrailingZeros . (fromInteger :: Integer -> Word) $ i
      _ -> Nothing

  , primStepEntry $(textNameLit 'GHC.Prim.byteSwap16#) $ \case
      PrimStepContext{..} | [i] <- wordLiterals' args
        -> reduce . integerToWordLiteral . toInteger . byteSwap16 . (fromInteger :: Integer -> Word16) $ i
      _ -> Nothing

  , primStepEntry $(textNameLit 'GHC.Prim.byteSwap32#) $ \case
      PrimStepContext{..} | [i] <- wordLiterals' args
        -> reduce . integerToWordLiteral . toInteger . byteSwap32 . (fromInteger :: Integer -> Word32) $ i
      _ -> Nothing

  , primStepEntry $(textNameLit 'GHC.Prim.byteSwap64#) $ \case
      PrimStepContext{..} | [i] <- word64Literals' args
        -> reduce . integerToWordLiteral . toInteger . byteSwap64 . (fromInteger :: Integer -> Word64) $ i
      _ -> Nothing

  , primStepEntry $(textNameLit 'GHC.Prim.byteSwap#) $ \case
      PrimStepContext{..} | [i] <- wordLiterals' args -- assume 64bits
        -> reduce . integerToWordLiteral . toInteger . byteSwap64 . (fromInteger :: Integer -> Word64) $ i
      _ -> Nothing

  , primStepEntry $(textNameLit 'GHC.Prim.bitReverse#) $ \case
      PrimStepContext{..} | [i] <- wordLiterals' args
        -> reduce . integerToWordLiteral . toInteger . bitReverse64 . fromInteger $ i -- assume 64bits
      _ -> Nothing

  , primStepEntry $(textNameLit 'GHC.Prim.bitReverse8#) $ \case
      PrimStepContext{..} | [i] <- wordLiterals' args
        -> reduce . integerToWordLiteral . toInteger . bitReverse8 . fromInteger $ i
      _ -> Nothing

  , primStepEntry $(textNameLit 'GHC.Prim.bitReverse16#) $ \case
      PrimStepContext{..} | [i] <- wordLiterals' args
        -> reduce . integerToWordLiteral . toInteger . bitReverse16 . fromInteger $ i
      _ -> Nothing

  , primStepEntry $(textNameLit 'GHC.Prim.bitReverse32#) $ \case
      PrimStepContext{..} | [i] <- wordLiterals' args
        -> reduce . integerToWordLiteral . toInteger . bitReverse32 . fromInteger $ i
      _ -> Nothing

  , primStepEntry $(textNameLit 'GHC.Prim.bitReverse64#) $ \case
      PrimStepContext{..} | [i] <- word64Literals' args
        -> reduce . integerToWordLiteral . toInteger . bitReverse64 . fromInteger $ i
      _ -> Nothing

------------
-- Narrowing
------------
  , primStepEntry $(textNameLit 'GHC.Prim.narrow8Int#) $ \case
      PrimStepContext{..} | [i] <- intLiterals' args
        -> let !(I# a)  = fromInteger i
               b = narrow8Int# a
           in  reduce . Literal . IntLiteral . toInteger $ I# b
      _ -> Nothing

  , primStepEntry $(textNameLit 'GHC.Prim.narrow16Int#) $ \case
      PrimStepContext{..} | [i] <- intLiterals' args
        -> let !(I# a)  = fromInteger i
               b = narrow16Int# a
           in  reduce . Literal . IntLiteral . toInteger $ I# b
      _ -> Nothing

  , primStepEntry $(textNameLit 'GHC.Prim.narrow32Int#) $ \case
      PrimStepContext{..} | [i] <- intLiterals' args
        -> let !(I# a)  = fromInteger i
               b = narrow32Int# a
           in  reduce . Literal . IntLiteral . toInteger $ I# b
      _ -> Nothing

  , primStepEntry $(textNameLit 'GHC.Prim.narrow8Word#) $ \case
      PrimStepContext{..} | [i] <- wordLiterals' args
        -> let !(W# a)  = fromInteger i
               b = narrow8Word# a
           in  reduce . Literal . WordLiteral . toInteger $ W# b
      _ -> Nothing

  , primStepEntry $(textNameLit 'GHC.Prim.narrow16Word#) $ \case
      PrimStepContext{..} | [i] <- wordLiterals' args
        -> let !(W# a)  = fromInteger i
               b = narrow16Word# a
           in  reduce . Literal . WordLiteral . toInteger $ W# b
      _ -> Nothing

  , primStepEntry $(textNameLit 'GHC.Prim.narrow32Word#) $ \case
      PrimStepContext{..} | [i] <- wordLiterals' args
        -> let !(W# a)  = fromInteger i
               b = narrow32Word# a
           in  reduce . Literal . WordLiteral . toInteger $ W# b
      _ -> Nothing


--------
-- Int8#
--------
  , primStepEntry $(textNameLit 'GHC.Prim.intToInt8#) $ \case
      PrimStepContext{..} | [i] <- intLiterals' args
        -> let !(I# a)  = fromInteger i
               b = narrow8Int# a
           in  reduce . Literal . Int8Literal . toInteger $ I# b
      _ -> Nothing

  , primStepEntry $(textNameLit 'GHC.Prim.int8ToInt#) $ \case
      PrimStepContext{..} | [i] <- int8Literals' args
        -> reduce . Literal $ IntLiteral i
      _ -> Nothing

  -- XXX: Primitive does not exist?
  , primStepEntry "GHC.Prim.negateInt8" $ \case
      PrimStepContext{..} | [i] <- int8Literals' args
        -> let !(I8# a) = fromInteger i
            in reduce (Literal (Int8Literal (toInteger (I8# (negateInt8# a)))))
      _ -> Nothing

  , primStepEntry $(textNameLit 'GHC.Prim.plusInt8#) $ \case
      PrimStepContext{..} | Just r <- liftI8 plusInt8# args
        -> reduce r
      _ -> Nothing

  , primStepEntry $(textNameLit 'GHC.Prim.subInt8#) $ \case
      PrimStepContext{..} | Just r <- liftI8 subInt8# args
        -> reduce r
      _ -> Nothing

  , primStepEntry $(textNameLit 'GHC.Prim.timesInt8#) $ \case
      PrimStepContext{..} | Just r <- liftI8 timesInt8# args
        -> reduce r
      _ -> Nothing

  , primStepEntry $(textNameLit 'GHC.Prim.quotInt8#) $ \case
      PrimStepContext{..} | [i, j] <- int8Literals' args
        -> reduce $ catchDivByZero
             (Literal (Int8Literal (toInteger (fromInteger i `quot` fromInteger j :: Int8))))
      _ -> Nothing

  , primStepEntry $(textNameLit 'GHC.Prim.remInt8#) $ \case
      PrimStepContext{..} | [i, j] <- int8Literals' args
        -> reduce $ catchDivByZero
             (Literal (Int8Literal (toInteger (fromInteger i `rem` fromInteger j :: Int8))))
      _ -> Nothing

  , primStepEntry $(textNameLit 'GHC.Prim.quotRemInt8#) $ \case
      PrimStepContext{..}
        | [i, j] <- int8Literals' args
        , (_,tyView -> TyConApp tupTcNm tyArgs) <- splitFunForallTy ty
        , (Just tupTc) <- UniqMap.lookup tupTcNm tcm
        , [tupDc] <- tyConDataCons tupTc
        -> let (q,r) = quotRem (fromInteger i :: Int8) (fromInteger j)
           in reduce $ mkApps (Data tupDc) (map Right tyArgs ++
                      [ Left $ catchDivByZero (Literal (Int8Literal (toInteger q)))
                      , Left $ catchDivByZero (Literal (Int8Literal (toInteger r)))])
      _ -> Nothing

  , primStepEntry $(textNameLit 'GHC.Prim.uncheckedShiftLInt8#) $ \case
      PrimStepContext{..} | Just r <- liftI8I uncheckedShiftLInt8# args
        -> reduce r
      _ -> Nothing

  , primStepEntry $(textNameLit 'GHC.Prim.uncheckedShiftRAInt8#) $ \case
      PrimStepContext{..} | Just r <- liftI8I uncheckedShiftRAInt8# args
        -> reduce r
      _ -> Nothing

  , primStepEntry $(textNameLit 'GHC.Prim.uncheckedShiftRLInt8#) $ \case
      PrimStepContext{..} | Just r <- liftI8I uncheckedShiftRLInt8# args
        -> reduce r
      _ -> Nothing

  , primStepEntry $(textNameLit 'GHC.Prim.int8ToWord8#) $ \case
      PrimStepContext{..} | [i] <- int8Literals' args
        -> let !(I8# a) = fromInteger i
            in reduce (Literal (Word8Literal (toInteger (W8# (int8ToWord8# a)))))
      _ -> Nothing

  , primStepEntry $(textNameLit 'GHC.Prim.eqInt8#) $ \case
      PrimStepContext{..} | Just r <- liftI8RI eqInt8# args
        -> reduce r
      _ -> Nothing

  , primStepEntry $(textNameLit 'GHC.Prim.geInt8#) $ \case
      PrimStepContext{..} | Just r <- liftI8RI geInt8# args
        -> reduce r
      _ -> Nothing

  , primStepEntry $(textNameLit 'GHC.Prim.gtInt8#) $ \case
      PrimStepContext{..} | Just r <- liftI8RI gtInt8# args
        -> reduce r
      _ -> Nothing

  , primStepEntry $(textNameLit 'GHC.Prim.leInt8#) $ \case
      PrimStepContext{..} | Just r <- liftI8RI leInt8# args
        -> reduce r
      _ -> Nothing

  , primStepEntry $(textNameLit 'GHC.Prim.ltInt8#) $ \case
      PrimStepContext{..} | Just r <- liftI8RI ltInt8# args
        -> reduce r
      _ -> Nothing

  , primStepEntry $(textNameLit 'GHC.Prim.neInt8#) $ \case
      PrimStepContext{..} | Just r <- liftI8RI neInt8# args
        -> reduce r
      _ -> Nothing


---------
-- Int16#
---------
  , primStepEntry $(textNameLit 'GHC.Prim.intToInt16#) $ \case
      PrimStepContext{..} | [i] <- intLiterals' args
        -> let !(I# a)  = fromInteger i
               b = narrow16Int# a
           in  reduce . Literal . Int16Literal . toInteger $ I# b
      _ -> Nothing

  , primStepEntry $(textNameLit 'GHC.Prim.int16ToInt#) $ \case
      PrimStepContext{..} | [i] <- int16Literals' args
        -> reduce . Literal $ IntLiteral i
      _ -> Nothing

  -- XXX: Primitive does not exist?
  , primStepEntry "GHC.Prim.negateInt16" $ \case
      PrimStepContext{..} | [i] <- int16Literals' args
        -> let !(I16# a) = fromInteger i
            in reduce (Literal (Int16Literal (toInteger (I16# (negateInt16# a)))))
      _ -> Nothing

  , primStepEntry $(textNameLit 'GHC.Prim.plusInt16#) $ \case
      PrimStepContext{..} | Just r <- liftI16 plusInt16# args
        -> reduce r
      _ -> Nothing

  , primStepEntry $(textNameLit 'GHC.Prim.subInt16#) $ \case
      PrimStepContext{..} | Just r <- liftI16 subInt16# args
        -> reduce r
      _ -> Nothing

  , primStepEntry $(textNameLit 'GHC.Prim.timesInt16#) $ \case
      PrimStepContext{..} | Just r <- liftI16 timesInt16# args
        -> reduce r
      _ -> Nothing

  , primStepEntry $(textNameLit 'GHC.Prim.quotInt16#) $ \case
      PrimStepContext{..} | [i, j] <- int16Literals' args
        -> reduce $ catchDivByZero
             (Literal (Int16Literal (toInteger (fromInteger i `quot` fromInteger j :: Int16))))
      _ -> Nothing

  , primStepEntry $(textNameLit 'GHC.Prim.remInt16#) $ \case
      PrimStepContext{..} | [i, j] <- int16Literals' args
        -> reduce $ catchDivByZero
             (Literal (Int16Literal (toInteger (fromInteger i `rem` fromInteger j :: Int16))))
      _ -> Nothing

  , primStepEntry $(textNameLit 'GHC.Prim.quotRemInt16#) $ \case
      PrimStepContext{..}
        | [i, j] <- int16Literals' args
        , (_,tyView -> TyConApp tupTcNm tyArgs) <- splitFunForallTy ty
        , (Just tupTc) <- UniqMap.lookup tupTcNm tcm
        , [tupDc] <- tyConDataCons tupTc
        -> let (q,r) = quotRem (fromInteger i :: Int16) (fromInteger j)
           in reduce $ mkApps (Data tupDc) (map Right tyArgs ++
                      [ Left $ catchDivByZero (Literal (Int16Literal (toInteger q)))
                      , Left $ catchDivByZero (Literal (Int16Literal (toInteger r)))])
      _ -> Nothing

  , primStepEntry $(textNameLit 'GHC.Prim.uncheckedShiftLInt16#) $ \case
      PrimStepContext{..} | Just r <- liftI16I uncheckedShiftLInt16# args
        -> reduce r
      _ -> Nothing

  , primStepEntry $(textNameLit 'GHC.Prim.uncheckedShiftRAInt16#) $ \case
      PrimStepContext{..} | Just r <- liftI16I uncheckedShiftRAInt16# args
        -> reduce r
      _ -> Nothing

  , primStepEntry $(textNameLit 'GHC.Prim.uncheckedShiftRLInt16#) $ \case
      PrimStepContext{..} | Just r <- liftI16I uncheckedShiftRLInt16# args
        -> reduce r
      _ -> Nothing

  , primStepEntry $(textNameLit 'GHC.Prim.int16ToWord16#) $ \case
      PrimStepContext{..} | [i] <- int16Literals' args
        -> let !(I16# a) = fromInteger i
            in reduce (Literal (Word16Literal (toInteger (W16# (int16ToWord16# a)))))
      _ -> Nothing

  , primStepEntry $(textNameLit 'GHC.Prim.eqInt16#) $ \case
      PrimStepContext{..} | Just r <- liftI16RI eqInt16# args
        -> reduce r
      _ -> Nothing

  , primStepEntry $(textNameLit 'GHC.Prim.geInt16#) $ \case
      PrimStepContext{..} | Just r <- liftI16RI geInt16# args
        -> reduce r
      _ -> Nothing

  , primStepEntry $(textNameLit 'GHC.Prim.gtInt16#) $ \case
      PrimStepContext{..} | Just r <- liftI16RI gtInt16# args
        -> reduce r
      _ -> Nothing

  , primStepEntry $(textNameLit 'GHC.Prim.leInt16#) $ \case
      PrimStepContext{..} | Just r <- liftI16RI leInt16# args
        -> reduce r
      _ -> Nothing

  , primStepEntry $(textNameLit 'GHC.Prim.ltInt16#) $ \case
      PrimStepContext{..} | Just r <- liftI16RI ltInt16# args
        -> reduce r
      _ -> Nothing

  , primStepEntry $(textNameLit 'GHC.Prim.neInt16#) $ \case
      PrimStepContext{..} | Just r <- liftI16RI neInt16# args
        -> reduce r
      _ -> Nothing


---------
-- Int32#
---------
  , primStepEntry $(textNameLit 'GHC.Prim.intToInt32#) $ \case
      PrimStepContext{..} | [i] <- intLiterals' args
        -> let !(I# a)  = fromInteger i
               b = narrow32Int# a
           in  reduce . Literal . Int32Literal . toInteger $ I# b
      _ -> Nothing

  , primStepEntry $(textNameLit 'GHC.Prim.int32ToInt#) $ \case
      PrimStepContext{..} | [i] <- int32Literals' args
        -> reduce . Literal $ IntLiteral i
      _ -> Nothing

  -- XXX: Primitive does not exist?
  , primStepEntry "GHC.Prim.negateInt32" $ \case
      PrimStepContext{..} | [i] <- int32Literals' args
        -> let !(I32# a) = fromInteger i
            in reduce (Literal (Int32Literal (toInteger (I32# (negateInt32# a)))))
      _ -> Nothing

  , primStepEntry $(textNameLit 'GHC.Prim.plusInt32#) $ \case
      PrimStepContext{..} | Just r <- liftI32 plusInt32# args
        -> reduce r
      _ -> Nothing

  , primStepEntry $(textNameLit 'GHC.Prim.subInt32#) $ \case
      PrimStepContext{..} | Just r <- liftI32 subInt32# args
        -> reduce r
      _ -> Nothing

  , primStepEntry $(textNameLit 'GHC.Prim.timesInt32#) $ \case
      PrimStepContext{..} | Just r <- liftI32 timesInt32# args
        -> reduce r
      _ -> Nothing

  , primStepEntry $(textNameLit 'GHC.Prim.quotInt32#) $ \case
      PrimStepContext{..} | [i, j] <- int32Literals' args
        -> reduce $ catchDivByZero
             (Literal (Int32Literal (toInteger (fromInteger i `quot` fromInteger j :: Int32))))
      _ -> Nothing

  , primStepEntry $(textNameLit 'GHC.Prim.remInt32#) $ \case
      PrimStepContext{..} | [i, j] <- int32Literals' args
        -> reduce $ catchDivByZero
             (Literal (Int32Literal (toInteger (fromInteger i `rem` fromInteger j :: Int32))))
      _ -> Nothing

  , primStepEntry $(textNameLit 'GHC.Prim.quotRemInt32#) $ \case
      PrimStepContext{..}
        | [i, j] <- int32Literals' args
        , (_,tyView -> TyConApp tupTcNm tyArgs) <- splitFunForallTy ty
        , (Just tupTc) <- UniqMap.lookup tupTcNm tcm
        , [tupDc] <- tyConDataCons tupTc
        -> let (q,r) = quotRem (fromInteger i :: Int32) (fromInteger j)
           in reduce $ mkApps (Data tupDc) (map Right tyArgs ++
                      [ Left $ catchDivByZero (Literal (Int32Literal (toInteger q)))
                      , Left $ catchDivByZero (Literal (Int32Literal (toInteger r)))])
      _ -> Nothing

  , primStepEntry $(textNameLit 'GHC.Prim.uncheckedShiftLInt32#) $ \case
      PrimStepContext{..} | Just r <- liftI32I uncheckedShiftLInt32# args
        -> reduce r
      _ -> Nothing

  , primStepEntry $(textNameLit 'GHC.Prim.uncheckedShiftRAInt32#) $ \case
      PrimStepContext{..} | Just r <- liftI32I uncheckedShiftRAInt32# args
        -> reduce r
      _ -> Nothing

  , primStepEntry $(textNameLit 'GHC.Prim.uncheckedShiftRLInt32#) $ \case
      PrimStepContext{..} | Just r <- liftI32I uncheckedShiftRLInt32# args
        -> reduce r
      _ -> Nothing

  , primStepEntry $(textNameLit 'GHC.Prim.int32ToWord32#) $ \case
      PrimStepContext{..} | [i] <- int32Literals' args
        -> let !(I32# a) = fromInteger i
            in reduce (Literal (Word32Literal (toInteger (W32# (int32ToWord32# a)))))
      _ -> Nothing

  , primStepEntry $(textNameLit 'GHC.Prim.eqInt32#) $ \case
      PrimStepContext{..} | Just r <- liftI32RI eqInt32# args
        -> reduce r
      _ -> Nothing

  , primStepEntry $(textNameLit 'GHC.Prim.geInt32#) $ \case
      PrimStepContext{..} | Just r <- liftI32RI geInt32# args
        -> reduce r
      _ -> Nothing

  , primStepEntry $(textNameLit 'GHC.Prim.gtInt32#) $ \case
      PrimStepContext{..} | Just r <- liftI32RI gtInt32# args
        -> reduce r
      _ -> Nothing

  , primStepEntry $(textNameLit 'GHC.Prim.leInt32#) $ \case
      PrimStepContext{..} | Just r <- liftI32RI leInt32# args
        -> reduce r
      _ -> Nothing

  , primStepEntry $(textNameLit 'GHC.Prim.ltInt32#) $ \case
      PrimStepContext{..} | Just r <- liftI32RI ltInt32# args
        -> reduce r
      _ -> Nothing

  , primStepEntry $(textNameLit 'GHC.Prim.neInt32#) $ \case
      PrimStepContext{..} | Just r <- liftI32RI neInt32# args
        -> reduce r
      _ -> Nothing


---------
-- Int64#
---------
  , primStepEntry $(textNameLit 'GHC.Prim.intToInt64#) $ \case
      PrimStepContext{..} | [i] <- intLiterals' args
        -> reduce (Literal (Int64Literal i))
      _ -> Nothing

  , primStepEntry $(textNameLit 'GHC.Prim.int64ToInt#) $ \case
      PrimStepContext{..} | [i] <- int64Literals' args
        -> reduce . Literal $ IntLiteral i
      _ -> Nothing

  -- XXX: Primitive does not exist?
  , primStepEntry "GHC.Prim.negateInt64" $ \case
      PrimStepContext{..} | [i] <- int64Literals' args
        -> let !(I64# a) = fromInteger i
            in reduce (Literal (Int64Literal (toInteger (I64# (negateInt64# a)))))
      _ -> Nothing

  , primStepEntry $(textNameLit 'GHC.Prim.plusInt64#) $ \case
      PrimStepContext{..} | Just r <- liftI64 plusInt64# args
        -> reduce r
      _ -> Nothing

  , primStepEntry $(textNameLit 'GHC.Prim.subInt64#) $ \case
      PrimStepContext{..} | Just r <- liftI64 subInt64# args
        -> reduce r
      _ -> Nothing

  , primStepEntry $(textNameLit 'GHC.Prim.timesInt64#) $ \case
      PrimStepContext{..} | Just r <- liftI64 timesInt64# args
        -> reduce r
      _ -> Nothing

  , primStepEntry $(textNameLit 'GHC.Prim.quotInt64#) $ \case
      PrimStepContext{..} | [i, j] <- int64Literals' args
        -> reduce $ catchDivByZero
             (Literal (Int64Literal (toInteger (fromInteger i `quot` fromInteger j :: Int64))))
      _ -> Nothing

  , primStepEntry $(textNameLit 'GHC.Prim.remInt64#) $ \case
      PrimStepContext{..} | [i, j] <- int64Literals' args
        -> reduce $ catchDivByZero
             (Literal (Int64Literal (toInteger (fromInteger i `rem` fromInteger j :: Int64))))
      _ -> Nothing

  , primStepEntry $(textNameLit 'GHC.Prim.uncheckedIShiftL64#) $ \case
      PrimStepContext{..} | Just r <- liftI64I uncheckedIShiftL64# args
        -> reduce r
      _ -> Nothing

  , primStepEntry $(textNameLit 'GHC.Prim.uncheckedIShiftRA64#) $ \case
      PrimStepContext{..} | Just r <- liftI64I uncheckedIShiftRA64# args
        -> reduce r
      _ -> Nothing

  , primStepEntry $(textNameLit 'GHC.Prim.uncheckedIShiftRL64#) $ \case
      PrimStepContext{..} | Just r <- liftI64I uncheckedIShiftRL64# args
        -> reduce r
      _ -> Nothing

  , primStepEntry $(textNameLit 'GHC.Prim.int64ToWord64#) $ \case
      PrimStepContext{..} | [i] <- int64Literals' args
        -> let !(I64# a) = fromInteger i
            in reduce (Literal (Word64Literal (toInteger (W64# (int64ToWord64# a)))))
      _ -> Nothing

  , primStepEntry $(textNameLit 'GHC.Prim.eqInt64#) $ \case
      PrimStepContext{..} | Just r <- liftI64RI eqInt64# args
        -> reduce r
      _ -> Nothing

  , primStepEntry $(textNameLit 'GHC.Prim.geInt64#) $ \case
      PrimStepContext{..} | Just r <- liftI64RI geInt64# args
        -> reduce r
      _ -> Nothing

  , primStepEntry $(textNameLit 'GHC.Prim.gtInt64#) $ \case
      PrimStepContext{..} | Just r <- liftI64RI gtInt64# args
        -> reduce r
      _ -> Nothing

  , primStepEntry $(textNameLit 'GHC.Prim.leInt64#) $ \case
      PrimStepContext{..} | Just r <- liftI64RI leInt64# args
        -> reduce r
      _ -> Nothing

  , primStepEntry $(textNameLit 'GHC.Prim.ltInt64#) $ \case
      PrimStepContext{..} | Just r <- liftI64RI ltInt64# args
        -> reduce r
      _ -> Nothing

  , primStepEntry $(textNameLit 'GHC.Prim.neInt64#) $ \case
      PrimStepContext{..} | Just r <- liftI64RI neInt64# args
        -> reduce r
      _ -> Nothing


---------
-- Word8#
---------
  , primStepEntry $(textNameLit 'GHC.Prim.wordToWord8#) $ \case
      PrimStepContext{..} | [i] <- wordLiterals' args
        -> let !(W# a)  = fromInteger i
               b = narrow8Word# a
           in  reduce . Literal . Word8Literal . toInteger $ W# b
      _ -> Nothing

  , primStepEntry $(textNameLit 'GHC.Prim.word8ToWord#) $ \case
      PrimStepContext{..} | [i] <- word8Literals' args
        -> reduce . Literal $ WordLiteral i
      _ -> Nothing

  , primStepEntry $(textNameLit 'GHC.Prim.plusWord8#) $ \case
      PrimStepContext{..} | Just r <- liftW8 plusWord8# args
        -> reduce r
      _ -> Nothing

  , primStepEntry $(textNameLit 'GHC.Prim.subWord8#) $ \case
      PrimStepContext{..} | Just r <- liftW8 subWord8# args
        -> reduce r
      _ -> Nothing

  , primStepEntry $(textNameLit 'GHC.Prim.timesWord8#) $ \case
      PrimStepContext{..} | Just r <- liftW8 timesWord8# args
        -> reduce r
      _ -> Nothing

  , primStepEntry $(textNameLit 'GHC.Prim.quotWord8#) $ \case
      PrimStepContext{..} | [i, j] <- word8Literals' args
        -> reduce $ catchDivByZero
             (Literal (Word8Literal (toInteger (fromInteger i `quot` fromInteger j :: Word8))))
      _ -> Nothing

  , primStepEntry $(textNameLit 'GHC.Prim.remWord8#) $ \case
      PrimStepContext{..} | [i, j] <- word8Literals' args
        -> reduce $ catchDivByZero
             (Literal (Word8Literal (toInteger (fromInteger i `rem` fromInteger j :: Word8))))
      _ -> Nothing

  , primStepEntry $(textNameLit 'GHC.Prim.quotRemWord8#) $ \case
      PrimStepContext{..}
        | [i, j] <- word8Literals' args
        , (_,tyView -> TyConApp tupTcNm tyArgs) <- splitFunForallTy ty
        , (Just tupTc) <- UniqMap.lookup tupTcNm tcm
        , [tupDc] <- tyConDataCons tupTc
        -> let (q,r) = quotRem (fromInteger i :: Word8) (fromInteger j)
           in reduce $ mkApps (Data tupDc) (map Right tyArgs ++
                      [ Left $ catchDivByZero (Literal (Word8Literal (toInteger q)))
                      , Left $ catchDivByZero (Literal (Word8Literal (toInteger r)))])
      _ -> Nothing

  , primStepEntry $(textNameLit 'GHC.Prim.andWord8#) $ \case
      PrimStepContext{..} | Just r <- liftW8 andWord8# args
        -> reduce r
      _ -> Nothing

  , primStepEntry $(textNameLit 'GHC.Prim.orWord8#) $ \case
      PrimStepContext{..} | Just r <- liftW8 orWord8# args
        -> reduce r
      _ -> Nothing

  , primStepEntry $(textNameLit 'GHC.Prim.xorWord8#) $ \case
      PrimStepContext{..} | Just r <- liftW8 xorWord8# args
        -> reduce r
      _ -> Nothing

  , primStepEntry $(textNameLit 'GHC.Prim.notWord8#) $ \case
      PrimStepContext{..} | [i] <- word8Literals' args
        -> let !(W8# a) = fromInteger i
            in reduce (Literal (Word8Literal (toInteger (W8# (notWord8# a)))))
      _ -> Nothing

  , primStepEntry $(textNameLit 'GHC.Prim.uncheckedShiftLWord8#) $ \case
      PrimStepContext{..} | Just r <- liftW8I uncheckedShiftLWord8# args
        -> reduce r
      _ -> Nothing

  , primStepEntry $(textNameLit 'GHC.Prim.uncheckedShiftRLWord8#) $ \case
      PrimStepContext{..} | Just r <- liftW8I uncheckedShiftRLWord8# args
        -> reduce r
      _ -> Nothing

  , primStepEntry $(textNameLit 'GHC.Prim.word8ToInt8#) $ \case
      PrimStepContext{..} | [i] <- word8Literals' args
        -> let !(W8# a) = fromInteger i
            in reduce (Literal (Int8Literal (toInteger (I8# (word8ToInt8# a)))))
      _ -> Nothing

  , primStepEntry $(textNameLit 'GHC.Prim.eqWord8#) $ \case
      PrimStepContext{..} | Just r <- liftW8RI eqWord8# args
        -> reduce r
      _ -> Nothing

  , primStepEntry $(textNameLit 'GHC.Prim.geWord8#) $ \case
      PrimStepContext{..} | Just r <- liftW8RI geWord8# args
        -> reduce r
      _ -> Nothing

  , primStepEntry $(textNameLit 'GHC.Prim.gtWord8#) $ \case
      PrimStepContext{..} | Just r <- liftW8RI gtWord8# args
        -> reduce r
      _ -> Nothing

  , primStepEntry $(textNameLit 'GHC.Prim.leWord8#) $ \case
      PrimStepContext{..} | Just r <- liftW8RI leWord8# args
        -> reduce r
      _ -> Nothing

  , primStepEntry $(textNameLit 'GHC.Prim.ltWord8#) $ \case
      PrimStepContext{..} | Just r <- liftW8RI ltWord8# args
        -> reduce r
      _ -> Nothing

  , primStepEntry $(textNameLit 'GHC.Prim.neWord8#) $ \case
      PrimStepContext{..} | Just r <- liftW8RI neWord8# args
        -> reduce r
      _ -> Nothing


----------
-- Word16#
----------
  , primStepEntry $(textNameLit 'GHC.Prim.wordToWord16#) $ \case
      PrimStepContext{..} | [i] <- wordLiterals' args
        -> let !(W# a)  = fromInteger i
               b = narrow16Word# a
           in  reduce . Literal . Word16Literal . toInteger $ W# b
      _ -> Nothing

  , primStepEntry $(textNameLit 'GHC.Prim.word16ToWord#) $ \case
      PrimStepContext{..} | [i] <- word16Literals' args
        -> reduce . Literal $ WordLiteral i
      _ -> Nothing

  , primStepEntry $(textNameLit 'GHC.Prim.plusWord16#) $ \case
      PrimStepContext{..} | Just r <- liftW16 plusWord16# args
        -> reduce r
      _ -> Nothing

  , primStepEntry $(textNameLit 'GHC.Prim.subWord16#) $ \case
      PrimStepContext{..} | Just r <- liftW16 subWord16# args
        -> reduce r
      _ -> Nothing

  , primStepEntry $(textNameLit 'GHC.Prim.timesWord16#) $ \case
      PrimStepContext{..} | Just r <- liftW16 timesWord16# args
        -> reduce r
      _ -> Nothing

  , primStepEntry $(textNameLit 'GHC.Prim.quotWord16#) $ \case
      PrimStepContext{..} | [i, j] <- word16Literals' args
        -> reduce $ catchDivByZero
             (Literal (Word16Literal (toInteger (fromInteger i `quot` fromInteger j :: Word16))))
      _ -> Nothing

  , primStepEntry $(textNameLit 'GHC.Prim.remWord16#) $ \case
      PrimStepContext{..} | [i, j] <- word16Literals' args
        -> reduce $ catchDivByZero
             (Literal (Word16Literal (toInteger (fromInteger i `rem` fromInteger j :: Word16))))
      _ -> Nothing

  , primStepEntry $(textNameLit 'GHC.Prim.quotRemWord16#) $ \case
      PrimStepContext{..}
        | [i, j] <- word16Literals' args
        , (_,tyView -> TyConApp tupTcNm tyArgs) <- splitFunForallTy ty
        , (Just tupTc) <- UniqMap.lookup tupTcNm tcm
        , [tupDc] <- tyConDataCons tupTc
        -> let (q,r) = quotRem (fromInteger i :: Word16) (fromInteger j)
           in reduce $ mkApps (Data tupDc) (map Right tyArgs ++
                      [ Left $ catchDivByZero (Literal (Word16Literal (toInteger q)))
                      , Left $ catchDivByZero (Literal (Word16Literal (toInteger r)))])
      _ -> Nothing

  , primStepEntry $(textNameLit 'GHC.Prim.andWord16#) $ \case
      PrimStepContext{..} | Just r <- liftW16 andWord16# args
        -> reduce r
      _ -> Nothing

  , primStepEntry $(textNameLit 'GHC.Prim.orWord16#) $ \case
      PrimStepContext{..} | Just r <- liftW16 orWord16# args
        -> reduce r
      _ -> Nothing

  , primStepEntry $(textNameLit 'GHC.Prim.xorWord16#) $ \case
      PrimStepContext{..} | Just r <- liftW16 xorWord16# args
        -> reduce r
      _ -> Nothing

  , primStepEntry $(textNameLit 'GHC.Prim.notWord16#) $ \case
      PrimStepContext{..} | [i] <- word16Literals' args
        -> let !(W16# a) = fromInteger i
            in reduce (Literal (Word16Literal (toInteger (W16# (notWord16# a)))))
      _ -> Nothing

  , primStepEntry $(textNameLit 'GHC.Prim.uncheckedShiftLWord16#) $ \case
      PrimStepContext{..} | Just r <- liftW16I uncheckedShiftLWord16# args
        -> reduce r
      _ -> Nothing

  , primStepEntry $(textNameLit 'GHC.Prim.uncheckedShiftRLWord16#) $ \case
      PrimStepContext{..} | Just r <- liftW16I uncheckedShiftRLWord16# args
        -> reduce r
      _ -> Nothing

  , primStepEntry $(textNameLit 'GHC.Prim.word16ToInt16#) $ \case
      PrimStepContext{..} | [i] <- word16Literals' args
        -> let !(W16# a) = fromInteger i
            in reduce (Literal (Int16Literal (toInteger (I16# (word16ToInt16# a)))))
      _ -> Nothing

  , primStepEntry $(textNameLit 'GHC.Prim.eqWord16#) $ \case
      PrimStepContext{..} | Just r <- liftW16RI eqWord16# args
        -> reduce r
      _ -> Nothing

  , primStepEntry $(textNameLit 'GHC.Prim.geWord16#) $ \case
      PrimStepContext{..} | Just r <- liftW16RI geWord16# args
        -> reduce r
      _ -> Nothing

  , primStepEntry $(textNameLit 'GHC.Prim.gtWord16#) $ \case
      PrimStepContext{..} | Just r <- liftW16RI gtWord16# args
        -> reduce r
      _ -> Nothing

  , primStepEntry $(textNameLit 'GHC.Prim.leWord16#) $ \case
      PrimStepContext{..} | Just r <- liftW16RI leWord16# args
        -> reduce r
      _ -> Nothing

  , primStepEntry $(textNameLit 'GHC.Prim.ltWord16#) $ \case
      PrimStepContext{..} | Just r <- liftW16RI ltWord16# args
        -> reduce r
      _ -> Nothing

  , primStepEntry $(textNameLit 'GHC.Prim.neWord16#) $ \case
      PrimStepContext{..} | Just r <- liftW16RI neWord16# args
        -> reduce r
      _ -> Nothing


----------
-- Word32#
----------
  , primStepEntry $(textNameLit 'GHC.Prim.wordToWord32#) $ \case
      PrimStepContext{..} | [i] <- wordLiterals' args
        -> let !(W# a)  = fromInteger i
               b = narrow32Word# a
           in  reduce . Literal . Word32Literal . toInteger $ W# b
      _ -> Nothing

  , primStepEntry $(textNameLit 'GHC.Prim.word32ToWord#) $ \case
      PrimStepContext{..} | [i] <- word32Literals' args
        -> reduce . Literal $ WordLiteral i
      _ -> Nothing

  , primStepEntry $(textNameLit 'GHC.Prim.plusWord32#) $ \case
      PrimStepContext{..} | Just r <- liftW32 plusWord32# args
        -> reduce r
      _ -> Nothing

  , primStepEntry $(textNameLit 'GHC.Prim.subWord32#) $ \case
      PrimStepContext{..} | Just r <- liftW32 subWord32# args
        -> reduce r
      _ -> Nothing

  , primStepEntry $(textNameLit 'GHC.Prim.timesWord32#) $ \case
      PrimStepContext{..} | Just r <- liftW32 timesWord32# args
        -> reduce r
      _ -> Nothing

  , primStepEntry $(textNameLit 'GHC.Prim.quotWord32#) $ \case
      PrimStepContext{..} | [i, j] <- word32Literals' args
        -> reduce $ catchDivByZero
             (Literal (Word32Literal (toInteger (fromInteger i `quot` fromInteger j :: Word32))))
      _ -> Nothing

  , primStepEntry $(textNameLit 'GHC.Prim.remWord32#) $ \case
      PrimStepContext{..} | [i, j] <- word32Literals' args
        -> reduce $ catchDivByZero
             (Literal (Word32Literal (toInteger (fromInteger i `rem` fromInteger j :: Word32))))
      _ -> Nothing

  , primStepEntry $(textNameLit 'GHC.Prim.quotRemWord32#) $ \case
      PrimStepContext{..}
        | [i, j] <- word32Literals' args
        , (_,tyView -> TyConApp tupTcNm tyArgs) <- splitFunForallTy ty
        , (Just tupTc) <- UniqMap.lookup tupTcNm tcm
        , [tupDc] <- tyConDataCons tupTc
        -> let (q,r) = quotRem (fromInteger i :: Word32) (fromInteger j)
           in reduce $ mkApps (Data tupDc) (map Right tyArgs ++
                      [ Left $ catchDivByZero (Literal (Word32Literal (toInteger q)))
                      , Left $ catchDivByZero (Literal (Word32Literal (toInteger r)))])
      _ -> Nothing

  , primStepEntry $(textNameLit 'GHC.Prim.andWord32#) $ \case
      PrimStepContext{..} | Just r <- liftW32 andWord32# args
        -> reduce r
      _ -> Nothing

  , primStepEntry $(textNameLit 'GHC.Prim.orWord32#) $ \case
      PrimStepContext{..} | Just r <- liftW32 orWord32# args
        -> reduce r
      _ -> Nothing

  , primStepEntry $(textNameLit 'GHC.Prim.xorWord32#) $ \case
      PrimStepContext{..} | Just r <- liftW32 xorWord32# args
        -> reduce r
      _ -> Nothing

  , primStepEntry $(textNameLit 'GHC.Prim.notWord32#) $ \case
      PrimStepContext{..} | [i] <- word32Literals' args
        -> let !(W32# a) = fromInteger i
            in reduce (Literal (Word32Literal (toInteger (W32# (notWord32# a)))))
      _ -> Nothing

  , primStepEntry $(textNameLit 'GHC.Prim.uncheckedShiftLWord32#) $ \case
      PrimStepContext{..} | Just r <- liftW32I uncheckedShiftLWord32# args
        -> reduce r
      _ -> Nothing

  , primStepEntry $(textNameLit 'GHC.Prim.uncheckedShiftRLWord32#) $ \case
      PrimStepContext{..} | Just r <- liftW32I uncheckedShiftRLWord32# args
        -> reduce r
      _ -> Nothing

  , primStepEntry $(textNameLit 'GHC.Prim.word32ToInt32#) $ \case
      PrimStepContext{..} | [i] <- word32Literals' args
        -> let !(W32# a) = fromInteger i
            in reduce (Literal (Int32Literal (toInteger (I32# (word32ToInt32# a)))))
      _ -> Nothing

  , primStepEntry $(textNameLit 'GHC.Prim.eqWord32#) $ \case
      PrimStepContext{..} | Just r <- liftW32RI eqWord32# args
        -> reduce r
      _ -> Nothing

  , primStepEntry $(textNameLit 'GHC.Prim.geWord32#) $ \case
      PrimStepContext{..} | Just r <- liftW32RI geWord32# args
        -> reduce r
      _ -> Nothing

  , primStepEntry $(textNameLit 'GHC.Prim.gtWord32#) $ \case
      PrimStepContext{..} | Just r <- liftW32RI gtWord32# args
        -> reduce r
      _ -> Nothing

  , primStepEntry $(textNameLit 'GHC.Prim.leWord32#) $ \case
      PrimStepContext{..} | Just r <- liftW32RI leWord32# args
        -> reduce r
      _ -> Nothing

  , primStepEntry $(textNameLit 'GHC.Prim.ltWord32#) $ \case
      PrimStepContext{..} | Just r <- liftW32RI ltWord32# args
        -> reduce r
      _ -> Nothing

  , primStepEntry $(textNameLit 'GHC.Prim.neWord32#) $ \case
      PrimStepContext{..} | Just r <- liftW32RI neWord32# args
        -> reduce r
      _ -> Nothing


----------
-- Word64#
----------
  , primStepEntry $(textNameLit 'GHC.Prim.wordToWord64#) $ \case
      PrimStepContext{..} | [i] <- wordLiterals' args
        -> reduce (Literal (Word64Literal i))
      _ -> Nothing

  , primStepEntry $(textNameLit 'GHC.Prim.word64ToWord#) $ \case
      PrimStepContext{..} | [i] <- word64Literals' args
        -> reduce . Literal $ WordLiteral i
      _ -> Nothing

  , primStepEntry $(textNameLit 'GHC.Prim.plusWord64#) $ \case
      PrimStepContext{..} | Just r <- liftW64 plusWord64# args
        -> reduce r
      _ -> Nothing

  , primStepEntry $(textNameLit 'GHC.Prim.subWord64#) $ \case
      PrimStepContext{..} | Just r <- liftW64 subWord64# args
        -> reduce r
      _ -> Nothing

  , primStepEntry $(textNameLit 'GHC.Prim.timesWord64#) $ \case
      PrimStepContext{..} | Just r <- liftW64 timesWord64# args
        -> reduce r
      _ -> Nothing

  , primStepEntry $(textNameLit 'GHC.Prim.quotWord64#) $ \case
      PrimStepContext{..} | [i, j] <- word64Literals' args
        -> reduce $ catchDivByZero
             (Literal (Word64Literal (toInteger (fromInteger i `quot` fromInteger j :: Word64))))
      _ -> Nothing

  , primStepEntry $(textNameLit 'GHC.Prim.remWord64#) $ \case
      PrimStepContext{..} | [i, j] <- word64Literals' args
        -> reduce $ catchDivByZero
             (Literal (Word64Literal (toInteger (fromInteger i `rem` fromInteger j :: Word64))))
      _ -> Nothing

  , primStepEntry $(textNameLit 'GHC.Prim.and64#) $ \case
      PrimStepContext{..} | Just r <- liftW64 and64# args
        -> reduce r
      _ -> Nothing

  , primStepEntry $(textNameLit 'GHC.Prim.or64#) $ \case
      PrimStepContext{..} | Just r <- liftW64 or64# args
        -> reduce r
      _ -> Nothing

  , primStepEntry $(textNameLit 'GHC.Prim.xor64#) $ \case
      PrimStepContext{..} | Just r <- liftW64 xor64# args
        -> reduce r
      _ -> Nothing

  , primStepEntry $(textNameLit 'GHC.Prim.not64#) $ \case
      PrimStepContext{..} | [i] <- word64Literals' args
        -> let !(W64# a) = fromInteger i
            in reduce (Literal (Word64Literal (toInteger (W64# (not64# a)))))
      _ -> Nothing

  , primStepEntry $(textNameLit 'GHC.Prim.uncheckedShiftL64#) $ \case
      PrimStepContext{..} | Just r <- liftW64I uncheckedShiftL64# args
        -> reduce r
      _ -> Nothing

  , primStepEntry $(textNameLit 'GHC.Prim.uncheckedShiftRL64#) $ \case
      PrimStepContext{..} | Just r <- liftW64I uncheckedShiftRL64# args
        -> reduce r
      _ -> Nothing

  , primStepEntry $(textNameLit 'GHC.Prim.word64ToInt64#) $ \case
      PrimStepContext{..} | [i] <- word64Literals' args
        -> let !(W64# a) = fromInteger i
            in reduce (Literal (Int64Literal (toInteger (I64# (word64ToInt64# a)))))
      _ -> Nothing

  , primStepEntry $(textNameLit 'GHC.Prim.eqWord64#) $ \case
      PrimStepContext{..} | Just r <- liftW64RI eqWord64# args
        -> reduce r
      _ -> Nothing

  , primStepEntry $(textNameLit 'GHC.Prim.geWord64#) $ \case
      PrimStepContext{..} | Just r <- liftW64RI geWord64# args
        -> reduce r
      _ -> Nothing

  , primStepEntry $(textNameLit 'GHC.Prim.gtWord64#) $ \case
      PrimStepContext{..} | Just r <- liftW64RI gtWord64# args
        -> reduce r
      _ -> Nothing

  , primStepEntry $(textNameLit 'GHC.Prim.leWord64#) $ \case
      PrimStepContext{..} | Just r <- liftW64RI leWord64# args
        -> reduce r
      _ -> Nothing

  , primStepEntry $(textNameLit 'GHC.Prim.ltWord64#) $ \case
      PrimStepContext{..} | Just r <- liftW64RI ltWord64# args
        -> reduce r
      _ -> Nothing

  , primStepEntry $(textNameLit 'GHC.Prim.neWord64#) $ \case
      PrimStepContext{..} | Just r <- liftW64RI neWord64# args
        -> reduce r
      _ -> Nothing


----------
-- Double#
----------
  , primStepEntry $(textNameLit '(GHC.Prim.>##)) $ \case
      PrimStepContext{..}  | Just r <- liftDDI (>##)  args
        -> reduce r
      _ -> Nothing

  , primStepEntry $(textNameLit '(GHC.Prim.>=##)) $ \case
      PrimStepContext{..} | Just r <- liftDDI (>=##) args
        -> reduce r
      _ -> Nothing

  , primStepEntry $(textNameLit '(GHC.Prim.==##)) $ \case
      PrimStepContext{..} | Just r <- liftDDI (==##) args
        -> reduce r
      _ -> Nothing

  , primStepEntry $(textNameLit '(GHC.Prim./=##)) $ \case
      PrimStepContext{..} | Just r <- liftDDI (/=##) args
        -> reduce r
      _ -> Nothing

  , primStepEntry $(textNameLit '(GHC.Prim.<##)) $ \case
      PrimStepContext{..}  | Just r <- liftDDI (<##)  args
        -> reduce r
      _ -> Nothing

  , primStepEntry $(textNameLit '(GHC.Prim.<=##)) $ \case
      PrimStepContext{..} | Just r <- liftDDI (<=##) args
        -> reduce r
      _ -> Nothing

  , primStepEntry $(textNameLit '(GHC.Prim.+##)) $ \case
      PrimStepContext{..}  | Just r <- liftDDD (+##)  args
        -> reduce r
      _ -> Nothing

  , primStepEntry $(textNameLit '(GHC.Prim.-##)) $ \case
      PrimStepContext{..}  | Just r <- liftDDD (-##)  args
        -> reduce r
      _ -> Nothing

  , primStepEntry $(textNameLit '(GHC.Prim.*##)) $ \case
      PrimStepContext{..}  | Just r <- liftDDD (*##)  args
        -> reduce r
      _ -> Nothing

  , primStepEntry $(textNameLit '(GHC.Prim./##)) $ \case
      PrimStepContext{..}  | Just r <- liftDDD (/##)  args
        -> reduce r
      _ -> Nothing

  , primStepEntry $(textNameLit 'GHC.Prim.negateDouble#) $ \case
      PrimStepContext{..} | Just r <- liftDD negateDouble# args
        -> reduce r
      _ -> Nothing

  , primStepEntry $(textNameLit 'GHC.Prim.fabsDouble#) $ \case
      PrimStepContext{..} | Just r <- liftDD fabsDouble# args
        -> reduce r
      _ -> Nothing

  , primStepEntry $(textNameLit 'GHC.Prim.double2Int#) $ \case
      PrimStepContext{..} | [i] <- doubleLiterals' args
        -> let !(D# a) = castWord64ToDouble i
               r = double2Int# a
           in  reduce . Literal . IntLiteral . toInteger $ I# r
      _ -> Nothing

  , primStepEntry $(textNameLit 'GHC.Prim.double2Float#) $ \case
      PrimStepContext{..}
        | [Lit (DoubleLiteral d)] <- args
        -> let !(D# a) = castWord64ToDouble d
               r = double2Float# a
           in reduce . Literal . FloatLiteral . castFloatToWord32 $ F# r
      _ -> Nothing

  , primStepEntry $(textNameLit 'GHC.Prim.expDouble#) $ \case
      PrimStepContext{..} | Just r <- liftDD expDouble# args
        -> reduce r
      _ -> Nothing

  , primStepEntry $(textNameLit 'GHC.Prim.logDouble#) $ \case
      PrimStepContext{..} | Just r <- liftDD logDouble# args
        -> reduce r
      _ -> Nothing

  , primStepEntry $(textNameLit 'GHC.Prim.sqrtDouble#) $ \case
      PrimStepContext{..} | Just r <- liftDD sqrtDouble# args
        -> reduce r
      _ -> Nothing

  , primStepEntry $(textNameLit 'GHC.Prim.sinDouble#) $ \case
      PrimStepContext{..} | Just r <- liftDD sinDouble# args
        -> reduce r
      _ -> Nothing

  , primStepEntry $(textNameLit 'GHC.Prim.cosDouble#) $ \case
      PrimStepContext{..} | Just r <- liftDD cosDouble# args
        -> reduce r
      _ -> Nothing

  , primStepEntry $(textNameLit 'GHC.Prim.tanDouble#) $ \case
      PrimStepContext{..} | Just r <- liftDD tanDouble# args
        -> reduce r
      _ -> Nothing

  , primStepEntry $(textNameLit 'GHC.Prim.asinDouble#) $ \case
      PrimStepContext{..} | Just r <- liftDD asinDouble# args
        -> reduce r
      _ -> Nothing

  , primStepEntry $(textNameLit 'GHC.Prim.acosDouble#) $ \case
      PrimStepContext{..} | Just r <- liftDD acosDouble# args
        -> reduce r
      _ -> Nothing

  , primStepEntry $(textNameLit 'GHC.Prim.atanDouble#) $ \case
      PrimStepContext{..} | Just r <- liftDD atanDouble# args
        -> reduce r
      _ -> Nothing

  , primStepEntry $(textNameLit 'GHC.Prim.sinhDouble#) $ \case
      PrimStepContext{..} | Just r <- liftDD sinhDouble# args
        -> reduce r
      _ -> Nothing

  , primStepEntry $(textNameLit 'GHC.Prim.coshDouble#) $ \case
      PrimStepContext{..} | Just r <- liftDD coshDouble# args
        -> reduce r
      _ -> Nothing

  , primStepEntry $(textNameLit 'GHC.Prim.tanhDouble#) $ \case
      PrimStepContext{..} | Just r <- liftDD tanhDouble# args
        -> reduce r
      _ -> Nothing

  , primStepEntry $(textNameLit 'GHC.Prim.asinhDouble#) $ \case
      PrimStepContext{..}  | Just r <- liftDD asinhDouble# args
        -> reduce r
      _ -> Nothing

  , primStepEntry $(textNameLit 'GHC.Prim.acoshDouble#) $ \case
      PrimStepContext{..}  | Just r <- liftDD acoshDouble# args
        -> reduce r
      _ -> Nothing

  , primStepEntry $(textNameLit 'GHC.Prim.atanhDouble#) $ \case
      PrimStepContext{..}  | Just r <- liftDD atanhDouble# args
        -> reduce r
      _ -> Nothing

  , primStepEntry $(textNameLit '(GHC.Prim.**##)) $ \case
      PrimStepContext{..} | Just r <- liftDDD (**##) args
        -> reduce r
      _ -> Nothing

-- decodeDouble_2Int# :: Double# -> (#Int#, Word#, Word#, Int##)
  , primStepEntry $(textNameLit 'GHC.Prim.decodeDouble_2Int#) $ \case
      PrimStepContext{..} | [i] <- doubleLiterals' args
        -> let (_,tyView -> TyConApp tupTcNm tyArgs) = splitFunForallTy ty
               (Just tupTc) = UniqMap.lookup tupTcNm tcm
               [tupDc] = tyConDataCons tupTc
               !(D# a) = castWord64ToDouble i
               !(# p, q, r, s #) = decodeDouble_2Int# a
           in reduce $
              mkApps (Data tupDc) (map Right tyArgs ++
                       [ Left (Literal . IntLiteral  . toInteger $ I# p)
                       , Left (Literal . WordLiteral . toInteger $ W# q)
                       , Left (Literal . WordLiteral . toInteger $ W# r)
                       , Left (Literal . IntLiteral  . toInteger $ I# s)])
      _ -> Nothing

-- decodeDouble_Int64# :: Double# -> (# Int64#, Int# #)
  , primStepEntry $(textNameLit 'GHC.Prim.decodeDouble_Int64#) $ \case
      PrimStepContext{..} | [i] <- doubleLiterals' args
        -> let (_,tyView -> TyConApp tupTcNm tyArgs) = splitFunForallTy ty
               (Just tupTc) = UniqMap.lookup tupTcNm tcm
               [tupDc] = tyConDataCons tupTc
               !(D# a) = castWord64ToDouble i
               !(# p, q #) = decodeDouble_Int64# a
           in reduce $
              mkApps (Data tupDc) (map Right tyArgs ++
                       [ Left (Literal . Int64Literal  . toInteger $ I64# p)
                       , Left (Literal . IntLiteral  . toInteger $ I# q)])
      _ -> Nothing


--------
-- Float
--------
  , primStepEntry $(textNameLit 'GHC.Prim.gtFloat#) $ \case
      PrimStepContext{..}  | Just r <- liftFFI gtFloat# args
        -> reduce r
      _ -> Nothing

  , primStepEntry $(textNameLit 'GHC.Prim.geFloat#) $ \case
      PrimStepContext{..}  | Just r <- liftFFI geFloat# args
        -> reduce r
      _ -> Nothing

  , primStepEntry $(textNameLit 'GHC.Prim.eqFloat#) $ \case
      PrimStepContext{..}  | Just r <- liftFFI eqFloat# args
        -> reduce r
      _ -> Nothing

  , primStepEntry $(textNameLit 'GHC.Prim.neFloat#) $ \case
      PrimStepContext{..}  | Just r <- liftFFI neFloat# args
        -> reduce r
      _ -> Nothing

  , primStepEntry $(textNameLit 'GHC.Prim.ltFloat#) $ \case
      PrimStepContext{..}  | Just r <- liftFFI ltFloat# args
        -> reduce r
      _ -> Nothing

  , primStepEntry $(textNameLit 'GHC.Prim.leFloat#) $ \case
      PrimStepContext{..}  | Just r <- liftFFI leFloat# args
        -> reduce r
      _ -> Nothing

  , primStepEntry $(textNameLit 'GHC.Prim.plusFloat#) $ \case
      PrimStepContext{..}  | Just r <- liftFFF plusFloat# args
        -> reduce r
      _ -> Nothing

  , primStepEntry $(textNameLit 'GHC.Prim.minusFloat#) $ \case
      PrimStepContext{..}  | Just r <- liftFFF minusFloat# args
        -> reduce r
      _ -> Nothing

  , primStepEntry $(textNameLit 'GHC.Prim.timesFloat#) $ \case
      PrimStepContext{..}  | Just r <- liftFFF timesFloat# args
        -> reduce r
      _ -> Nothing

  , primStepEntry $(textNameLit 'GHC.Prim.divideFloat#) $ \case
      PrimStepContext{..}  | Just r <- liftFFF divideFloat# args
        -> reduce r
      _ -> Nothing

  , primStepEntry $(textNameLit 'GHC.Prim.negateFloat#) $ \case
      PrimStepContext{..}  | Just r <- liftFF negateFloat# args
        -> reduce r
      _ -> Nothing

  , primStepEntry $(textNameLit 'GHC.Prim.fabsFloat#) $ \case
      PrimStepContext{..}  | Just r <- liftFF fabsFloat# args
        -> reduce r
      _ -> Nothing

  , primStepEntry $(textNameLit 'GHC.Prim.float2Int#) $ \case
      PrimStepContext{..} | [i] <- floatLiterals' args
        -> let !(F# a) = castWord32ToFloat i
               r = float2Int# a
           in  reduce . Literal . IntLiteral . toInteger $ I# r
      _ -> Nothing

  , primStepEntry $(textNameLit 'GHC.Prim.expFloat#) $ \case
      PrimStepContext{..}  | Just r <- liftFF expFloat# args
        -> reduce r
      _ -> Nothing

  , primStepEntry $(textNameLit 'GHC.Prim.logFloat#) $ \case
      PrimStepContext{..}  | Just r <- liftFF logFloat# args
        -> reduce r
      _ -> Nothing

  , primStepEntry $(textNameLit 'GHC.Prim.sqrtFloat#) $ \case
      PrimStepContext{..}  | Just r <- liftFF sqrtFloat# args
        -> reduce r
      _ -> Nothing

  , primStepEntry $(textNameLit 'GHC.Prim.sinFloat#) $ \case
      PrimStepContext{..}  | Just r <- liftFF sinFloat# args
        -> reduce r
      _ -> Nothing

  , primStepEntry $(textNameLit 'GHC.Prim.cosFloat#) $ \case
      PrimStepContext{..}  | Just r <- liftFF cosFloat# args
        -> reduce r
      _ -> Nothing

  , primStepEntry $(textNameLit 'GHC.Prim.tanFloat#) $ \case
      PrimStepContext{..}  | Just r <- liftFF tanFloat# args
        -> reduce r
      _ -> Nothing

  , primStepEntry $(textNameLit 'GHC.Prim.asinFloat#) $ \case
      PrimStepContext{..}  | Just r <- liftFF asinFloat# args
        -> reduce r
      _ -> Nothing

  , primStepEntry $(textNameLit 'GHC.Prim.acosFloat#) $ \case
      PrimStepContext{..}  | Just r <- liftFF acosFloat# args
        -> reduce r
      _ -> Nothing

  , primStepEntry $(textNameLit 'GHC.Prim.atanFloat#) $ \case
      PrimStepContext{..}  | Just r <- liftFF atanFloat# args
        -> reduce r
      _ -> Nothing

  , primStepEntry $(textNameLit 'GHC.Prim.sinhFloat#) $ \case
      PrimStepContext{..}  | Just r <- liftFF sinhFloat# args
        -> reduce r
      _ -> Nothing

  , primStepEntry $(textNameLit 'GHC.Prim.coshFloat#) $ \case
      PrimStepContext{..}  | Just r <- liftFF coshFloat# args
        -> reduce r
      _ -> Nothing

  , primStepEntry $(textNameLit 'GHC.Prim.tanhFloat#) $ \case
      PrimStepContext{..}  | Just r <- liftFF tanhFloat# args
        -> reduce r
      _ -> Nothing

  , primStepEntry $(textNameLit 'GHC.Prim.powerFloat#) $ \case
      PrimStepContext{..}  | Just r <- liftFFF powerFloat# args
        -> reduce r
      _ -> Nothing

  , primStepEntry $(textNameLit 'GHC.Prim.asinhFloat#) $ \case
      PrimStepContext{..}  | Just r <- liftFF asinhFloat# args
        -> reduce r
      _ -> Nothing

  , primStepEntry $(textNameLit 'GHC.Prim.acoshFloat#) $ \case
      PrimStepContext{..}  | Just r <- liftFF acoshFloat# args
        -> reduce r
      _ -> Nothing

  , primStepEntry $(textNameLit 'GHC.Prim.atanhFloat#) $ \case
      PrimStepContext{..}  | Just r <- liftFF atanhFloat# args
        -> reduce r
      _ -> Nothing

  , primStepEntry $(textNameLit 'GHC.Prim.float2Double#) $ \case
      PrimStepContext{..} | [i] <- floatLiterals' args
        -> let !(F# a) = castWord32ToFloat i
               r = float2Double# a
           in  reduce . Literal . DoubleLiteral . castDoubleToWord64 $ D# r
      _ -> Nothing


  , primStepEntry $(textNameLit 'GHC.Prim.newByteArray#) $ \case
      PrimStepContext{..}
        | [iV,PrimVal rwTy _ _] <- args
        , [i] <- intLiterals' [iV]
        -> let (_,tyView -> TyConApp tupTcNm tyArgs) = splitFunForallTy ty
               (Just tupTc) = UniqMap.lookup tupTcNm tcm
               [tupDc] = tyConDataCons tupTc
               p = primCount mach
               lit = Literal (ByteArrayLiteral (fromList (List.genericReplicate i 0)))
               mbaTy = mkFunTy intPrimTy (last tyArgs)
               newE = mkApps (Data tupDc) (map Right tyArgs ++
                        [Left (Prim rwTy)
                        ,Left (mkApps (Prim (PrimInfo (showt ''MutableByteArray#) mbaTy WorkNever SingleResult NoUnfolding))
                                      [Left (Literal . IntLiteral $ toInteger p)])
                        ])
           in Just . setTerm newE $ primInsert p lit mach
      _ -> Nothing

  , primStepEntry $(textNameLit 'GHC.Prim.setByteArray#) $ \case
      PrimStepContext{..}
        | [PrimVal _mbaTy _ [baV]
          ,offV,lenV,cV
          ,PrimVal rwTy _ _
          ] <- args
        , [ba,off,len,c] <- intLiterals' [baV,offV,lenV,cV]
        -> let Just (Literal (ByteArrayLiteral ba1)) =
                  primLookup (fromInteger ba) mach
               !(I# off') = fromInteger off
               !(I# len') = fromInteger len
               !(I# c')   = fromInteger c
               ba2 = unsafeDupablePerformIO $ do
                      BA.MutableByteArray mba <- BA.unsafeThawByteArray ba1
                      svoid (setByteArray# mba off' len' c')
                      BA.unsafeFreezeByteArray (BA.MutableByteArray mba)
               ba3 = Literal (ByteArrayLiteral ba2)
           in Just . setTerm (Prim rwTy) $ primUpdate (fromInteger ba) ba3 mach
      _ -> Nothing

  , primStepEntry $(textNameLit 'GHC.Prim.writeWordArray#) $ \case
      PrimStepContext{..}
        | [PrimVal _mbaTy _  [baV]
          ,iV,wV
          ,PrimVal rwTy _ _
          ] <- args
        , [ba,i] <- intLiterals' [baV,iV]
        , [w] <- wordLiterals' [wV]
        -> let Just (Literal (ByteArrayLiteral ba1)) =
                  primLookup (fromInteger ba) mach
               !(I# i') = fromInteger i
               !(W# w') = fromIntegral w
               ba2 = unsafeDupablePerformIO $ do
                      BA.MutableByteArray mba <- BA.unsafeThawByteArray ba1
                      svoid (writeWordArray# mba i' w')
                      BA.unsafeFreezeByteArray (BA.MutableByteArray mba)
               ba3 = Literal (ByteArrayLiteral ba2)
           in Just . setTerm (Prim rwTy) $ primUpdate (fromInteger ba) ba3 mach
      _ -> Nothing

  , primStepEntry $(textNameLit 'GHC.Prim.unsafeFreezeByteArray#) $ \case
      PrimStepContext{..}
        | [PrimVal _mbaTy _ [baV]
          ,PrimVal rwTy _ _
          ] <- args
        , [ba] <-  intLiterals' [baV]
        -> let (_,tyView -> TyConApp tupTcNm tyArgs) = splitFunForallTy ty
               (Just tupTc) = UniqMap.lookup tupTcNm tcm
               [tupDc] = tyConDataCons tupTc
               Just ba' = primLookup (fromInteger ba) mach
           in  reduce $ mkApps (Data tupDc) (map Right tyArgs ++
                          [Left (Prim rwTy)
                          ,Left ba'])
      _ -> Nothing

  , primStepEntry $(textNameLit 'GHC.Prim.sizeofByteArray#) $ \case
      PrimStepContext{..}
        | [Lit (ByteArrayLiteral ba)] <- args
        -> reduce (Literal (IntLiteral (toInteger (BA.sizeofByteArray ba))))
      _ -> Nothing

  , primStepEntry $(textNameLit 'GHC.Prim.indexWordArray#) $ \case
      PrimStepContext{..}
        | [Lit (ByteArrayLiteral (BA.ByteArray ba)),iV] <- args
        , [i] <- intLiterals' [iV]
        -> let !(I# i') = fromInteger i
               !w       = indexWordArray# ba i'
           in  reduce (Literal (WordLiteral (toInteger (W# w))))
      _ -> Nothing


  -- XXX: Primitive does not exist?
  , primStepEntry "GHC.Prim.getSizeofMutBigNat#" $ \case
      PrimStepContext{..}
        | [PrimVal _mbaTy _ [baV]
          ,PrimVal rwTy _ _
          ] <- args
        , [ba] <- intLiterals' [baV]
        -> let (_,tyView -> TyConApp tupTcNm tyArgs) = splitFunForallTy ty
               (Just tupTc) = UniqMap.lookup tupTcNm tcm
               [tupDc] = tyConDataCons tupTc
               Just (Literal (ByteArrayLiteral ba')) = primLookup (fromInteger ba) mach
               lit = Literal (IntLiteral (toInteger (BA.sizeofByteArray ba')))
           in  reduce $ mkApps (Data tupDc) (map Right tyArgs ++
                          [Left (Prim rwTy)
                          ,Left lit])
      _ -> Nothing

  , primStepEntry $(textNameLit 'GHC.Prim.resizeMutableByteArray#) $ \case
      PrimStepContext{..}
        | [PrimVal mbaTy _ [baV]
          ,iV
          ,PrimVal rwTy _ _
          ] <- args
        , [ba,i] <- intLiterals' [baV,iV]
        -> let (_,tyView -> TyConApp tupTcNm tyArgs) = splitFunForallTy ty
               (Just tupTc) = UniqMap.lookup tupTcNm tcm
               [tupDc] = tyConDataCons tupTc
               p = primCount mach
               Just (Literal (ByteArrayLiteral ba1))
                = primLookup (fromInteger ba) mach
               !(I# i') = fromInteger i
               ba2 = unsafeDupablePerformIO $ do
                       BA.MutableByteArray mba <- BA.unsafeThawByteArray ba1
                       mba' <- IO (\s -> case resizeMutableByteArray# mba i' s of
                                     (# s', mba' #) -> (# s', BA.MutableByteArray mba' #))
                       BA.unsafeFreezeByteArray mba'
               ba3 = Literal (ByteArrayLiteral ba2)
               newE = mkApps (Data tupDc) (map Right tyArgs ++
                        [Left (Prim rwTy)
                        ,Left (mkApps (Prim mbaTy)
                                      [Left (Literal . IntLiteral $ toInteger p)])
                        ])
           in Just . setTerm newE $ primInsert p ba3 mach
      _ -> Nothing

  , primStepEntry $(textNameLit 'GHC.Prim.shrinkMutableByteArray#) $ \case
      PrimStepContext{..}
        | [PrimVal _mbaTy _ [baV]
          ,lenV
          ,PrimVal rwTy _ _
          ] <- args
        , [ba,len] <- intLiterals' [baV,lenV]
        -> let Just (Literal (ByteArrayLiteral ba1)) =
                  primLookup (fromInteger ba) mach
               !(I# len') = fromInteger len
               ba2 = unsafeDupablePerformIO $ do
                      BA.MutableByteArray mba <- BA.unsafeThawByteArray ba1
                      svoid (shrinkMutableByteArray# mba len')
                      BA.unsafeFreezeByteArray (BA.MutableByteArray mba)
               ba3 = Literal (ByteArrayLiteral ba2)
           in Just . setTerm (Prim rwTy) $ primUpdate (fromInteger ba) ba3 mach
      _ -> Nothing

  , primStepEntry $(textNameLit 'GHC.Prim.copyByteArray#) $ \case
      PrimStepContext{..}
        | [Lit (ByteArrayLiteral (BA.ByteArray src_ba))
          ,src_offV
          ,PrimVal _mbaTy _ [dst_mbaV]
          ,dst_offV, nV
          ,PrimVal rwTy _ _
          ] <- args
        , [src_off,dst_mba,dst_off,n] <- intLiterals' [src_offV,dst_mbaV,dst_offV,nV]
        -> let Just (Literal (ByteArrayLiteral dst_ba)) =
                  primLookup (fromInteger dst_mba) mach
               !(I# src_off') = fromInteger src_off
               !(I# dst_off') = fromInteger dst_off
               !(I# n')       = fromInteger n
               ba2 = unsafeDupablePerformIO $ do
                      BA.MutableByteArray dst_mba1 <- BA.unsafeThawByteArray dst_ba
                      svoid (copyByteArray# src_ba src_off' dst_mba1 dst_off' n')
                      BA.unsafeFreezeByteArray (BA.MutableByteArray dst_mba1)
               ba3 = Literal (ByteArrayLiteral ba2)
           in Just . setTerm (Prim rwTy) $ primUpdate (fromInteger dst_mba) ba3 mach
      _ -> Nothing

  , primStepEntry $(textNameLit 'GHC.Prim.readWordArray#) $ \case
      PrimStepContext{..}
        | [PrimVal _mbaTy _  [baV]
          ,offV
          ,PrimVal rwTy _ _
          ] <- args
        , [ba,off] <- intLiterals' [baV,offV]
        -> let (_,tyView -> TyConApp tupTcNm tyArgs) = splitFunForallTy ty
               (Just tupTc) = UniqMap.lookup tupTcNm tcm
               [tupDc] = tyConDataCons tupTc
               Just (Literal (ByteArrayLiteral ba1)) =
                  primLookup (fromInteger ba) mach
               !(I# off') = fromInteger off
               w = unsafeDupablePerformIO $ do
                      BA.MutableByteArray mba <- BA.unsafeThawByteArray ba1
                      IO (\s -> case readWordArray# mba off' s of
                            (# s', w' #) -> (# s',  W# w' #))
               newE = mkApps (Data tupDc) (map Right tyArgs ++
                        [Left (Prim rwTy)
                        ,Left (Literal (WordLiteral (toInteger w)))
                        ])
           in reduce newE
      _ -> Nothing

  , primStepEntry $(textNameLit 'GHC.Prim.copyAddrToByteArray#) $ \case
      PrimStepContext{..}
        | [ Lit (StringLiteral addr)
          , PrimVal _mbaTy _ [dst_mbaV]
          , offV, lenV
          , PrimVal rwTy _ _
          ] <- args
        , [off,len,dst_mba] <- intLiterals' [offV, lenV, dst_mbaV]
        -> let Just (Literal (ByteArrayLiteral dst_ba)) =
                  primLookup (fromInteger dst_mba) mach
               !(I# off') = fromInteger off
               !(I# len') = fromInteger len
               !(BS.PS (ForeignPtr addr' _) _ _) = BS.packChars addr
               ba2 = unsafeDupablePerformIO $ do
                        BA.MutableByteArray dst_mba1 <- BA.unsafeThawByteArray dst_ba
                        svoid (copyAddrToByteArray# addr' dst_mba1 off' len')
                        BA.unsafeFreezeByteArray (BA.MutableByteArray dst_mba1)
               ba3 = Literal (ByteArrayLiteral ba2)
            in Just . setTerm (Prim rwTy) $ primUpdate (fromInteger dst_mba) ba3 mach
      _ -> Nothing


-- decodeFloat_Int# :: Float# -> (#Int#, Int##)
  , primStepEntry $(textNameLit 'GHC.Prim.decodeFloat_Int#) $ \case
      PrimStepContext{..} | [i] <- floatLiterals' args
        -> let (_,tyView -> TyConApp tupTcNm tyArgs) = splitFunForallTy ty
               (Just tupTc) = UniqMap.lookup tupTcNm tcm
               [tupDc] = tyConDataCons tupTc
               !(F# a) = castWord32ToFloat i
               !(# p, q #) = decodeFloat_Int# a
           in reduce $
              mkApps (Data tupDc) (map Right tyArgs ++
                       [ Left (Literal . IntLiteral  . toInteger $ I# p)
                       , Left (Literal . IntLiteral  . toInteger $ I# q)])
      _ -> Nothing

  , primStepEntry $(textNameLit 'GHC.Prim.tagToEnum#) $ \case
      PrimStepContext{..}
        | [ConstTy (TyCon tcN)] <- tys
        , [Lit (IntLiteral i)]  <- args
        -> let dc = do { tc <- UniqMap.lookup tcN tcm
                       ; let dcs = tyConDataCons tc
                       ; List.find ((== (i+1)) . toInteger . dcTag) dcs
                       }
           in (\e -> setTerm (Data e) mach) <$> dc
      _ -> Nothing

#if MIN_VERSION_ghc(9,10,0)
  , primStepEntry $(textNameLit 'GHC.Prim.dataToTagSmall#) $ \case
      PrimStepContext{..}
        | [DC dc _] <- args
        -> reduce (Literal (IntLiteral (toInteger (dcTag dc - 1))))
      _ -> Nothing

  , primStepEntry $(textNameLit 'GHC.Prim.dataToTagLarge#) $ \case
      PrimStepContext{..}
        | [DC dc _] <- args
        -> reduce (Literal (IntLiteral (toInteger (dcTag dc - 1))))
      _ -> Nothing

#else
  , primStepEntry $(textNameLit 'GHC.Prim.dataToTag#) $ \case
      PrimStepContext{..}
        | [DC dc _] <- args
        -> reduce (Literal (IntLiteral (toInteger (dcTag dc - 1))))
      _ -> Nothing

#endif
  ]
