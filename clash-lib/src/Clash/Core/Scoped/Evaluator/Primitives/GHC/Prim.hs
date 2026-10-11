{-|
  Copyright   :  (C) 2013-2016, University of Twente,
                     2016-2017, Myrtle Software Ltd,
                     2017-2022, Google Inc.,
                     2017-2026, QBayLogic B.V.
  License     :  BSD2 (see the file LICENSE)
  Maintainer  :  QBayLogic B.V. <devops@qbaylogic.com>

  Reductions of the primitives of "GHC.Prim" in the scoped evaluator: ports
  of @Clash.GHC.Evaluator.Primitives.GHC.Prim@. See
  "Clash.Core.Scoped.Evaluator.Primitives" for how to port them.
-}

{-# LANGUAGE CPP #-}
{-# LANGUAGE GADTs #-}
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

module Clash.Core.Scoped.Evaluator.Primitives.GHC.Prim
  ( primitives
  ) where

import Data.Bits
import Data.Char (chr, ord)
import qualified Data.Primitive.ByteArray as BA
import Data.Text (Text)
import GHC.Float
import GHC.Int
import GHC.Prim
import GHC.Word
import System.IO.Unsafe (unsafeDupablePerformIO)

import Clash.Core.Literal (Literal (..))
import Clash.Util (textNameLit)

import Clash.Core.Scoped.Evaluator.Primitives.Util
import Clash.Core.Scoped.Evaluator.Types
import Clash.Core.Scoped.Syntax

primitives :: [(Text, PrimReduction)]
primitives =
-----------------
-- GHC.Prim.Char#
-----------------
  [ primStepEntry $(textNameLit 'GHC.Prim.gtChar#) $ \case
      PrimStepContext{..} | Just (i,j) <- charLiterals args
        -> reduce (boolToIntLiteral (i > j))
      _ -> Nothing

  , liftLit2 $(textNameLit 'GHC.Prim.geChar#) charLiteral (\i j -> boolToIntLiteral (i >= j))

  , liftLit2 $(textNameLit 'GHC.Prim.eqChar#) charLiteral (\i j -> boolToIntLiteral (i == j))

  , liftLit2 $(textNameLit 'GHC.Prim.neChar#) charLiteral (\i j -> boolToIntLiteral (i /= j))

  , liftLit2 $(textNameLit 'GHC.Prim.ltChar#) charLiteral (\i j -> boolToIntLiteral (i < j))

  , liftLit2 $(textNameLit 'GHC.Prim.leChar#) charLiteral (\i j -> boolToIntLiteral (i <= j))

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

  , liftLit2 $(textNameLit 'GHC.Prim.remInt#) intLiteral (\i j -> integerToIntLiteral (i `rem` j))

  , primStepEntry $(textNameLit 'GHC.Prim.quotRemInt#) $ \case
      PrimStepContext{..} | Just (i,j) <- intLiterals args
        -> let (q,r) = quotRem i j
           in  reduce $ mkResultCon te sc ty tys
                 [ catchDivByZero (integerToIntLiteral q)
                 , catchDivByZero (integerToIntLiteral r) ]
      _ -> Nothing

  , liftLit2 $(textNameLit 'GHC.Prim.andI#) intLiteral (\i j -> integerToIntLiteral (i .&. j))

  , liftLit2 $(textNameLit 'GHC.Prim.orI#) intLiteral (\i j -> integerToIntLiteral (i .|. j))

  , liftLit2 $(textNameLit 'GHC.Prim.xorI#) intLiteral (\i j -> integerToIntLiteral (i `xor` j))

  , liftLit1 $(textNameLit 'GHC.Prim.notI#) intLiteral (integerToIntLiteral . complement)

  , primStepEntry $(textNameLit 'GHC.Prim.negateInt#) $ \case
      PrimStepContext{..}
        | [Lit (IntLiteral i)] <- args
        -> reduce (integerToIntLiteral (negate i))
      _ -> Nothing

  , primStepEntry $(textNameLit 'GHC.Prim.addIntC#) $ \case
      PrimStepContext{..} | Just (i,j) <- intLiterals args
        -> let !(I# a)  = fromInteger i
               !(I# b)  = fromInteger j
               !(# d, c #) = addIntC# a b
           in  reduce $
               mkResultCon te sc ty tys
                       [ (Literal . IntLiteral . toInteger $ I# d)
                       , (Literal . IntLiteral . toInteger $ I# c)]
      _ -> Nothing

  , primStepEntry $(textNameLit 'GHC.Prim.subIntC#) $ \case
      PrimStepContext{..} | Just (i,j) <- intLiterals args
        -> let !(I# a)  = fromInteger i
               !(I# b)  = fromInteger j
               !(# d, c #) = subIntC# a b
           in  reduce $
               mkResultCon te sc ty tys
                       [ (Literal . IntLiteral . toInteger $ I# d)
                       , (Literal . IntLiteral . toInteger $ I# c)]
      _ -> Nothing

  , liftLit2 $(textNameLit '(GHC.Prim.>#)) intLiteral (\i j -> boolToIntLiteral (i > j))

  , liftLit2 $(textNameLit '(GHC.Prim.>=#)) intLiteral (\i j -> boolToIntLiteral (i >= j))

  , primStepEntry $(textNameLit '(GHC.Prim.==#)) $ \case
      PrimStepContext{..} | Just (i,j) <- intLiterals args
        -> reduce (boolToIntLiteral (i == j))
      _ -> Nothing

  , liftLit2 $(textNameLit '(GHC.Prim./=#)) intLiteral (\i j -> boolToIntLiteral (i /= j))

  , liftLit2 $(textNameLit '(GHC.Prim.<#)) intLiteral (\i j -> boolToIntLiteral (i < j))

  , liftLit2 $(textNameLit '(GHC.Prim.<=#)) intLiteral (\i j -> boolToIntLiteral (i <= j))

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
  , liftLit2 $(textNameLit 'GHC.Prim.plusWord#) wordLiteral (\i j -> integerToWordLiteral (i+j))

  , primStepEntry $(textNameLit 'GHC.Prim.subWordC#) $ \case
      PrimStepContext{..} | Just (i,j) <- wordLiterals args
        -> let !(W# a)  = fromInteger i
               !(W# b)  = fromInteger j
               !(# d, c #) = subWordC# a b
           in  reduce $
               mkResultCon te sc ty tys
                       [ (Literal . WordLiteral . toInteger $ W# d)
                       , (Literal . IntLiteral . toInteger $ I# c)]
      _ -> Nothing

  , primStepEntry $(textNameLit 'GHC.Prim.plusWord2#) $ \case
      PrimStepContext{..} | Just (i,j) <- wordLiterals args
        -> let !(W# a)  = fromInteger i
               !(W# b)  = fromInteger j
               !(# h', l #) = plusWord2# a b
           in  reduce $
               mkResultCon te sc ty tys
                       [ (Literal . WordLiteral . toInteger $ W# h')
                       , (Literal . WordLiteral . toInteger $ W# l)]
      _ -> Nothing

  , liftLit2 $(textNameLit 'GHC.Prim.minusWord#) wordLiteral (\i j -> integerToWordLiteral (i-j))

  , liftLit2 $(textNameLit 'GHC.Prim.timesWord#) wordLiteral (\i j -> integerToWordLiteral (i*j))

  , primStepEntry $(textNameLit 'GHC.Prim.timesWord2#) $ \case
      PrimStepContext{..} | Just (i,j) <- wordLiterals args
        -> let !(W# a)  = fromInteger i
               !(W# b)  = fromInteger j
               !(# h', l #) = timesWord2# a b
           in  reduce $
               mkResultCon te sc ty tys
                       [ (Literal . WordLiteral . toInteger $ W# h')
                       , (Literal . WordLiteral . toInteger $ W# l)]
      _ -> Nothing

  , liftLit2 $(textNameLit 'GHC.Prim.quotWord#) wordLiteral (\i j -> integerToWordLiteral (i `quot` j))

  , liftLit2 $(textNameLit 'GHC.Prim.remWord#) wordLiteral (\i j -> integerToWordLiteral (i `rem` j))

  , primStepEntry $(textNameLit 'GHC.Prim.quotRemWord#) $ \case
      PrimStepContext{..} | Just (i,j) <- wordLiterals args
        -> let (q,r)   = quotRem i j
               ret     = mkResultCon te sc ty tys
                        [catchDivByZero (integerToWordLiteral q)
                        ,catchDivByZero (integerToWordLiteral r)]
           in  reduce ret
      _ -> Nothing

  , primStepEntry $(textNameLit 'GHC.Prim.quotRemWord2#) $ \case
      PrimStepContext{..} | [i,j,k'] <- wordLiterals' args
        -> let !(W# a)  = fromInteger i
               !(W# b)  = fromInteger j
               !(W# c)  = fromInteger k'
               !(# x, y #) = quotRemWord2# a b c
           in  reduce $
               mkResultCon te sc ty tys
                       [ catchDivByZero (Literal . WordLiteral . toInteger $ W# x)
                       , catchDivByZero (Literal . WordLiteral . toInteger $ W# y)]
      _ -> Nothing

  , liftLit2 $(textNameLit 'GHC.Prim.and#) wordLiteral (\i j -> integerToWordLiteral (i .&. j))

  , liftLit2 $(textNameLit 'GHC.Prim.or#) wordLiteral (\i j -> integerToWordLiteral (i .|. j))

  , liftLit2 $(textNameLit 'GHC.Prim.xor#) wordLiteral (\i j -> integerToWordLiteral (i `xor` j))

  , liftLit1 $(textNameLit 'GHC.Prim.not#) wordLiteral (\i -> integerToWordLiteral (complement i))

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

  , liftLit2 $(textNameLit 'GHC.Prim.gtWord#) wordLiteral (\i j -> boolToIntLiteral (i > j))

  , liftLit2 $(textNameLit 'GHC.Prim.geWord#) wordLiteral (\i j -> boolToIntLiteral (i >= j))

  , liftLit2 $(textNameLit 'GHC.Prim.eqWord#) wordLiteral (\i j -> boolToIntLiteral (i == j))

  , liftLit2 $(textNameLit 'GHC.Prim.neWord#) wordLiteral (\i j -> boolToIntLiteral (i /= j))

  , liftLit2 $(textNameLit 'GHC.Prim.ltWord#) wordLiteral (\i j -> boolToIntLiteral (i < j))

  , liftLit2 $(textNameLit 'GHC.Prim.leWord#) wordLiteral (\i j -> boolToIntLiteral (i <= j))

  , liftLit1 $(textNameLit 'GHC.Prim.popCnt8#) wordLiteral (\i -> integerToWordLiteral . toInteger . popCount . (fromInteger :: Integer -> Word8) $ i)

  , liftLit1 $(textNameLit 'GHC.Prim.popCnt16#) wordLiteral (\i -> integerToWordLiteral . toInteger . popCount . (fromInteger :: Integer -> Word16) $ i)

  , liftLit1 $(textNameLit 'GHC.Prim.popCnt32#) wordLiteral (\i -> integerToWordLiteral . toInteger . popCount . (fromInteger :: Integer -> Word32) $ i)

  , liftLit1 $(textNameLit 'GHC.Prim.popCnt64#) word64Literal (\i -> integerToWordLiteral . toInteger . popCount . (fromInteger :: Integer -> Word64) $ i)

  , liftLit1 $(textNameLit 'GHC.Prim.popCnt#) wordLiteral (\i -> integerToWordLiteral . toInteger . popCount . (fromInteger :: Integer -> Word) $ i)

  , liftLit1 $(textNameLit 'GHC.Prim.clz8#) wordLiteral (\i -> integerToWordLiteral . toInteger . countLeadingZeros . (fromInteger :: Integer -> Word8) $ i)

  , liftLit1 $(textNameLit 'GHC.Prim.clz16#) wordLiteral (\i -> integerToWordLiteral . toInteger . countLeadingZeros . (fromInteger :: Integer -> Word16) $ i)

  , liftLit1 $(textNameLit 'GHC.Prim.clz32#) wordLiteral (\i -> integerToWordLiteral . toInteger . countLeadingZeros . (fromInteger :: Integer -> Word32) $ i)

  , liftLit1 $(textNameLit 'GHC.Prim.clz64#) word64Literal (\i -> integerToWordLiteral . toInteger . countLeadingZeros . (fromInteger :: Integer -> Word64) $ i)

  , liftLit1 $(textNameLit 'GHC.Prim.clz#) wordLiteral (\i -> integerToWordLiteral . toInteger . countLeadingZeros . (fromInteger :: Integer -> Word) $ i)

  , liftLit1 $(textNameLit 'GHC.Prim.ctz8#) wordLiteral (\i -> integerToWordLiteral . toInteger . countTrailingZeros . (fromInteger :: Integer -> Word) $ i .&. (bit 8 - 1))

  , liftLit1 $(textNameLit 'GHC.Prim.ctz16#) wordLiteral (\i -> integerToWordLiteral . toInteger . countTrailingZeros . (fromInteger :: Integer -> Word) $ i .&. (bit 16 - 1))

  , liftLit1 $(textNameLit 'GHC.Prim.ctz32#) wordLiteral (\i -> integerToWordLiteral . toInteger . countTrailingZeros . (fromInteger :: Integer -> Word) $ i .&. (bit 32 - 1))

  , liftLit1 $(textNameLit 'GHC.Prim.ctz64#) word64Literal (\i -> integerToWordLiteral . toInteger . countTrailingZeros . (fromInteger :: Integer -> Word64) $ i .&. (bit 64 - 1))

  , liftLit1 $(textNameLit 'GHC.Prim.ctz#) wordLiteral (\i -> integerToWordLiteral . toInteger . countTrailingZeros . (fromInteger :: Integer -> Word) $ i)

  , liftLit1 $(textNameLit 'GHC.Prim.byteSwap16#) wordLiteral (\i -> integerToWordLiteral . toInteger . byteSwap16 . (fromInteger :: Integer -> Word16) $ i)

  , liftLit1 $(textNameLit 'GHC.Prim.byteSwap32#) wordLiteral (\i -> integerToWordLiteral . toInteger . byteSwap32 . (fromInteger :: Integer -> Word32) $ i)

  , liftLit1 $(textNameLit 'GHC.Prim.byteSwap64#) word64Literal (\i -> integerToWordLiteral . toInteger . byteSwap64 . (fromInteger :: Integer -> Word64) $ i)

  , primStepEntry $(textNameLit 'GHC.Prim.byteSwap#) $ \case
      PrimStepContext{..} | [i] <- wordLiterals' args -- assume 64bits
        -> reduce . integerToWordLiteral . toInteger . byteSwap64 . (fromInteger :: Integer -> Word64) $ i
      _ -> Nothing

  , primStepEntry $(textNameLit 'GHC.Prim.bitReverse#) $ \case
      PrimStepContext{..} | [i] <- wordLiterals' args
        -> reduce . integerToWordLiteral . toInteger . bitReverse64 . fromInteger $ i -- assume 64bits
      _ -> Nothing

  , liftLit1 $(textNameLit 'GHC.Prim.bitReverse8#) wordLiteral (\i -> integerToWordLiteral . toInteger . bitReverse8 . fromInteger $ i)

  , liftLit1 $(textNameLit 'GHC.Prim.bitReverse16#) wordLiteral (\i -> integerToWordLiteral . toInteger . bitReverse16 . fromInteger $ i)

  , liftLit1 $(textNameLit 'GHC.Prim.bitReverse32#) wordLiteral (\i -> integerToWordLiteral . toInteger . bitReverse32 . fromInteger $ i)

  , liftLit1 $(textNameLit 'GHC.Prim.bitReverse64#) word64Literal (\i -> integerToWordLiteral . toInteger . bitReverse64 . fromInteger $ i)

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


-------------------
-- GHC.Prim.Double#
-------------------
  , primStepEntry $(textNameLit '(GHC.Prim.+##)) $ \case
      PrimStepContext{..}  | Just r <- liftDDD (+##)  args
        -> reduce r
      _ -> Nothing

----------------------
-- GHC.Prim.ByteArray#
----------------------
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
           in Just . stay . setTerm (Prim rwTy) $ primUpdate (fromInteger ba) ba3 mach
      _ -> Nothing

  , primStepEntry $(textNameLit 'GHC.Prim.unsafeFreezeByteArray#) $ \case
      PrimStepContext{..}
        | [PrimVal _mbaTy _ [baV]
          ,PrimVal rwTy _ _
          ] <- args
        , [ba] <-  intLiterals' [baV]
        -> let Just ba' = primLookup (fromInteger ba) mach
           in  reduce $ mkResultCon te sc ty tys [Prim rwTy, ba']
      _ -> Nothing

  , primStepEntry $(textNameLit 'GHC.Prim.sizeofByteArray#) $ \case
      PrimStepContext{..}
        | [Lit (ByteArrayLiteral ba)] <- args
        -> reduce (Literal (IntLiteral (toInteger (BA.sizeofByteArray ba))))
      _ -> Nothing
  ]
