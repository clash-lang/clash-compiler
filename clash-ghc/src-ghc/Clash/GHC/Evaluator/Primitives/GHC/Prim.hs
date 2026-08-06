{-|
  Copyright   :  (C) 2013-2016, University of Twente,
                     2016-2017, Myrtle Software Ltd,
                     2017-2022, Google Inc.,
                     2017-2026, QBayLogic B.V.
  License     :  BSD2 (see the file LICENSE)
  Maintainer  :  QBayLogic B.V. <devops@qbaylogic.com>
-}

{-# LANGUAGE CPP #-}
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
  [ ( $(textNameLit 'GHC.Prim.gtChar#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} | Just (i,j) <- charLiterals args
            -> reduce (boolToIntLiteral (i > j))
          _ -> Nothing
    )
  , ( $(textNameLit 'GHC.Prim.geChar#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} | Just (i,j) <- charLiterals args
            -> reduce (boolToIntLiteral (i >= j))
          _ -> Nothing
    )
  , ( $(textNameLit 'GHC.Prim.eqChar#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} | Just (i,j) <- charLiterals args
            -> reduce (boolToIntLiteral (i == j))
          _ -> Nothing
    )
  , ( $(textNameLit 'GHC.Prim.neChar#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} | Just (i,j) <- charLiterals args
            -> reduce (boolToIntLiteral (i /= j))
          _ -> Nothing
    )
  , ( $(textNameLit 'GHC.Prim.ltChar#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} | Just (i,j) <- charLiterals args
            -> reduce (boolToIntLiteral (i < j))
          _ -> Nothing
    )
  , ( $(textNameLit 'GHC.Prim.leChar#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} | Just (i,j) <- charLiterals args
            -> reduce (boolToIntLiteral (i <= j))
          _ -> Nothing
    )
  , ( $(textNameLit 'GHC.Prim.ord#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} | [i] <- charLiterals' args
            -> reduce (integerToIntLiteral (toInteger $ ord i))
          _ -> Nothing
    )

----------------
-- GHC.Prim.Int#
----------------
  , ( $(textNameLit '(GHC.Prim.+#))
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} | Just (i,j) <- intLiterals args
            -> reduce (integerToIntLiteral (i+j))
          _ -> Nothing
    )
  , ( $(textNameLit '(GHC.Prim.-#))
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} | Just (i,j) <- intLiterals args
            -> reduce (integerToIntLiteral (i-j))
          _ -> Nothing
    )
  , ( $(textNameLit '(GHC.Prim.*#))
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} | Just (i,j) <- intLiterals args
            -> reduce (integerToIntLiteral (i*j))
          _ -> Nothing
    )

  , ( $(textNameLit 'GHC.Prim.mulIntMayOflo#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} | Just (i,j) <- intLiterals  args
            -> let !(I# a)  = fromInteger i
                   !(I# b)  = fromInteger j
                   c :: Int#
                   c = mulIntMayOflo# a b
               in  reduce (integerToIntLiteral (toInteger $ I# c))
          _ -> Nothing
    )

  , ( $(textNameLit 'GHC.Prim.quotInt#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} | Just (i,j) <- intLiterals args
            -> reduce $ catchDivByZero (integerToIntLiteral (i `quot` j))
          _ -> Nothing
    )
  , ( $(textNameLit 'GHC.Prim.remInt#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} | Just (i,j) <- intLiterals args
            -> reduce $ catchDivByZero (integerToIntLiteral (i `rem` j))
          _ -> Nothing
    )
  , ( $(textNameLit 'GHC.Prim.quotRemInt#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
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
    )

  , ( $(textNameLit 'GHC.Prim.andI#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} | Just (i,j) <- intLiterals args
            -> reduce (integerToIntLiteral (i .&. j))
          _ -> Nothing
    )
  , ( $(textNameLit 'GHC.Prim.orI#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} | Just (i,j) <- intLiterals args
            -> reduce (integerToIntLiteral (i .|. j))
          _ -> Nothing
    )
  , ( $(textNameLit 'GHC.Prim.xorI#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} | Just (i,j) <- intLiterals args
            -> reduce (integerToIntLiteral (i `xor` j))
          _ -> Nothing
    )
  , ( $(textNameLit 'GHC.Prim.notI#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} | [i] <- intLiterals' args
            -> reduce (integerToIntLiteral (complement i))
          _ -> Nothing
    )

  , ( $(textNameLit 'GHC.Prim.negateInt#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..}
            | [Lit (IntLiteral i)] <- args
            -> reduce (integerToIntLiteral (negate i))
          _ -> Nothing
    )

  , ( $(textNameLit 'GHC.Prim.addIntC#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
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
    )
  , ( $(textNameLit 'GHC.Prim.subIntC#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
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
    )

  , ( $(textNameLit '(GHC.Prim.>#))
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} | Just (i,j) <- intLiterals args
            -> reduce (boolToIntLiteral (i > j))
          _ -> Nothing
    )
  , ( $(textNameLit '(GHC.Prim.>=#))
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} | Just (i,j) <- intLiterals args
            -> reduce (boolToIntLiteral (i >= j))
          _ -> Nothing
    )
  , ( $(textNameLit '(GHC.Prim.==#))
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} | Just (i,j) <- intLiterals args
            -> reduce (boolToIntLiteral (i == j))
          _ -> Nothing
    )
  , ( $(textNameLit '(GHC.Prim./=#))
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} | Just (i,j) <- intLiterals args
            -> reduce (boolToIntLiteral (i /= j))
          _ -> Nothing
    )
  , ( $(textNameLit '(GHC.Prim.<#))
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..}| Just (i,j) <- intLiterals args
            -> reduce (boolToIntLiteral (i < j))
          _ -> Nothing
    )
  , ( $(textNameLit '(GHC.Prim.<=#))
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} | Just (i,j) <- intLiterals args
            -> reduce (boolToIntLiteral (i <= j))
          _ -> Nothing
    )

  , ( $(textNameLit 'GHC.Prim.chr#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} | [i] <- intLiterals' args
            -> reduce (catchErrorCall (charToCharLiteral (chr $ fromInteger i)))
          _ -> Nothing
    )

  , ( $(textNameLit 'GHC.Prim.int2Word#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..}
            | [Lit (IntLiteral i)] <- args
            -> reduce . Literal . WordLiteral . toInteger $ (fromInteger :: Integer -> Word) i -- for overflow behavior
          _ -> Nothing
    )

  , ( $(textNameLit 'GHC.Prim.int2Float#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..}
            | [Lit (IntLiteral i)] <- args
            -> reduce . Literal . FloatLiteral  . castFloatToWord32 $ fromInteger i
          _ -> Nothing
    )
  , ( $(textNameLit 'GHC.Prim.int2Double#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..}
            | [Lit (IntLiteral i)] <- args
            -> reduce . Literal . DoubleLiteral . castDoubleToWord64 $ fromInteger i
          _ -> Nothing
    )

  , ( $(textNameLit 'GHC.Prim.word2Float#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..}
            | [Lit (WordLiteral i)] <- args
            -> reduce . Literal . FloatLiteral  . castFloatToWord32 $ fromInteger i
          _ -> Nothing
    )
  , ( $(textNameLit 'GHC.Prim.word2Double#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..}
            | [Lit (WordLiteral i)] <- args
            -> reduce . Literal . DoubleLiteral . castDoubleToWord64 $ fromInteger i
          _ -> Nothing
    )

  , ( $(textNameLit 'GHC.Prim.uncheckedIShiftL#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..}
            | [ Lit (IntLiteral i)
              , Lit (IntLiteral s)
              ] <- args
            -> reduce (integerToIntLiteral (i `shiftL` fromInteger s))
          _ -> Nothing
    )
  , ( $(textNameLit 'GHC.Prim.uncheckedIShiftRA#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..}
            | [ Lit (IntLiteral i)
              , Lit (IntLiteral s)
              ] <- args
            -> reduce (integerToIntLiteral (i `shiftR` fromInteger s))
          _ -> Nothing
    )
  , ( $(textNameLit 'GHC.Prim.uncheckedIShiftRL#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} | Just (i,j) <- intLiterals args
            -> let !(I# a)  = fromInteger i
                   !(I# b)  = fromInteger j
                   c :: Int#
                   c = uncheckedIShiftRL# a b
               in  reduce (integerToIntLiteral (toInteger $ I# c))
          _ -> Nothing
    )

-----------------
-- GHC.Prim.Word#
-----------------
  , ( $(textNameLit 'GHC.Prim.plusWord#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} | Just (i,j) <- wordLiterals args
            -> reduce (integerToWordLiteral (i+j))
          _ -> Nothing
    )

  , ( $(textNameLit 'GHC.Prim.subWordC#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
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
    )

  , ( $(textNameLit 'GHC.Prim.plusWord2#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
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
    )

  , ( $(textNameLit 'GHC.Prim.minusWord#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} | Just (i,j) <- wordLiterals args
            -> reduce (integerToWordLiteral (i-j))
          _ -> Nothing
    )
  , ( $(textNameLit 'GHC.Prim.timesWord#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} | Just (i,j) <- wordLiterals args
            -> reduce (integerToWordLiteral (i*j))
          _ -> Nothing
    )

  , ( $(textNameLit 'GHC.Prim.timesWord2#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
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
    )

  , ( $(textNameLit 'GHC.Prim.quotWord#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} | Just (i,j) <- wordLiterals args
            -> reduce $ catchDivByZero (integerToWordLiteral (i `quot` j))
          _ -> Nothing
    )
  , ( $(textNameLit 'GHC.Prim.remWord#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} | Just (i,j) <- wordLiterals args
            -> reduce $ catchDivByZero (integerToWordLiteral (i `rem` j))
          _ -> Nothing
    )
  , ( $(textNameLit 'GHC.Prim.quotRemWord#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
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
    )
  , ( $(textNameLit 'GHC.Prim.quotRemWord2#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
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
    )

  , ( $(textNameLit 'GHC.Prim.and#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} | Just (i,j) <- wordLiterals args
            -> reduce (integerToWordLiteral (i .&. j))
          _ -> Nothing
    )
  , ( $(textNameLit 'GHC.Prim.or#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} | Just (i,j) <- wordLiterals args
            -> reduce (integerToWordLiteral (i .|. j))
          _ -> Nothing
    )
  , ( $(textNameLit 'GHC.Prim.xor#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} | Just (i,j) <- wordLiterals args
            -> reduce (integerToWordLiteral (i `xor` j))
          _ -> Nothing
    )
  , ( $(textNameLit 'GHC.Prim.not#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} | [i] <- wordLiterals' args
            -> reduce (integerToWordLiteral (complement i))
          _ -> Nothing
    )

  , ( $(textNameLit 'GHC.Prim.uncheckedShiftL#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..}
            | [ Lit (WordLiteral w)
              , Lit (IntLiteral  i)
              ] <- args
            -> reduce (Literal (WordLiteral (w `shiftL` fromInteger i)))
          _ -> Nothing
    )
  , ( $(textNameLit 'GHC.Prim.uncheckedShiftRL#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..}
            | [ Lit (WordLiteral w)
              , Lit (IntLiteral  i)
              ] <- args
            -> reduce (Literal (WordLiteral (w `shiftR` fromInteger i)))
          _ -> Nothing
    )

  , ( $(textNameLit 'GHC.Prim.word2Int#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..}
            | [Lit (WordLiteral i)] <- args
            -> reduce . Literal . IntLiteral . toInteger $ (fromInteger :: Integer -> Int) i -- for overflow behavior
          _ -> Nothing
    )

  , ( $(textNameLit 'GHC.Prim.gtWord#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} | Just (i,j) <- wordLiterals args
            -> reduce (boolToIntLiteral (i > j))
          _ -> Nothing
    )
  , ( $(textNameLit 'GHC.Prim.geWord#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} | Just (i,j) <- wordLiterals args
            -> reduce (boolToIntLiteral (i >= j))
          _ -> Nothing
    )
  , ( $(textNameLit 'GHC.Prim.eqWord#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} | Just (i,j) <- wordLiterals args
            -> reduce (boolToIntLiteral (i == j))
          _ -> Nothing
    )
  , ( $(textNameLit 'GHC.Prim.neWord#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} | Just (i,j) <- wordLiterals args
            -> reduce (boolToIntLiteral (i /= j))
          _ -> Nothing
    )
  , ( $(textNameLit 'GHC.Prim.ltWord#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} | Just (i,j) <- wordLiterals args
            -> reduce (boolToIntLiteral (i < j))
          _ -> Nothing
    )
  , ( $(textNameLit 'GHC.Prim.leWord#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} | Just (i,j) <- wordLiterals args
            -> reduce (boolToIntLiteral (i <= j))
          _ -> Nothing
    )

  , ( $(textNameLit 'GHC.Prim.popCnt8#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} | [i] <- wordLiterals' args
            -> reduce . integerToWordLiteral . toInteger . popCount . (fromInteger :: Integer -> Word8) $ i
          _ -> Nothing
    )
  , ( $(textNameLit 'GHC.Prim.popCnt16#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} | [i] <- wordLiterals' args
            -> reduce . integerToWordLiteral . toInteger . popCount . (fromInteger :: Integer -> Word16) $ i
          _ -> Nothing
    )
  , ( $(textNameLit 'GHC.Prim.popCnt32#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} | [i] <- wordLiterals' args
            -> reduce . integerToWordLiteral . toInteger . popCount . (fromInteger :: Integer -> Word32) $ i
          _ -> Nothing
    )
  , ( $(textNameLit 'GHC.Prim.popCnt64#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} | [i] <- word64Literals' args
            -> reduce . integerToWordLiteral . toInteger . popCount . (fromInteger :: Integer -> Word64) $ i
          _ -> Nothing
    )
  , ( $(textNameLit 'GHC.Prim.popCnt#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} | [i] <- wordLiterals' args
            -> reduce . integerToWordLiteral . toInteger . popCount . (fromInteger :: Integer -> Word) $ i
          _ -> Nothing
    )

  , ( $(textNameLit 'GHC.Prim.clz8#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} | [i] <- wordLiterals' args
            -> reduce . integerToWordLiteral . toInteger . countLeadingZeros . (fromInteger :: Integer -> Word8) $ i
          _ -> Nothing
    )
  , ( $(textNameLit 'GHC.Prim.clz16#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} | [i] <- wordLiterals' args
            -> reduce . integerToWordLiteral . toInteger . countLeadingZeros . (fromInteger :: Integer -> Word16) $ i
          _ -> Nothing
    )
  , ( $(textNameLit 'GHC.Prim.clz32#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} | [i] <- wordLiterals' args
            -> reduce . integerToWordLiteral . toInteger . countLeadingZeros . (fromInteger :: Integer -> Word32) $ i
          _ -> Nothing
    )
  , ( $(textNameLit 'GHC.Prim.clz64#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} | [i] <- word64Literals' args
            -> reduce . integerToWordLiteral . toInteger . countLeadingZeros . (fromInteger :: Integer -> Word64) $ i
          _ -> Nothing
    )
  , ( $(textNameLit 'GHC.Prim.clz#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} | [i] <- wordLiterals' args
            -> reduce . integerToWordLiteral . toInteger . countLeadingZeros . (fromInteger :: Integer -> Word) $ i
          _ -> Nothing
    )

  , ( $(textNameLit 'GHC.Prim.ctz8#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} | [i] <- wordLiterals' args
            -> reduce . integerToWordLiteral . toInteger . countTrailingZeros . (fromInteger :: Integer -> Word) $ i .&. (bit 8 - 1)
          _ -> Nothing
    )
  , ( $(textNameLit 'GHC.Prim.ctz16#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} | [i] <- wordLiterals' args
            -> reduce . integerToWordLiteral . toInteger . countTrailingZeros . (fromInteger :: Integer -> Word) $ i .&. (bit 16 - 1)
          _ -> Nothing
    )
  , ( $(textNameLit 'GHC.Prim.ctz32#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} | [i] <- wordLiterals' args
            -> reduce . integerToWordLiteral . toInteger . countTrailingZeros . (fromInteger :: Integer -> Word) $ i .&. (bit 32 - 1)
          _ -> Nothing
    )
  , ( $(textNameLit 'GHC.Prim.ctz64#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} | [i] <- word64Literals' args
            -> reduce . integerToWordLiteral . toInteger . countTrailingZeros . (fromInteger :: Integer -> Word64) $ i .&. (bit 64 - 1)
          _ -> Nothing
    )
  , ( $(textNameLit 'GHC.Prim.ctz#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} | [i] <- wordLiterals' args
            -> reduce . integerToWordLiteral . toInteger . countTrailingZeros . (fromInteger :: Integer -> Word) $ i
          _ -> Nothing
    )

  , ( $(textNameLit 'GHC.Prim.byteSwap16#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} | [i] <- wordLiterals' args
            -> reduce . integerToWordLiteral . toInteger . byteSwap16 . (fromInteger :: Integer -> Word16) $ i
          _ -> Nothing
    )
  , ( $(textNameLit 'GHC.Prim.byteSwap32#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} | [i] <- wordLiterals' args
            -> reduce . integerToWordLiteral . toInteger . byteSwap32 . (fromInteger :: Integer -> Word32) $ i
          _ -> Nothing
    )
  , ( $(textNameLit 'GHC.Prim.byteSwap64#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} | [i] <- word64Literals' args
            -> reduce . integerToWordLiteral . toInteger . byteSwap64 . (fromInteger :: Integer -> Word64) $ i
          _ -> Nothing
    )
  , ( $(textNameLit 'GHC.Prim.byteSwap#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} | [i] <- wordLiterals' args -- assume 64bits
            -> reduce . integerToWordLiteral . toInteger . byteSwap64 . (fromInteger :: Integer -> Word64) $ i
          _ -> Nothing
    )

  , ( $(textNameLit 'GHC.Prim.bitReverse#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} | [i] <- wordLiterals' args
            -> reduce . integerToWordLiteral . toInteger . bitReverse64 . fromInteger $ i -- assume 64bits
          _ -> Nothing
    )
  , ( $(textNameLit 'GHC.Prim.bitReverse8#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} | [i] <- wordLiterals' args
            -> reduce . integerToWordLiteral . toInteger . bitReverse8 . fromInteger $ i
          _ -> Nothing
    )
  , ( $(textNameLit 'GHC.Prim.bitReverse16#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} | [i] <- wordLiterals' args
            -> reduce . integerToWordLiteral . toInteger . bitReverse16 . fromInteger $ i
          _ -> Nothing
    )
  , ( $(textNameLit 'GHC.Prim.bitReverse32#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} | [i] <- wordLiterals' args
            -> reduce . integerToWordLiteral . toInteger . bitReverse32 . fromInteger $ i
          _ -> Nothing
    )
  , ( $(textNameLit 'GHC.Prim.bitReverse64#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} | [i] <- word64Literals' args
            -> reduce . integerToWordLiteral . toInteger . bitReverse64 . fromInteger $ i
          _ -> Nothing
    )
------------
-- Narrowing
------------
  , ( $(textNameLit 'GHC.Prim.narrow8Int#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} | [i] <- intLiterals' args
            -> let !(I# a)  = fromInteger i
                   b = narrow8Int# a
               in  reduce . Literal . IntLiteral . toInteger $ I# b
          _ -> Nothing
    )
  , ( $(textNameLit 'GHC.Prim.narrow16Int#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} | [i] <- intLiterals' args
            -> let !(I# a)  = fromInteger i
                   b = narrow16Int# a
               in  reduce . Literal . IntLiteral . toInteger $ I# b
          _ -> Nothing
    )
  , ( $(textNameLit 'GHC.Prim.narrow32Int#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} | [i] <- intLiterals' args
            -> let !(I# a)  = fromInteger i
                   b = narrow32Int# a
               in  reduce . Literal . IntLiteral . toInteger $ I# b
          _ -> Nothing
    )
  , ( $(textNameLit 'GHC.Prim.narrow8Word#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} | [i] <- wordLiterals' args
            -> let !(W# a)  = fromInteger i
                   b = narrow8Word# a
               in  reduce . Literal . WordLiteral . toInteger $ W# b
          _ -> Nothing
    )
  , ( $(textNameLit 'GHC.Prim.narrow16Word#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} | [i] <- wordLiterals' args
            -> let !(W# a)  = fromInteger i
                   b = narrow16Word# a
               in  reduce . Literal . WordLiteral . toInteger $ W# b
          _ -> Nothing
    )
  , ( $(textNameLit 'GHC.Prim.narrow32Word#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} | [i] <- wordLiterals' args
            -> let !(W# a)  = fromInteger i
                   b = narrow32Word# a
               in  reduce . Literal . WordLiteral . toInteger $ W# b
          _ -> Nothing
    )

--------
-- Int8#
--------
  , ( $(textNameLit 'GHC.Prim.intToInt8#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} | [i] <- intLiterals' args
            -> let !(I# a)  = fromInteger i
                   b = narrow8Int# a
               in  reduce . Literal . Int8Literal . toInteger $ I# b
          _ -> Nothing
    )
  , ( $(textNameLit 'GHC.Prim.int8ToInt#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} | [i] <- int8Literals' args
            -> reduce . Literal $ IntLiteral i
          _ -> Nothing
    )
  -- XXX: Primitive does not exist?
  , ( "GHC.Prim.negateInt8"
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} | [i] <- int8Literals' args
            -> let !(I8# a) = fromInteger i
                in reduce (Literal (Int8Literal (toInteger (I8# (negateInt8# a)))))
          _ -> Nothing
    )
  , ( $(textNameLit 'GHC.Prim.plusInt8#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} | Just r <- liftI8 plusInt8# args
            -> reduce r
          _ -> Nothing
    )
  , ( $(textNameLit 'GHC.Prim.subInt8#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} | Just r <- liftI8 subInt8# args
            -> reduce r
          _ -> Nothing
    )
  , ( $(textNameLit 'GHC.Prim.timesInt8#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} | Just r <- liftI8 timesInt8# args
            -> reduce r
          _ -> Nothing
    )
  , ( $(textNameLit 'GHC.Prim.quotInt8#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} | [i, j] <- int8Literals' args
            -> reduce $ catchDivByZero
                 (Literal (Int8Literal (toInteger (fromInteger i `quot` fromInteger j :: Int8))))
          _ -> Nothing
    )
  , ( $(textNameLit 'GHC.Prim.remInt8#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} | [i, j] <- int8Literals' args
            -> reduce $ catchDivByZero
                 (Literal (Int8Literal (toInteger (fromInteger i `rem` fromInteger j :: Int8))))
          _ -> Nothing
    )
  , ( $(textNameLit 'GHC.Prim.quotRemInt8#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
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
    )
  , ( $(textNameLit 'GHC.Prim.uncheckedShiftLInt8#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} | Just r <- liftI8I uncheckedShiftLInt8# args
            -> reduce r
          _ -> Nothing
    )
  , ( $(textNameLit 'GHC.Prim.uncheckedShiftRAInt8#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} | Just r <- liftI8I uncheckedShiftRAInt8# args
            -> reduce r
          _ -> Nothing
    )
  , ( $(textNameLit 'GHC.Prim.uncheckedShiftRLInt8#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} | Just r <- liftI8I uncheckedShiftRLInt8# args
            -> reduce r
          _ -> Nothing
    )
  , ( $(textNameLit 'GHC.Prim.int8ToWord8#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} | [i] <- int8Literals' args
            -> let !(I8# a) = fromInteger i
                in reduce (Literal (Word8Literal (toInteger (W8# (int8ToWord8# a)))))
          _ -> Nothing
    )
  , ( $(textNameLit 'GHC.Prim.eqInt8#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} | Just r <- liftI8RI eqInt8# args
            -> reduce r
          _ -> Nothing
    )
  , ( $(textNameLit 'GHC.Prim.geInt8#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} | Just r <- liftI8RI geInt8# args
            -> reduce r
          _ -> Nothing
    )
  , ( $(textNameLit 'GHC.Prim.gtInt8#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} | Just r <- liftI8RI gtInt8# args
            -> reduce r
          _ -> Nothing
    )
  , ( $(textNameLit 'GHC.Prim.leInt8#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} | Just r <- liftI8RI leInt8# args
            -> reduce r
          _ -> Nothing
    )
  , ( $(textNameLit 'GHC.Prim.ltInt8#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} | Just r <- liftI8RI ltInt8# args
            -> reduce r
          _ -> Nothing
    )
  , ( $(textNameLit 'GHC.Prim.neInt8#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} | Just r <- liftI8RI neInt8# args
            -> reduce r
          _ -> Nothing
    )

---------
-- Int16#
---------
  , ( $(textNameLit 'GHC.Prim.intToInt16#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} | [i] <- intLiterals' args
            -> let !(I# a)  = fromInteger i
                   b = narrow16Int# a
               in  reduce . Literal . Int16Literal . toInteger $ I# b
          _ -> Nothing
    )
  , ( $(textNameLit 'GHC.Prim.int16ToInt#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} | [i] <- int16Literals' args
            -> reduce . Literal $ IntLiteral i
          _ -> Nothing
    )
  -- XXX: Primitive does not exist?
  , ( "GHC.Prim.negateInt16"
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} | [i] <- int16Literals' args
            -> let !(I16# a) = fromInteger i
                in reduce (Literal (Int16Literal (toInteger (I16# (negateInt16# a)))))
          _ -> Nothing
    )
  , ( $(textNameLit 'GHC.Prim.plusInt16#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} | Just r <- liftI16 plusInt16# args
            -> reduce r
          _ -> Nothing
    )
  , ( $(textNameLit 'GHC.Prim.subInt16#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} | Just r <- liftI16 subInt16# args
            -> reduce r
          _ -> Nothing
    )
  , ( $(textNameLit 'GHC.Prim.timesInt16#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} | Just r <- liftI16 timesInt16# args
            -> reduce r
          _ -> Nothing
    )
  , ( $(textNameLit 'GHC.Prim.quotInt16#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} | [i, j] <- int16Literals' args
            -> reduce $ catchDivByZero
                 (Literal (Int16Literal (toInteger (fromInteger i `quot` fromInteger j :: Int16))))
          _ -> Nothing
    )
  , ( $(textNameLit 'GHC.Prim.remInt16#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} | [i, j] <- int16Literals' args
            -> reduce $ catchDivByZero
                 (Literal (Int16Literal (toInteger (fromInteger i `rem` fromInteger j :: Int16))))
          _ -> Nothing
    )
  , ( $(textNameLit 'GHC.Prim.quotRemInt16#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
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
    )
  , ( $(textNameLit 'GHC.Prim.uncheckedShiftLInt16#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} | Just r <- liftI16I uncheckedShiftLInt16# args
            -> reduce r
          _ -> Nothing
    )
  , ( $(textNameLit 'GHC.Prim.uncheckedShiftRAInt16#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} | Just r <- liftI16I uncheckedShiftRAInt16# args
            -> reduce r
          _ -> Nothing
    )
  , ( $(textNameLit 'GHC.Prim.uncheckedShiftRLInt16#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} | Just r <- liftI16I uncheckedShiftRLInt16# args
            -> reduce r
          _ -> Nothing
    )
  , ( $(textNameLit 'GHC.Prim.int16ToWord16#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} | [i] <- int16Literals' args
            -> let !(I16# a) = fromInteger i
                in reduce (Literal (Word16Literal (toInteger (W16# (int16ToWord16# a)))))
          _ -> Nothing
    )
  , ( $(textNameLit 'GHC.Prim.eqInt16#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} | Just r <- liftI16RI eqInt16# args
            -> reduce r
          _ -> Nothing
    )
  , ( $(textNameLit 'GHC.Prim.geInt16#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} | Just r <- liftI16RI geInt16# args
            -> reduce r
          _ -> Nothing
    )
  , ( $(textNameLit 'GHC.Prim.gtInt16#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} | Just r <- liftI16RI gtInt16# args
            -> reduce r
          _ -> Nothing
    )
  , ( $(textNameLit 'GHC.Prim.leInt16#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} | Just r <- liftI16RI leInt16# args
            -> reduce r
          _ -> Nothing
    )
  , ( $(textNameLit 'GHC.Prim.ltInt16#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} | Just r <- liftI16RI ltInt16# args
            -> reduce r
          _ -> Nothing
    )
  , ( $(textNameLit 'GHC.Prim.neInt16#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} | Just r <- liftI16RI neInt16# args
            -> reduce r
          _ -> Nothing
    )

---------
-- Int32#
---------
  , ( $(textNameLit 'GHC.Prim.intToInt32#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} | [i] <- intLiterals' args
            -> let !(I# a)  = fromInteger i
                   b = narrow32Int# a
               in  reduce . Literal . Int32Literal . toInteger $ I# b
          _ -> Nothing
    )
  , ( $(textNameLit 'GHC.Prim.int32ToInt#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} | [i] <- int32Literals' args
            -> reduce . Literal $ IntLiteral i
          _ -> Nothing
    )
  -- XXX: Primitive does not exist?
  , ( "GHC.Prim.negateInt32"
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} | [i] <- int32Literals' args
            -> let !(I32# a) = fromInteger i
                in reduce (Literal (Int32Literal (toInteger (I32# (negateInt32# a)))))
          _ -> Nothing
    )
  , ( $(textNameLit 'GHC.Prim.plusInt32#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} | Just r <- liftI32 plusInt32# args
            -> reduce r
          _ -> Nothing
    )
  , ( $(textNameLit 'GHC.Prim.subInt32#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} | Just r <- liftI32 subInt32# args
            -> reduce r
          _ -> Nothing
    )
  , ( $(textNameLit 'GHC.Prim.timesInt32#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} | Just r <- liftI32 timesInt32# args
            -> reduce r
          _ -> Nothing
    )
  , ( $(textNameLit 'GHC.Prim.quotInt32#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} | [i, j] <- int32Literals' args
            -> reduce $ catchDivByZero
                 (Literal (Int32Literal (toInteger (fromInteger i `quot` fromInteger j :: Int32))))
          _ -> Nothing
    )
  , ( $(textNameLit 'GHC.Prim.remInt32#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} | [i, j] <- int32Literals' args
            -> reduce $ catchDivByZero
                 (Literal (Int32Literal (toInteger (fromInteger i `rem` fromInteger j :: Int32))))
          _ -> Nothing
    )
  , ( $(textNameLit 'GHC.Prim.quotRemInt32#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
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
    )
  , ( $(textNameLit 'GHC.Prim.uncheckedShiftLInt32#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} | Just r <- liftI32I uncheckedShiftLInt32# args
            -> reduce r
          _ -> Nothing
    )
  , ( $(textNameLit 'GHC.Prim.uncheckedShiftRAInt32#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} | Just r <- liftI32I uncheckedShiftRAInt32# args
            -> reduce r
          _ -> Nothing
    )
  , ( $(textNameLit 'GHC.Prim.uncheckedShiftRLInt32#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} | Just r <- liftI32I uncheckedShiftRLInt32# args
            -> reduce r
          _ -> Nothing
    )
  , ( $(textNameLit 'GHC.Prim.int32ToWord32#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} | [i] <- int32Literals' args
            -> let !(I32# a) = fromInteger i
                in reduce (Literal (Word32Literal (toInteger (W32# (int32ToWord32# a)))))
          _ -> Nothing
    )
  , ( $(textNameLit 'GHC.Prim.eqInt32#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} | Just r <- liftI32RI eqInt32# args
            -> reduce r
          _ -> Nothing
    )
  , ( $(textNameLit 'GHC.Prim.geInt32#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} | Just r <- liftI32RI geInt32# args
            -> reduce r
          _ -> Nothing
    )
  , ( $(textNameLit 'GHC.Prim.gtInt32#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} | Just r <- liftI32RI gtInt32# args
            -> reduce r
          _ -> Nothing
    )
  , ( $(textNameLit 'GHC.Prim.leInt32#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} | Just r <- liftI32RI leInt32# args
            -> reduce r
          _ -> Nothing
    )
  , ( $(textNameLit 'GHC.Prim.ltInt32#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} | Just r <- liftI32RI ltInt32# args
            -> reduce r
          _ -> Nothing
    )
  , ( $(textNameLit 'GHC.Prim.neInt32#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} | Just r <- liftI32RI neInt32# args
            -> reduce r
          _ -> Nothing
    )

---------
-- Int64#
---------
  , ( $(textNameLit 'GHC.Prim.intToInt64#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} | [i] <- intLiterals' args
            -> reduce (Literal (Int64Literal i))
          _ -> Nothing
    )
  , ( $(textNameLit 'GHC.Prim.int64ToInt#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} | [i] <- int64Literals' args
            -> reduce . Literal $ IntLiteral i
          _ -> Nothing
    )
  -- XXX: Primitive does not exist?
  , ( "GHC.Prim.negateInt64"
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} | [i] <- int64Literals' args
            -> let !(I64# a) = fromInteger i
                in reduce (Literal (Int64Literal (toInteger (I64# (negateInt64# a)))))
          _ -> Nothing
    )
  , ( $(textNameLit 'GHC.Prim.plusInt64#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} | Just r <- liftI64 plusInt64# args
            -> reduce r
          _ -> Nothing
    )
  , ( $(textNameLit 'GHC.Prim.subInt64#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} | Just r <- liftI64 subInt64# args
            -> reduce r
          _ -> Nothing
    )
  , ( $(textNameLit 'GHC.Prim.timesInt64#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} | Just r <- liftI64 timesInt64# args
            -> reduce r
          _ -> Nothing
    )
  , ( $(textNameLit 'GHC.Prim.quotInt64#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} | [i, j] <- int64Literals' args
            -> reduce $ catchDivByZero
                 (Literal (Int64Literal (toInteger (fromInteger i `quot` fromInteger j :: Int64))))
          _ -> Nothing
    )
  , ( $(textNameLit 'GHC.Prim.remInt64#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} | [i, j] <- int64Literals' args
            -> reduce $ catchDivByZero
                 (Literal (Int64Literal (toInteger (fromInteger i `rem` fromInteger j :: Int64))))
          _ -> Nothing
    )
  , ( $(textNameLit 'GHC.Prim.uncheckedIShiftL64#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} | Just r <- liftI64I uncheckedIShiftL64# args
            -> reduce r
          _ -> Nothing
    )
  , ( $(textNameLit 'GHC.Prim.uncheckedIShiftRA64#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} | Just r <- liftI64I uncheckedIShiftRA64# args
            -> reduce r
          _ -> Nothing
    )
  , ( $(textNameLit 'GHC.Prim.uncheckedIShiftRL64#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} | Just r <- liftI64I uncheckedIShiftRL64# args
            -> reduce r
          _ -> Nothing
    )
  , ( $(textNameLit 'GHC.Prim.int64ToWord64#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} | [i] <- int64Literals' args
            -> let !(I64# a) = fromInteger i
                in reduce (Literal (Word64Literal (toInteger (W64# (int64ToWord64# a)))))
          _ -> Nothing
    )
  , ( $(textNameLit 'GHC.Prim.eqInt64#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} | Just r <- liftI64RI eqInt64# args
            -> reduce r
          _ -> Nothing
    )
  , ( $(textNameLit 'GHC.Prim.geInt64#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} | Just r <- liftI64RI geInt64# args
            -> reduce r
          _ -> Nothing
    )
  , ( $(textNameLit 'GHC.Prim.gtInt64#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} | Just r <- liftI64RI gtInt64# args
            -> reduce r
          _ -> Nothing
    )
  , ( $(textNameLit 'GHC.Prim.leInt64#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} | Just r <- liftI64RI leInt64# args
            -> reduce r
          _ -> Nothing
    )
  , ( $(textNameLit 'GHC.Prim.ltInt64#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} | Just r <- liftI64RI ltInt64# args
            -> reduce r
          _ -> Nothing
    )
  , ( $(textNameLit 'GHC.Prim.neInt64#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} | Just r <- liftI64RI neInt64# args
            -> reduce r
          _ -> Nothing
    )

---------
-- Word8#
---------
  , ( $(textNameLit 'GHC.Prim.wordToWord8#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} | [i] <- wordLiterals' args
            -> let !(W# a)  = fromInteger i
                   b = narrow8Word# a
               in  reduce . Literal . Word8Literal . toInteger $ W# b
          _ -> Nothing
    )
  , ( $(textNameLit 'GHC.Prim.word8ToWord#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} | [i] <- word8Literals' args
            -> reduce . Literal $ WordLiteral i
          _ -> Nothing
    )
  , ( $(textNameLit 'GHC.Prim.plusWord8#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} | Just r <- liftW8 plusWord8# args
            -> reduce r
          _ -> Nothing
    )
  , ( $(textNameLit 'GHC.Prim.subWord8#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} | Just r <- liftW8 subWord8# args
            -> reduce r
          _ -> Nothing
    )
  , ( $(textNameLit 'GHC.Prim.timesWord8#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} | Just r <- liftW8 timesWord8# args
            -> reduce r
          _ -> Nothing
    )
  , ( $(textNameLit 'GHC.Prim.quotWord8#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} | [i, j] <- word8Literals' args
            -> reduce $ catchDivByZero
                 (Literal (Word8Literal (toInteger (fromInteger i `quot` fromInteger j :: Word8))))
          _ -> Nothing
    )
  , ( $(textNameLit 'GHC.Prim.remWord8#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} | [i, j] <- word8Literals' args
            -> reduce $ catchDivByZero
                 (Literal (Word8Literal (toInteger (fromInteger i `rem` fromInteger j :: Word8))))
          _ -> Nothing
    )
  , ( $(textNameLit 'GHC.Prim.quotRemWord8#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
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
    )
  , ( $(textNameLit 'GHC.Prim.andWord8#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} | Just r <- liftW8 andWord8# args
            -> reduce r
          _ -> Nothing
    )
  , ( $(textNameLit 'GHC.Prim.orWord8#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} | Just r <- liftW8 orWord8# args
            -> reduce r
          _ -> Nothing
    )
  , ( $(textNameLit 'GHC.Prim.xorWord8#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} | Just r <- liftW8 xorWord8# args
            -> reduce r
          _ -> Nothing
    )
  , ( $(textNameLit 'GHC.Prim.notWord8#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} | [i] <- word8Literals' args
            -> let !(W8# a) = fromInteger i
                in reduce (Literal (Word8Literal (toInteger (W8# (notWord8# a)))))
          _ -> Nothing
    )
  , ( $(textNameLit 'GHC.Prim.uncheckedShiftLWord8#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} | Just r <- liftW8I uncheckedShiftLWord8# args
            -> reduce r
          _ -> Nothing
    )
  , ( $(textNameLit 'GHC.Prim.uncheckedShiftRLWord8#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} | Just r <- liftW8I uncheckedShiftRLWord8# args
            -> reduce r
          _ -> Nothing
    )
  , ( $(textNameLit 'GHC.Prim.word8ToInt8#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} | [i] <- word8Literals' args
            -> let !(W8# a) = fromInteger i
                in reduce (Literal (Int8Literal (toInteger (I8# (word8ToInt8# a)))))
          _ -> Nothing
    )
  , ( $(textNameLit 'GHC.Prim.eqWord8#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} | Just r <- liftW8RI eqWord8# args
            -> reduce r
          _ -> Nothing
    )
  , ( $(textNameLit 'GHC.Prim.geWord8#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} | Just r <- liftW8RI geWord8# args
            -> reduce r
          _ -> Nothing
    )
  , ( $(textNameLit 'GHC.Prim.gtWord8#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} | Just r <- liftW8RI gtWord8# args
            -> reduce r
          _ -> Nothing
    )
  , ( $(textNameLit 'GHC.Prim.leWord8#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} | Just r <- liftW8RI leWord8# args
            -> reduce r
          _ -> Nothing
    )
  , ( $(textNameLit 'GHC.Prim.ltWord8#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} | Just r <- liftW8RI ltWord8# args
            -> reduce r
          _ -> Nothing
    )
  , ( $(textNameLit 'GHC.Prim.neWord8#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} | Just r <- liftW8RI neWord8# args
            -> reduce r
          _ -> Nothing
    )

----------
-- Word16#
----------
  , ( $(textNameLit 'GHC.Prim.wordToWord16#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} | [i] <- wordLiterals' args
            -> let !(W# a)  = fromInteger i
                   b = narrow16Word# a
               in  reduce . Literal . Word16Literal . toInteger $ W# b
          _ -> Nothing
    )
  , ( $(textNameLit 'GHC.Prim.word16ToWord#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} | [i] <- word16Literals' args
            -> reduce . Literal $ WordLiteral i
          _ -> Nothing
    )
  , ( $(textNameLit 'GHC.Prim.plusWord16#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} | Just r <- liftW16 plusWord16# args
            -> reduce r
          _ -> Nothing
    )
  , ( $(textNameLit 'GHC.Prim.subWord16#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} | Just r <- liftW16 subWord16# args
            -> reduce r
          _ -> Nothing
    )
  , ( $(textNameLit 'GHC.Prim.timesWord16#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} | Just r <- liftW16 timesWord16# args
            -> reduce r
          _ -> Nothing
    )
  , ( $(textNameLit 'GHC.Prim.quotWord16#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} | [i, j] <- word16Literals' args
            -> reduce $ catchDivByZero
                 (Literal (Word16Literal (toInteger (fromInteger i `quot` fromInteger j :: Word16))))
          _ -> Nothing
    )
  , ( $(textNameLit 'GHC.Prim.remWord16#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} | [i, j] <- word16Literals' args
            -> reduce $ catchDivByZero
                 (Literal (Word16Literal (toInteger (fromInteger i `rem` fromInteger j :: Word16))))
          _ -> Nothing
    )
  , ( $(textNameLit 'GHC.Prim.quotRemWord16#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
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
    )
  , ( $(textNameLit 'GHC.Prim.andWord16#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} | Just r <- liftW16 andWord16# args
            -> reduce r
          _ -> Nothing
    )
  , ( $(textNameLit 'GHC.Prim.orWord16#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} | Just r <- liftW16 orWord16# args
            -> reduce r
          _ -> Nothing
    )
  , ( $(textNameLit 'GHC.Prim.xorWord16#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} | Just r <- liftW16 xorWord16# args
            -> reduce r
          _ -> Nothing
    )
  , ( $(textNameLit 'GHC.Prim.notWord16#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} | [i] <- word16Literals' args
            -> let !(W16# a) = fromInteger i
                in reduce (Literal (Word16Literal (toInteger (W16# (notWord16# a)))))
          _ -> Nothing
    )
  , ( $(textNameLit 'GHC.Prim.uncheckedShiftLWord16#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} | Just r <- liftW16I uncheckedShiftLWord16# args
            -> reduce r
          _ -> Nothing
    )
  , ( $(textNameLit 'GHC.Prim.uncheckedShiftRLWord16#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} | Just r <- liftW16I uncheckedShiftRLWord16# args
            -> reduce r
          _ -> Nothing
    )
  , ( $(textNameLit 'GHC.Prim.word16ToInt16#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} | [i] <- word16Literals' args
            -> let !(W16# a) = fromInteger i
                in reduce (Literal (Int16Literal (toInteger (I16# (word16ToInt16# a)))))
          _ -> Nothing
    )
  , ( $(textNameLit 'GHC.Prim.eqWord16#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} | Just r <- liftW16RI eqWord16# args
            -> reduce r
          _ -> Nothing
    )
  , ( $(textNameLit 'GHC.Prim.geWord16#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} | Just r <- liftW16RI geWord16# args
            -> reduce r
          _ -> Nothing
    )
  , ( $(textNameLit 'GHC.Prim.gtWord16#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} | Just r <- liftW16RI gtWord16# args
            -> reduce r
          _ -> Nothing
    )
  , ( $(textNameLit 'GHC.Prim.leWord16#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} | Just r <- liftW16RI leWord16# args
            -> reduce r
          _ -> Nothing
    )
  , ( $(textNameLit 'GHC.Prim.ltWord16#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} | Just r <- liftW16RI ltWord16# args
            -> reduce r
          _ -> Nothing
    )
  , ( $(textNameLit 'GHC.Prim.neWord16#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} | Just r <- liftW16RI neWord16# args
            -> reduce r
          _ -> Nothing
    )

----------
-- Word32#
----------
  , ( $(textNameLit 'GHC.Prim.wordToWord32#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} | [i] <- wordLiterals' args
            -> let !(W# a)  = fromInteger i
                   b = narrow32Word# a
               in  reduce . Literal . Word32Literal . toInteger $ W# b
          _ -> Nothing
    )
  , ( $(textNameLit 'GHC.Prim.word32ToWord#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} | [i] <- word32Literals' args
            -> reduce . Literal $ WordLiteral i
          _ -> Nothing
    )
  , ( $(textNameLit 'GHC.Prim.plusWord32#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} | Just r <- liftW32 plusWord32# args
            -> reduce r
          _ -> Nothing
    )
  , ( $(textNameLit 'GHC.Prim.subWord32#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} | Just r <- liftW32 subWord32# args
            -> reduce r
          _ -> Nothing
    )
  , ( $(textNameLit 'GHC.Prim.timesWord32#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} | Just r <- liftW32 timesWord32# args
            -> reduce r
          _ -> Nothing
    )
  , ( $(textNameLit 'GHC.Prim.quotWord32#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} | [i, j] <- word32Literals' args
            -> reduce $ catchDivByZero
                 (Literal (Word32Literal (toInteger (fromInteger i `quot` fromInteger j :: Word32))))
          _ -> Nothing
    )
  , ( $(textNameLit 'GHC.Prim.remWord32#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} | [i, j] <- word32Literals' args
            -> reduce $ catchDivByZero
                 (Literal (Word32Literal (toInteger (fromInteger i `rem` fromInteger j :: Word32))))
          _ -> Nothing
    )
  , ( $(textNameLit 'GHC.Prim.quotRemWord32#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
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
    )
  , ( $(textNameLit 'GHC.Prim.andWord32#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} | Just r <- liftW32 andWord32# args
            -> reduce r
          _ -> Nothing
    )
  , ( $(textNameLit 'GHC.Prim.orWord32#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} | Just r <- liftW32 orWord32# args
            -> reduce r
          _ -> Nothing
    )
  , ( $(textNameLit 'GHC.Prim.xorWord32#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} | Just r <- liftW32 xorWord32# args
            -> reduce r
          _ -> Nothing
    )
  , ( $(textNameLit 'GHC.Prim.notWord32#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} | [i] <- word32Literals' args
            -> let !(W32# a) = fromInteger i
                in reduce (Literal (Word32Literal (toInteger (W32# (notWord32# a)))))
          _ -> Nothing
    )
  , ( $(textNameLit 'GHC.Prim.uncheckedShiftLWord32#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} | Just r <- liftW32I uncheckedShiftLWord32# args
            -> reduce r
          _ -> Nothing
    )
  , ( $(textNameLit 'GHC.Prim.uncheckedShiftRLWord32#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} | Just r <- liftW32I uncheckedShiftRLWord32# args
            -> reduce r
          _ -> Nothing
    )
  , ( $(textNameLit 'GHC.Prim.word32ToInt32#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} | [i] <- word32Literals' args
            -> let !(W32# a) = fromInteger i
                in reduce (Literal (Int32Literal (toInteger (I32# (word32ToInt32# a)))))
          _ -> Nothing
    )
  , ( $(textNameLit 'GHC.Prim.eqWord32#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} | Just r <- liftW32RI eqWord32# args
            -> reduce r
          _ -> Nothing
    )
  , ( $(textNameLit 'GHC.Prim.geWord32#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} | Just r <- liftW32RI geWord32# args
            -> reduce r
          _ -> Nothing
    )
  , ( $(textNameLit 'GHC.Prim.gtWord32#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} | Just r <- liftW32RI gtWord32# args
            -> reduce r
          _ -> Nothing
    )
  , ( $(textNameLit 'GHC.Prim.leWord32#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} | Just r <- liftW32RI leWord32# args
            -> reduce r
          _ -> Nothing
    )
  , ( $(textNameLit 'GHC.Prim.ltWord32#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} | Just r <- liftW32RI ltWord32# args
            -> reduce r
          _ -> Nothing
    )
  , ( $(textNameLit 'GHC.Prim.neWord32#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} | Just r <- liftW32RI neWord32# args
            -> reduce r
          _ -> Nothing
    )

----------
-- Word64#
----------
  , ( $(textNameLit 'GHC.Prim.wordToWord64#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} | [i] <- wordLiterals' args
            -> reduce (Literal (Word64Literal i))
          _ -> Nothing
    )
  , ( $(textNameLit 'GHC.Prim.word64ToWord#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} | [i] <- word64Literals' args
            -> reduce . Literal $ WordLiteral i
          _ -> Nothing
    )
  , ( $(textNameLit 'GHC.Prim.plusWord64#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} | Just r <- liftW64 plusWord64# args
            -> reduce r
          _ -> Nothing
    )
  , ( $(textNameLit 'GHC.Prim.subWord64#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} | Just r <- liftW64 subWord64# args
            -> reduce r
          _ -> Nothing
    )
  , ( $(textNameLit 'GHC.Prim.timesWord64#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} | Just r <- liftW64 timesWord64# args
            -> reduce r
          _ -> Nothing
    )
  , ( $(textNameLit 'GHC.Prim.quotWord64#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} | [i, j] <- word64Literals' args
            -> reduce $ catchDivByZero
                 (Literal (Word64Literal (toInteger (fromInteger i `quot` fromInteger j :: Word64))))
          _ -> Nothing
    )
  , ( $(textNameLit 'GHC.Prim.remWord64#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} | [i, j] <- word64Literals' args
            -> reduce $ catchDivByZero
                 (Literal (Word64Literal (toInteger (fromInteger i `rem` fromInteger j :: Word64))))
          _ -> Nothing
    )
  , ( $(textNameLit 'GHC.Prim.and64#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} | Just r <- liftW64 and64# args
            -> reduce r
          _ -> Nothing
    )
  , ( $(textNameLit 'GHC.Prim.or64#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} | Just r <- liftW64 or64# args
            -> reduce r
          _ -> Nothing
    )
  , ( $(textNameLit 'GHC.Prim.xor64#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} | Just r <- liftW64 xor64# args
            -> reduce r
          _ -> Nothing
    )
  , ( $(textNameLit 'GHC.Prim.not64#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} | [i] <- word64Literals' args
            -> let !(W64# a) = fromInteger i
                in reduce (Literal (Word64Literal (toInteger (W64# (not64# a)))))
          _ -> Nothing
    )
  , ( $(textNameLit 'GHC.Prim.uncheckedShiftL64#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} | Just r <- liftW64I uncheckedShiftL64# args
            -> reduce r
          _ -> Nothing
    )
  , ( $(textNameLit 'GHC.Prim.uncheckedShiftRL64#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} | Just r <- liftW64I uncheckedShiftRL64# args
            -> reduce r
          _ -> Nothing
    )
  , ( $(textNameLit 'GHC.Prim.word64ToInt64#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} | [i] <- word64Literals' args
            -> let !(W64# a) = fromInteger i
                in reduce (Literal (Int64Literal (toInteger (I64# (word64ToInt64# a)))))
          _ -> Nothing
    )
  , ( $(textNameLit 'GHC.Prim.eqWord64#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} | Just r <- liftW64RI eqWord64# args
            -> reduce r
          _ -> Nothing
    )
  , ( $(textNameLit 'GHC.Prim.geWord64#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} | Just r <- liftW64RI geWord64# args
            -> reduce r
          _ -> Nothing
    )
  , ( $(textNameLit 'GHC.Prim.gtWord64#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} | Just r <- liftW64RI gtWord64# args
            -> reduce r
          _ -> Nothing
    )
  , ( $(textNameLit 'GHC.Prim.leWord64#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} | Just r <- liftW64RI leWord64# args
            -> reduce r
          _ -> Nothing
    )
  , ( $(textNameLit 'GHC.Prim.ltWord64#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} | Just r <- liftW64RI ltWord64# args
            -> reduce r
          _ -> Nothing
    )
  , ( $(textNameLit 'GHC.Prim.neWord64#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} | Just r <- liftW64RI neWord64# args
            -> reduce r
          _ -> Nothing
    )

----------
-- Double#
----------
  , ( $(textNameLit '(GHC.Prim.>##))
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..}  | Just r <- liftDDI (>##)  args
            -> reduce r
          _ -> Nothing
    )
  , ( $(textNameLit '(GHC.Prim.>=##))
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} | Just r <- liftDDI (>=##) args
            -> reduce r
          _ -> Nothing
    )
  , ( $(textNameLit '(GHC.Prim.==##))
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} | Just r <- liftDDI (==##) args
            -> reduce r
          _ -> Nothing
    )
  , ( $(textNameLit '(GHC.Prim./=##))
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} | Just r <- liftDDI (/=##) args
            -> reduce r
          _ -> Nothing
    )
  , ( $(textNameLit '(GHC.Prim.<##))
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..}  | Just r <- liftDDI (<##)  args
            -> reduce r
          _ -> Nothing
    )
  , ( $(textNameLit '(GHC.Prim.<=##))
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} | Just r <- liftDDI (<=##) args
            -> reduce r
          _ -> Nothing
    )
  , ( $(textNameLit '(GHC.Prim.+##))
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..}  | Just r <- liftDDD (+##)  args
            -> reduce r
          _ -> Nothing
    )
  , ( $(textNameLit '(GHC.Prim.-##))
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..}  | Just r <- liftDDD (-##)  args
            -> reduce r
          _ -> Nothing
    )
  , ( $(textNameLit '(GHC.Prim.*##))
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..}  | Just r <- liftDDD (*##)  args
            -> reduce r
          _ -> Nothing
    )
  , ( $(textNameLit '(GHC.Prim./##))
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..}  | Just r <- liftDDD (/##)  args
            -> reduce r
          _ -> Nothing
    )

  , ( $(textNameLit 'GHC.Prim.negateDouble#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} | Just r <- liftDD negateDouble# args
            -> reduce r
          _ -> Nothing
    )
  , ( $(textNameLit 'GHC.Prim.fabsDouble#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} | Just r <- liftDD fabsDouble# args
            -> reduce r
          _ -> Nothing
    )

  , ( $(textNameLit 'GHC.Prim.double2Int#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} | [i] <- doubleLiterals' args
            -> let !(D# a) = castWord64ToDouble i
                   r = double2Int# a
               in  reduce . Literal . IntLiteral . toInteger $ I# r
          _ -> Nothing
    )
  , ( $(textNameLit 'GHC.Prim.double2Float#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..}
            | [Lit (DoubleLiteral d)] <- args
            -> let !(D# a) = castWord64ToDouble d
                   r = double2Float# a
               in reduce . Literal . FloatLiteral . castFloatToWord32 $ F# r
          _ -> Nothing
    )

  , ( $(textNameLit 'GHC.Prim.expDouble#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} | Just r <- liftDD expDouble# args
            -> reduce r
          _ -> Nothing
    )
  , ( $(textNameLit 'GHC.Prim.logDouble#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} | Just r <- liftDD logDouble# args
            -> reduce r
          _ -> Nothing
    )
  , ( $(textNameLit 'GHC.Prim.sqrtDouble#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} | Just r <- liftDD sqrtDouble# args
            -> reduce r
          _ -> Nothing
    )
  , ( $(textNameLit 'GHC.Prim.sinDouble#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} | Just r <- liftDD sinDouble# args
            -> reduce r
          _ -> Nothing
    )
  , ( $(textNameLit 'GHC.Prim.cosDouble#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} | Just r <- liftDD cosDouble# args
            -> reduce r
          _ -> Nothing
    )
  , ( $(textNameLit 'GHC.Prim.tanDouble#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} | Just r <- liftDD tanDouble# args
            -> reduce r
          _ -> Nothing
    )
  , ( $(textNameLit 'GHC.Prim.asinDouble#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} | Just r <- liftDD asinDouble# args
            -> reduce r
          _ -> Nothing
    )
  , ( $(textNameLit 'GHC.Prim.acosDouble#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} | Just r <- liftDD acosDouble# args
            -> reduce r
          _ -> Nothing
    )
  , ( $(textNameLit 'GHC.Prim.atanDouble#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} | Just r <- liftDD atanDouble# args
            -> reduce r
          _ -> Nothing
    )
  , ( $(textNameLit 'GHC.Prim.sinhDouble#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} | Just r <- liftDD sinhDouble# args
            -> reduce r
          _ -> Nothing
    )
  , ( $(textNameLit 'GHC.Prim.coshDouble#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} | Just r <- liftDD coshDouble# args
            -> reduce r
          _ -> Nothing
    )
  , ( $(textNameLit 'GHC.Prim.tanhDouble#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} | Just r <- liftDD tanhDouble# args
            -> reduce r
          _ -> Nothing
    )
  , ( $(textNameLit 'GHC.Prim.asinhDouble#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..}  | Just r <- liftDD asinhDouble# args
            -> reduce r
          _ -> Nothing
    )
  , ( $(textNameLit 'GHC.Prim.acoshDouble#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..}  | Just r <- liftDD acoshDouble# args
            -> reduce r
          _ -> Nothing
    )
  , ( $(textNameLit 'GHC.Prim.atanhDouble#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..}  | Just r <- liftDD atanhDouble# args
            -> reduce r
          _ -> Nothing
    )

  , ( $(textNameLit '(GHC.Prim.**##))
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} | Just r <- liftDDD (**##) args
            -> reduce r
          _ -> Nothing
    )
-- decodeDouble_2Int# :: Double# -> (#Int#, Word#, Word#, Int##)
  , ( $(textNameLit 'GHC.Prim.decodeDouble_2Int#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
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
    )
-- decodeDouble_Int64# :: Double# -> (# Int64#, Int# #)
  , ( $(textNameLit 'GHC.Prim.decodeDouble_Int64#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
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
    )

--------
-- Float
--------
  , ( $(textNameLit 'GHC.Prim.gtFloat#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..}  | Just r <- liftFFI gtFloat# args
            -> reduce r
          _ -> Nothing
    )
  , ( $(textNameLit 'GHC.Prim.geFloat#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..}  | Just r <- liftFFI geFloat# args
            -> reduce r
          _ -> Nothing
    )
  , ( $(textNameLit 'GHC.Prim.eqFloat#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..}  | Just r <- liftFFI eqFloat# args
            -> reduce r
          _ -> Nothing
    )
  , ( $(textNameLit 'GHC.Prim.neFloat#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..}  | Just r <- liftFFI neFloat# args
            -> reduce r
          _ -> Nothing
    )
  , ( $(textNameLit 'GHC.Prim.ltFloat#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..}  | Just r <- liftFFI ltFloat# args
            -> reduce r
          _ -> Nothing
    )
  , ( $(textNameLit 'GHC.Prim.leFloat#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..}  | Just r <- liftFFI leFloat# args
            -> reduce r
          _ -> Nothing
    )

  , ( $(textNameLit 'GHC.Prim.plusFloat#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..}  | Just r <- liftFFF plusFloat# args
            -> reduce r
          _ -> Nothing
    )
  , ( $(textNameLit 'GHC.Prim.minusFloat#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..}  | Just r <- liftFFF minusFloat# args
            -> reduce r
          _ -> Nothing
    )
  , ( $(textNameLit 'GHC.Prim.timesFloat#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..}  | Just r <- liftFFF timesFloat# args
            -> reduce r
          _ -> Nothing
    )
  , ( $(textNameLit 'GHC.Prim.divideFloat#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..}  | Just r <- liftFFF divideFloat# args
            -> reduce r
          _ -> Nothing
    )

  , ( $(textNameLit 'GHC.Prim.negateFloat#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..}  | Just r <- liftFF negateFloat# args
            -> reduce r
          _ -> Nothing
    )
  , ( $(textNameLit 'GHC.Prim.fabsFloat#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..}  | Just r <- liftFF fabsFloat# args
            -> reduce r
          _ -> Nothing
    )

  , ( $(textNameLit 'GHC.Prim.float2Int#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} | [i] <- floatLiterals' args
            -> let !(F# a) = castWord32ToFloat i
                   r = float2Int# a
               in  reduce . Literal . IntLiteral . toInteger $ I# r
          _ -> Nothing
    )

  , ( $(textNameLit 'GHC.Prim.expFloat#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..}  | Just r <- liftFF expFloat# args
            -> reduce r
          _ -> Nothing
    )
  , ( $(textNameLit 'GHC.Prim.logFloat#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..}  | Just r <- liftFF logFloat# args
            -> reduce r
          _ -> Nothing
    )
  , ( $(textNameLit 'GHC.Prim.sqrtFloat#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..}  | Just r <- liftFF sqrtFloat# args
            -> reduce r
          _ -> Nothing
    )
  , ( $(textNameLit 'GHC.Prim.sinFloat#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..}  | Just r <- liftFF sinFloat# args
            -> reduce r
          _ -> Nothing
    )
  , ( $(textNameLit 'GHC.Prim.cosFloat#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..}  | Just r <- liftFF cosFloat# args
            -> reduce r
          _ -> Nothing
    )
  , ( $(textNameLit 'GHC.Prim.tanFloat#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..}  | Just r <- liftFF tanFloat# args
            -> reduce r
          _ -> Nothing
    )
  , ( $(textNameLit 'GHC.Prim.asinFloat#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..}  | Just r <- liftFF asinFloat# args
            -> reduce r
          _ -> Nothing
    )
  , ( $(textNameLit 'GHC.Prim.acosFloat#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..}  | Just r <- liftFF acosFloat# args
            -> reduce r
          _ -> Nothing
    )
  , ( $(textNameLit 'GHC.Prim.atanFloat#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..}  | Just r <- liftFF atanFloat# args
            -> reduce r
          _ -> Nothing
    )
  , ( $(textNameLit 'GHC.Prim.sinhFloat#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..}  | Just r <- liftFF sinhFloat# args
            -> reduce r
          _ -> Nothing
    )
  , ( $(textNameLit 'GHC.Prim.coshFloat#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..}  | Just r <- liftFF coshFloat# args
            -> reduce r
          _ -> Nothing
    )
  , ( $(textNameLit 'GHC.Prim.tanhFloat#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..}  | Just r <- liftFF tanhFloat# args
            -> reduce r
          _ -> Nothing
    )
  , ( $(textNameLit 'GHC.Prim.powerFloat#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..}  | Just r <- liftFFF powerFloat# args
            -> reduce r
          _ -> Nothing
    )

  , ( $(textNameLit 'GHC.Prim.asinhFloat#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..}  | Just r <- liftFF asinhFloat# args
            -> reduce r
          _ -> Nothing
    )
  , ( $(textNameLit 'GHC.Prim.acoshFloat#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..}  | Just r <- liftFF acoshFloat# args
            -> reduce r
          _ -> Nothing
    )
  , ( $(textNameLit 'GHC.Prim.atanhFloat#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..}  | Just r <- liftFF atanhFloat# args
            -> reduce r
          _ -> Nothing
    )

  , ( $(textNameLit 'GHC.Prim.float2Double#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} | [i] <- floatLiterals' args
            -> let !(F# a) = castWord32ToFloat i
                   r = float2Double# a
               in  reduce . Literal . DoubleLiteral . castDoubleToWord64 $ D# r
          _ -> Nothing
    )


  , ( $(textNameLit 'GHC.Prim.newByteArray#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
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
    )

  , ( $(textNameLit 'GHC.Prim.setByteArray#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{}
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
    )

  , ( $(textNameLit 'GHC.Prim.writeWordArray#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{}
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
    )

  , ( $(textNameLit 'GHC.Prim.unsafeFreezeByteArray#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
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
    )

  , ( $(textNameLit 'GHC.Prim.sizeofByteArray#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..}
            | [Lit (ByteArrayLiteral ba)] <- args
            -> reduce (Literal (IntLiteral (toInteger (BA.sizeofByteArray ba))))
          _ -> Nothing
    )

  , ( $(textNameLit 'GHC.Prim.indexWordArray#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..}
            | [Lit (ByteArrayLiteral (BA.ByteArray ba)),iV] <- args
            , [i] <- intLiterals' [iV]
            -> let !(I# i') = fromInteger i
                   !w       = indexWordArray# ba i'
               in  reduce (Literal (WordLiteral (toInteger (W# w))))
          _ -> Nothing
    )

  -- XXX: Primitive does not exist?
  , ( "GHC.Prim.getSizeofMutBigNat#"
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
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
    )

  , ( $(textNameLit 'GHC.Prim.resizeMutableByteArray#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
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
    )

  , ( $(textNameLit 'GHC.Prim.shrinkMutableByteArray#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{}
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
    )

  , ( $(textNameLit 'GHC.Prim.copyByteArray#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{}
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
    )

  , ( $(textNameLit 'GHC.Prim.readWordArray#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
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
    )

  , ( $(textNameLit 'GHC.Prim.copyAddrToByteArray#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{}
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
    )

-- decodeFloat_Int# :: Float# -> (#Int#, Int##)
  , ( $(textNameLit 'GHC.Prim.decodeFloat_Int#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
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
    )

  , ( $(textNameLit 'GHC.Prim.tagToEnum#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{}
            | [ConstTy (TyCon tcN)] <- tys
            , [Lit (IntLiteral i)]  <- args
            -> let dc = do { tc <- UniqMap.lookup tcN tcm
                           ; let dcs = tyConDataCons tc
                           ; List.find ((== (i+1)) . toInteger . dcTag) dcs
                           }
               in (\e -> setTerm (Data e) mach) <$> dc
          _ -> Nothing
    )
#if MIN_VERSION_ghc(9,10,0)
  , ( $(textNameLit 'GHC.Prim.dataToTagSmall#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..}
            | [DC dc _] <- args
            -> reduce (Literal (IntLiteral (toInteger (dcTag dc - 1))))
          _ -> Nothing
    )
  , ( $(textNameLit 'GHC.Prim.dataToTagLarge#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..}
            | [DC dc _] <- args
            -> reduce (Literal (IntLiteral (toInteger (dcTag dc - 1))))
          _ -> Nothing
    )
#else
  , ( $(textNameLit 'GHC.Prim.dataToTag#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..}
            | [DC dc _] <- args
            -> reduce (Literal (IntLiteral (toInteger (dcTag dc - 1))))
          _ -> Nothing
    )
#endif
  ]
