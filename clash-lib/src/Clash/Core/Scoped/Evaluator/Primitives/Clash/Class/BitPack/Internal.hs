{-|
  Copyright   :  (C) 2013-2016, University of Twente,
                     2016-2017, Myrtle Software Ltd,
                     2017-2022, Google Inc.,
                     2017-2026, QBayLogic B.V.
  License     :  BSD2 (see the file LICENSE)
  Maintainer  :  QBayLogic B.V. <devops@qbaylogic.com>

  Reductions of the primitives of "Clash.Class.BitPack.Internal" in the
  scoped evaluator: ports of
  @Clash.GHC.Evaluator.Primitives.Clash.Class.BitPack.Internal@. See
  "Clash.Core.Scoped.Evaluator.Primitives" for how to port them.

  The @pack@ reductions evaluate the field of their boxed argument with a
  nested machine, and continue in that machine.
-}

{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Clash.Core.Scoped.Evaluator.Primitives.Clash.Class.BitPack.Internal
  ( primitives
  ) where

import Data.Text (Text)
import GHC.Word

import Clash.Class.BitPack (pack, unpack)
import Clash.Core.Literal (Literal (..))
import Clash.Util (textNameLit)

import Clash.Sized.Internal.BitVector (BitVector (..))
import Clash.Sized.Internal.Signed (Signed (..))
import Clash.Sized.Internal.Unsigned (Unsigned (..))

import qualified Clash.Class.BitPack.Internal

import Clash.Core.Scoped.Evaluator.Primitives.Util
import Clash.Core.Scoped.Evaluator.Types
import Clash.Core.Scoped.Syntax

primitives :: [(Text, PrimReduction)]
primitives =
  [ primStepEntry $(textNameLit 'Clash.Class.BitPack.Internal.packInt8#) $ \case
      PrimStepContext{..} -- :: Int8 -> BitVector 8
        | [DC _ [Left arg]] <- args
        -> whnfArg arg $ \mach2 -> \case
             Literal (Int8Literal i) ->
               let resTyInfo = extractTySizeInfo te sc ty tys
               in Just $ Next mach2 { mTerm = sink (mkBitVectorLit' resTyInfo 0 i) }
             _ -> Nothing
      _ -> Nothing

  , primStepEntry $(textNameLit 'Clash.Class.BitPack.Internal.packInt16#) $ \case
      PrimStepContext{..} -- :: Int16 -> BitVector 16
        | [DC _ [Left arg]] <- args
        -> whnfArg arg $ \mach2 -> \case
             Literal (Int16Literal i) ->
               let resTyInfo = extractTySizeInfo te sc ty tys
               in Just $ Next mach2 { mTerm = sink (mkBitVectorLit' resTyInfo 0 i) }
             _ -> Nothing
      _ -> Nothing

  , primStepEntry $(textNameLit 'Clash.Class.BitPack.Internal.packInt32#) $ \case
      PrimStepContext{..} -- :: Int32 -> BitVector 32
        | [DC _ [Left arg]] <- args
        -> whnfArg arg $ \mach2 -> \case
             Literal (Int32Literal i) ->
               let resTyInfo = extractTySizeInfo te sc ty tys
               in Just $ Next mach2 { mTerm = sink (mkBitVectorLit' resTyInfo 0 i) }
             _ -> Nothing
      _ -> Nothing

  , primStepEntry $(textNameLit 'Clash.Class.BitPack.Internal.packInt64#) $ \case
      PrimStepContext{..} -- :: Int64 -> BitVector 64
        | [DC _ [Left arg]] <- args
        -> whnfArg arg $ \mach2 -> \case
             Literal (Int64Literal i) ->
               let resTyInfo = extractTySizeInfo te sc ty tys
               in Just $ Next mach2 { mTerm = sink (mkBitVectorLit' resTyInfo 0 i) }
             _ -> Nothing
      _ -> Nothing

  , primStepEntry $(textNameLit 'Clash.Class.BitPack.Internal.packWord#) $ \case
      PrimStepContext{..} -- :: Word -> BitVector WORD_SIZE_IN_BITS
        | [DC _ [Left arg]] <- args
        -> whnfArg arg $ \mach2 -> \case
             Literal (WordLiteral i) ->
               let resTyInfo = extractTySizeInfo te sc ty tys
               in Just $ Next mach2 { mTerm = sink (mkBitVectorLit' resTyInfo 0 i) }
             _ -> Nothing
      _ -> Nothing

  , primStepEntry $(textNameLit 'Clash.Class.BitPack.Internal.packWord8#) $ \case
      PrimStepContext{..} -- :: Word8 -> BitVector 8
        | [DC _ [Left arg]] <- args
        -> whnfArg arg $ \mach2 -> \case
             Literal (Word8Literal i) ->
               let resTyInfo = extractTySizeInfo te sc ty tys
               in Just $ Next mach2 { mTerm = sink (mkBitVectorLit' resTyInfo 0 i) }
             _ -> Nothing
      _ -> Nothing

  , primStepEntry $(textNameLit 'Clash.Class.BitPack.Internal.packWord16#) $ \case
      PrimStepContext{..} -- :: Word16 -> BitVector 16
        | [DC _ [Left arg]] <- args
        -> whnfArg arg $ \mach2 -> \case
             Literal (Word16Literal i) ->
               let resTyInfo = extractTySizeInfo te sc ty tys
               in Just $ Next mach2 { mTerm = sink (mkBitVectorLit' resTyInfo 0 i) }
             _ -> Nothing
      _ -> Nothing

  , primStepEntry $(textNameLit 'Clash.Class.BitPack.Internal.packWord32#) $ \case
      PrimStepContext{..} -- :: Word32 -> BitVector 32
        | [DC _ [Left arg]] <- args
        -> whnfArg arg $ \mach2 -> \case
             Literal (Word32Literal i) ->
               let resTyInfo = extractTySizeInfo te sc ty tys
               in Just $ Next mach2 { mTerm = sink (mkBitVectorLit' resTyInfo 0 i) }
             _ -> Nothing
      _ -> Nothing

  , primStepEntry $(textNameLit 'Clash.Class.BitPack.Internal.packWord64#) $ \case
      PrimStepContext{..} -- :: Word64 -> BitVector 64
        | [DC _ [Left arg]] <- args
        -> whnfArg arg $ \mach2 -> \case
             Literal (Word64Literal i) ->
               let resTyInfo = extractTySizeInfo te sc ty tys
               in Just $ Next mach2 { mTerm = sink (mkBitVectorLit' resTyInfo 0 i) }
             _ -> Nothing
      _ -> Nothing

  , primStepEntry $(textNameLit 'Clash.Class.BitPack.Internal.packDouble#) $ \case
      PrimStepContext{..} -- :: Double -> BitVector 64
        | [DC _ [Left arg]] <- args
        -> whnfArg arg $ \mach2 -> \case
             Literal (DoubleLiteral i) ->
               let resTyInfo = extractTySizeInfo te sc ty tys
                   val = toInteger ((pack :: Word64 -> BitVector 64) i)
               in Just $ Next mach2 { mTerm = sink (mkBitVectorLit' resTyInfo 0 val) }
             _ -> Nothing
      _ -> Nothing

  , primStepEntry $(textNameLit 'Clash.Class.BitPack.Internal.packFloat#) $ \case
      PrimStepContext{..} -- :: Float -> BitVector 32
        | [DC _ [Left arg]] <- args
        -> whnfArg arg $ \mach2 -> \case
             Literal (FloatLiteral i) ->
               let resTyInfo = extractTySizeInfo te sc ty tys
                   val = toInteger ((pack :: Word32 -> BitVector 32) i)
               in Just $ Next mach2 { mTerm = sink (mkBitVectorLit' resTyInfo 0 val) }
             _ -> Nothing
      _ -> Nothing

  , primStepEntry $(textNameLit 'Clash.Class.BitPack.Internal.packCUShort#) $ \case
      PrimStepContext{..} -- :: CUShort -> BitVector 16
        | [DC _ [Left arg]] <- args
        -> whnfArg arg $ \mach2 -> \case
             Literal (Word16Literal i) ->
               let resTyInfo = extractTySizeInfo te sc ty tys
               in Just $ Next mach2 { mTerm = sink (mkBitVectorLit' resTyInfo 0 i) }
             _ -> Nothing
      _ -> Nothing

  , primStepEntry $(textNameLit 'Clash.Class.BitPack.Internal.unpackInt8#) $ \case
      PrimStepContext{..} -- BitVector 8 -> Int8
        | [i] <- bitVectorLiterals' args
        -> let resTy = getResultTy te sc ty tys
               val = toInteger (unpack (toBV i) :: Signed 8)
               proj = Int8Literal
            in reduce (mkIntCLit te proj val resTy)
      _ -> Nothing

  , primStepEntry $(textNameLit 'Clash.Class.BitPack.Internal.unpackInt16#) $ \case
      PrimStepContext{..} -- BitVector 16 -> Int16
        | [i] <- bitVectorLiterals' args
        -> let resTy = getResultTy te sc ty tys
               val = toInteger (unpack (toBV i) :: Signed 16)
               proj = Int16Literal
            in reduce (mkIntCLit te proj val resTy)
      _ -> Nothing

  , primStepEntry $(textNameLit 'Clash.Class.BitPack.Internal.unpackInt32#) $ \case
      PrimStepContext{..} -- BitVector 32 -> Int32
        | [i] <- bitVectorLiterals' args
        -> let resTy = getResultTy te sc ty tys
               val = toInteger (unpack (toBV i) :: Signed 32)
               proj = Int32Literal
            in reduce (mkIntCLit te proj val resTy)
      _ -> Nothing

  , primStepEntry $(textNameLit 'Clash.Class.BitPack.Internal.unpackInt64#) $ \case
      PrimStepContext{..} -- BitVector 64 -> Int64
        | [i] <- bitVectorLiterals' args
        -> let resTy = getResultTy te sc ty tys
               val = toInteger (unpack (toBV i) :: Signed 64)
               proj = Int64Literal
            in reduce (mkIntCLit te proj val resTy)
      _ -> Nothing

  , primStepEntry $(textNameLit 'Clash.Class.BitPack.Internal.unpackWord#) $ \case
      PrimStepContext{..} -- BitVector WORD_SIZE_IN_BITS -> Word
        | [i] <- bitVectorLiterals' args
        -> let resTy = getResultTy te sc ty tys
               val = toInteger (unpack (toBV i) :: Unsigned 64)
            in reduce (mkIntCLit te WordLiteral val resTy)
      _ -> Nothing

  , primStepEntry $(textNameLit 'Clash.Class.BitPack.Internal.unpackWord8#) $ \case
      PrimStepContext{..} -- BitVector 8 -> Word8
        | [i] <- bitVectorLiterals' args
        -> let resTy = getResultTy te sc ty tys
               val = toInteger (unpack (toBV i) :: Unsigned 8)
               proj = Word8Literal
            in reduce (mkIntCLit te proj val resTy)
      _ -> Nothing

  , primStepEntry $(textNameLit 'Clash.Class.BitPack.Internal.unpackWord16#) $ \case
      PrimStepContext{..} -- BitVector 16 -> Word16
        | [i] <- bitVectorLiterals' args
        -> let resTy = getResultTy te sc ty tys
               val = toInteger (unpack (toBV i) :: Unsigned 16)
               proj = Word16Literal
            in reduce (mkIntCLit te proj val resTy)
      _ -> Nothing

  , primStepEntry $(textNameLit 'Clash.Class.BitPack.Internal.unpackWord32#) $ \case
      PrimStepContext{..} -- BitVector 32 -> Word32
        | [i] <- bitVectorLiterals' args
        -> let resTy = getResultTy te sc ty tys
               val = toInteger (unpack (toBV i) :: Unsigned 32)
               proj = Word32Literal
            in reduce (mkIntCLit te proj val resTy)
      _ -> Nothing

  , primStepEntry $(textNameLit 'Clash.Class.BitPack.Internal.unpackWord64#) $ \case
      PrimStepContext{..} -- BitVector 64 -> Word64
        | [i] <- bitVectorLiterals' args
        -> let resTy = getResultTy te sc ty tys
               val = toInteger (unpack (toBV i) :: Unsigned 64)
               proj = Word64Literal
            in reduce (mkIntCLit te proj val resTy)
      _ -> Nothing

  , primStepEntry $(textNameLit 'Clash.Class.BitPack.Internal.unpackFloat#) $ \case
      PrimStepContext{..}
        | [i] <- bitVectorLiterals' args
        -> let resTy = getResultTy te sc ty tys
               val = unpack (toBV i :: BitVector 32)
            in reduce (mkFloatCLit te val resTy)
      _ -> Nothing

  , primStepEntry $(textNameLit 'Clash.Class.BitPack.Internal.unpackDouble#) $ \case
      PrimStepContext{..}
        | [i] <- bitVectorLiterals' args
        -> let resTy = getResultTy te sc ty tys
               val = unpack (toBV i :: BitVector 64)
            in reduce (mkDoubleCLit te val resTy)
      _ -> Nothing

  , primStepEntry $(textNameLit 'Clash.Class.BitPack.Internal.unpackCUShort#) $ \case
      PrimStepContext{..}
        | [i] <- bitVectorLiterals' args
        -> let resTy = getResultTy te sc ty tys
               val = toInteger (unpack (toBV i) :: Unsigned 16)
               proj = Word16Literal
            in reduce (mkIntCLit te proj val resTy)
      _ -> Nothing

  ]
