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
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UnboxedTuples #-}

#include "MachDeps.h"

module Clash.GHC.Evaluator.Primitives.Clash.Class.BitPack.Internal
  ( primitives
  ) where

import           Data.Text           (Text)
import           GHC.Word

import           Clash.Class.BitPack (pack,unpack)
import           Clash.Core.Evaluator.Types
import           Clash.Core.Literal  (Literal (..))
import Clash.Core.Term (Term (..))
import Clash.Util (textNameLit)

import Clash.Sized.Internal.BitVector (BitVector(..))
import Clash.Sized.Internal.Signed   (Signed   (..))
import Clash.Sized.Internal.Unsigned (Unsigned (..))

import {-# SOURCE #-} Clash.GHC.Evaluator

import qualified Clash.Class.BitPack.Internal

import {-# SOURCE #-} Clash.GHC.Evaluator.Primitive
import Clash.GHC.Evaluator.Primitive.Util

primitives :: [(Text, PrimStep)]
primitives =
  [ ( $(textNameLit 'Clash.Class.BitPack.Internal.packInt8#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} -- :: Int8 -> BitVector 8
            | [DC _ [Left arg]] <- args
              , eval <- Evaluator ghcStep ghcUnwind ghcPrimStep ghcPrimUnwind
#if MIN_VERSION_base(4,16,0)
              , mach2@Machine{mStack=[],mTerm=Literal (Int8Literal i)} <-
                  whnf eval tcm True (setTerm arg $ stackClear mach)
#else
              , mach2@Machine{mStack=[],mTerm=Literal (IntLiteral i)} <-
                  whnf eval tcm True (setTerm arg $ stackClear mach)
#endif
              -> let resTyInfo = extractTySizeInfo tcm ty tys
                  in Just $ mach2
                      { mStack = mStack mach
                      , mTerm = mkBitVectorLit' resTyInfo 0 i
                      }
          _ -> Nothing
    )

  , ( $(textNameLit 'Clash.Class.BitPack.Internal.packInt16#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} -- :: Int16 -> BitVector 16
            | [DC _ [Left arg]] <- args
              , eval <- Evaluator ghcStep ghcUnwind ghcPrimStep ghcPrimUnwind
#if MIN_VERSION_base(4,16,0)
              , mach2@Machine{mStack=[],mTerm=Literal (Int16Literal i)} <-
                  whnf eval tcm True (setTerm arg $ stackClear mach)
#else
              , mach2@Machine{mStack=[],mTerm=Literal (IntLiteral i)} <-
                  whnf eval tcm True (setTerm arg $ stackClear mach)
#endif
              -> let resTyInfo = extractTySizeInfo tcm ty tys
                  in Just $ mach2
                      { mStack = mStack mach
                      , mTerm = mkBitVectorLit' resTyInfo 0 i
                      }
          _ -> Nothing
    )

  , ( $(textNameLit 'Clash.Class.BitPack.Internal.packInt32#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} -- :: Int32 -> BitVector 32
            | [DC _ [Left arg]] <- args
              , eval <- Evaluator ghcStep ghcUnwind ghcPrimStep ghcPrimUnwind
#if MIN_VERSION_base(4,16,0)
              , mach2@Machine{mStack=[],mTerm=Literal (Int32Literal i)} <-
                  whnf eval tcm True (setTerm arg $ stackClear mach)
#else
              , mach2@Machine{mStack=[],mTerm=Literal (IntLiteral i)} <-
                  whnf eval tcm True (setTerm arg $ stackClear mach)
#endif
              -> let resTyInfo = extractTySizeInfo tcm ty tys
                  in Just $ mach2
                      { mStack = mStack mach
                      , mTerm = mkBitVectorLit' resTyInfo 0 i
                      }
          _ -> Nothing
    )

  , ( $(textNameLit 'Clash.Class.BitPack.Internal.packInt64#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} -- :: Int64 -> BitVector 64
            | [DC _ [Left arg]] <- args
              , eval <- Evaluator ghcStep ghcUnwind ghcPrimStep ghcPrimUnwind
#if MIN_VERSION_base(4,16,0)
              , mach2@Machine{mStack=[],mTerm=Literal (Int64Literal i)} <-
                  whnf eval tcm True (setTerm arg $ stackClear mach)
#else
              , mach2@Machine{mStack=[],mTerm=Literal (IntLiteral i)} <-
                  whnf eval tcm True (setTerm arg $ stackClear mach)
#endif
              -> let resTyInfo = extractTySizeInfo tcm ty tys
                  in Just $ mach2
                      { mStack = mStack mach
                      , mTerm = mkBitVectorLit' resTyInfo 0 i
                      }
          _ -> Nothing
    )

  , ( $(textNameLit 'Clash.Class.BitPack.Internal.packWord#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} -- :: Word -> BitVector WORD_SIZE_IN_BITS
            | [DC _ [Left arg]] <- args
              , eval <- Evaluator ghcStep ghcUnwind ghcPrimStep ghcPrimUnwind
              , mach2@Machine{mStack=[],mTerm=Literal (WordLiteral i)} <-
                  whnf eval tcm True (setTerm arg $ stackClear mach)
              -> let resTyInfo = extractTySizeInfo tcm ty tys
                  in Just $ mach2
                      { mStack = mStack mach
                      , mTerm = mkBitVectorLit' resTyInfo 0 i
                      }
          _ -> Nothing
    )

  , ( $(textNameLit 'Clash.Class.BitPack.Internal.packWord8#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} -- :: Word8 -> BitVector 8
            | [DC _ [Left arg]] <- args
              , eval <- Evaluator ghcStep ghcUnwind ghcPrimStep ghcPrimUnwind
#if MIN_VERSION_base(4,16,0)
              , mach2@Machine{mStack=[],mTerm=Literal (Word8Literal i)} <-
                  whnf eval tcm True (setTerm arg $ stackClear mach)
#else
              , mach2@Machine{mStack=[],mTerm=Literal (WordLiteral i)} <-
                  whnf eval tcm True (setTerm arg $ stackClear mach)
#endif
              -> let resTyInfo = extractTySizeInfo tcm ty tys
                  in Just $ mach2
                      { mStack = mStack mach
                      , mTerm = mkBitVectorLit' resTyInfo 0 i
                      }
          _ -> Nothing
    )

  , ( $(textNameLit 'Clash.Class.BitPack.Internal.packWord16#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} -- :: Word16 -> BitVector 16
            | [DC _ [Left arg]] <- args
              , eval <- Evaluator ghcStep ghcUnwind ghcPrimStep ghcPrimUnwind
#if MIN_VERSION_base(4,16,0)
              , mach2@Machine{mStack=[],mTerm=Literal (Word16Literal i)} <-
                  whnf eval tcm True (setTerm arg $ stackClear mach)
#else
              , mach2@Machine{mStack=[],mTerm=Literal (WordLiteral i)} <-
                  whnf eval tcm True (setTerm arg $ stackClear mach)
#endif
              -> let resTyInfo = extractTySizeInfo tcm ty tys
                  in Just $ mach2
                      { mStack = mStack mach
                      , mTerm = mkBitVectorLit' resTyInfo 0 i
                      }
          _ -> Nothing
    )

  , ( $(textNameLit 'Clash.Class.BitPack.Internal.packWord32#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} -- :: Word32 -> BitVector 32
            | [DC _ [Left arg]] <- args
              , eval <- Evaluator ghcStep ghcUnwind ghcPrimStep ghcPrimUnwind
#if MIN_VERSION_base(4,16,0)
              , mach2@Machine{mStack=[],mTerm=Literal (Word32Literal i)} <-
                  whnf eval tcm True (setTerm arg $ stackClear mach)
#else
              , mach2@Machine{mStack=[],mTerm=Literal (WordLiteral i)} <-
                  whnf eval tcm True (setTerm arg $ stackClear mach)
#endif
              -> let resTyInfo = extractTySizeInfo tcm ty tys
                  in Just $ mach2
                      { mStack = mStack mach
                      , mTerm = mkBitVectorLit' resTyInfo 0 i
                      }
          _ -> Nothing
    )

  , ( $(textNameLit 'Clash.Class.BitPack.Internal.packWord64#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} -- :: Word64 -> BitVector 64
            | [DC _ [Left arg]] <- args
              , eval <- Evaluator ghcStep ghcUnwind ghcPrimStep ghcPrimUnwind
#if MIN_VERSION_base(4,16,0)
              , mach2@Machine{mStack=[],mTerm=Literal (Word64Literal i)} <-
                  whnf eval tcm True (setTerm arg $ stackClear mach)
#else
              , mach2@Machine{mStack=[],mTerm=Literal (WordLiteral i)} <-
                  whnf eval tcm True (setTerm arg $ stackClear mach)
#endif
              -> let resTyInfo = extractTySizeInfo tcm ty tys
                  in Just $ mach2
                      { mStack = mStack mach
                      , mTerm = mkBitVectorLit' resTyInfo 0 i
                      }
          _ -> Nothing
    )

  , ( $(textNameLit 'Clash.Class.BitPack.Internal.packDouble#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} -- :: Double -> BitVector 64
            | [DC _ [Left arg]] <- args
            , eval <- Evaluator ghcStep ghcUnwind ghcPrimStep ghcPrimUnwind
            , mach2@Machine{mStack=[],mTerm=Literal (DoubleLiteral i)} <- whnf eval tcm True (setTerm arg $ stackClear mach)
            -> let resTyInfo = extractTySizeInfo tcm ty tys
                in Just $ mach2
                     { mStack = mStack mach
                     , mTerm = mkBitVectorLit' resTyInfo 0 (toInteger $ (pack :: Word64 -> BitVector 64) i)
                     }
          _ -> Nothing
    )

  , ( $(textNameLit 'Clash.Class.BitPack.Internal.packFloat#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} -- :: Float -> BitVector 32
            | [DC _ [Left arg]] <- args
            , eval <- Evaluator ghcStep ghcUnwind ghcPrimStep ghcPrimUnwind
            , mach2@Machine{mStack=[],mTerm=Literal (FloatLiteral i)} <- whnf eval tcm True (setTerm arg $ stackClear mach)
            -> let resTyInfo = extractTySizeInfo tcm ty tys
                in Just $ mach2
                     { mStack = mStack mach
                     , mTerm = mkBitVectorLit' resTyInfo 0 (toInteger $ (pack :: Word32 -> BitVector 32) i)
                     }
          _ -> Nothing
    )

  , ( $(textNameLit 'Clash.Class.BitPack.Internal.packCUShort#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} -- :: CUShort -> BitVector 16
            | [DC _ [Left arg]] <- args
              , eval <- Evaluator ghcStep ghcUnwind ghcPrimStep ghcPrimUnwind
#if MIN_VERSION_base(4,16,0)
              , mach2@Machine{mStack=[],mTerm=Literal (Word16Literal i)}
                  <- whnf eval tcm True (setTerm arg $ stackClear mach)
#else
              , mach2@Machine{mStack=[],mTerm=Literal (WordLiteral i)}
                  <- whnf eval tcm True (setTerm arg $ stackClear mach)
#endif
              -> let resTyInfo = extractTySizeInfo tcm ty tys
                  in Just $ mach2
                      { mStack = mStack mach
                      , mTerm = mkBitVectorLit' resTyInfo 0 i
                      }
          _ -> Nothing
    )

  , ( $(textNameLit 'Clash.Class.BitPack.Internal.unpackInt8#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} -- BitVector 8 -> Int8
            | [i] <- bitVectorLiterals' args
            -> let resTy = getResultTy tcm ty tys
                   val = toInteger (unpack (toBV i) :: Signed 8)
#if MIN_VERSION_base(4,16,0)
                   proj = Int8Literal
#else
                   proj = IntLiteral
#endif
                in reduce (mkIntCLit tcm proj val resTy)
          _ -> Nothing
    )

  , ( $(textNameLit 'Clash.Class.BitPack.Internal.unpackInt16#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} -- BitVector 16 -> Int16
            | [i] <- bitVectorLiterals' args
            -> let resTy = getResultTy tcm ty tys
                   val = toInteger (unpack (toBV i) :: Signed 16)
#if MIN_VERSION_base(4,16,0)
                   proj = Int16Literal
#else
                   proj = IntLiteral
#endif
                in reduce (mkIntCLit tcm proj val resTy)
          _ -> Nothing
    )

  , ( $(textNameLit 'Clash.Class.BitPack.Internal.unpackInt32#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} -- BitVector 32 -> Int32
            | [i] <- bitVectorLiterals' args
            -> let resTy = getResultTy tcm ty tys
                   val = toInteger (unpack (toBV i) :: Signed 32)
#if MIN_VERSION_base(4,16,0)
                   proj = Int32Literal
#else
                   proj = IntLiteral
#endif
                in reduce (mkIntCLit tcm proj val resTy)
          _ -> Nothing
    )

  , ( $(textNameLit 'Clash.Class.BitPack.Internal.unpackInt64#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} -- BitVector 64 -> Int64
            | [i] <- bitVectorLiterals' args
            -> let resTy = getResultTy tcm ty tys
                   val = toInteger (unpack (toBV i) :: Signed 64)
#if MIN_VERSION_base(4,16,0)
                   proj = Int64Literal
#else
                   proj = IntLiteral
#endif
                in reduce (mkIntCLit tcm proj val resTy)
          _ -> Nothing
    )

  , ( $(textNameLit 'Clash.Class.BitPack.Internal.unpackWord#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} -- BitVector WORD_SIZE_IN_BITS -> Word
            | [i] <- bitVectorLiterals' args
            -> let resTy = getResultTy tcm ty tys
                   val = toInteger (unpack (toBV i) :: Unsigned 64)
                in reduce (mkIntCLit tcm WordLiteral val resTy)
          _ -> Nothing
    )

  , ( $(textNameLit 'Clash.Class.BitPack.Internal.unpackWord8#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} -- BitVector 8 -> Word8
            | [i] <- bitVectorLiterals' args
            -> let resTy = getResultTy tcm ty tys
                   val = toInteger (unpack (toBV i) :: Unsigned 8)
#if MIN_VERSION_base(4,16,0)
                   proj = Word8Literal
#else
                   proj = WordLiteral
#endif
                in reduce (mkIntCLit tcm proj val resTy)
          _ -> Nothing
    )

  , ( $(textNameLit 'Clash.Class.BitPack.Internal.unpackWord16#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} -- BitVector 16 -> Word16
            | [i] <- bitVectorLiterals' args
            -> let resTy = getResultTy tcm ty tys
                   val = toInteger (unpack (toBV i) :: Unsigned 16)
#if MIN_VERSION_base(4,16,0)
                   proj = Word16Literal
#else
                   proj = WordLiteral
#endif
                in reduce (mkIntCLit tcm proj val resTy)
          _ -> Nothing
    )

  , ( $(textNameLit 'Clash.Class.BitPack.Internal.unpackWord32#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} -- BitVector 32 -> Word32
            | [i] <- bitVectorLiterals' args
            -> let resTy = getResultTy tcm ty tys
                   val = toInteger (unpack (toBV i) :: Unsigned 32)
#if MIN_VERSION_base(4,16,0)
                   proj = Word32Literal
#else
                   proj = WordLiteral
#endif
                in reduce (mkIntCLit tcm proj val resTy)
          _ -> Nothing
    )

  , ( $(textNameLit 'Clash.Class.BitPack.Internal.unpackWord64#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} -- BitVector 64 -> Word64
            | [i] <- bitVectorLiterals' args
            -> let resTy = getResultTy tcm ty tys
                   val = toInteger (unpack (toBV i) :: Unsigned 64)
#if MIN_VERSION_base(4,16,0)
                   proj = Word64Literal
#else
                   proj = WordLiteral
#endif
                in reduce (mkIntCLit tcm proj val resTy)
          _ -> Nothing
    )

  , ( $(textNameLit 'Clash.Class.BitPack.Internal.unpackFloat#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..}
            | [i] <- bitVectorLiterals' args
            -> let resTy = getResultTy tcm ty tys
                   val = unpack (toBV i :: BitVector 32)
                in reduce (mkFloatCLit tcm val resTy)
          _ -> Nothing
    )

  , ( $(textNameLit 'Clash.Class.BitPack.Internal.unpackDouble#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..}
            | [i] <- bitVectorLiterals' args
            -> let resTy = getResultTy tcm ty tys
                   val = unpack (toBV i :: BitVector 64)
                in reduce (mkDoubleCLit tcm val resTy)
          _ -> Nothing
    )

  , ( $(textNameLit 'Clash.Class.BitPack.Internal.unpackCUShort#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..}
            | [i] <- bitVectorLiterals' args
            -> let resTy = getResultTy tcm ty tys
                   val = toInteger (unpack (toBV i) :: Unsigned 16)
#if MIN_VERSION_base(4,16,0)
                   proj = Word16Literal
#else
                   proj = WordLiteral
#endif
                in reduce (mkIntCLit tcm proj val resTy)
          _ -> Nothing
    )
  ]
