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

{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UnboxedTuples #-}

{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Clash.Core.Scoped.Evaluator.Primitives.GHC.Prim
  ( primitives
  ) where

import Data.Bits
import Data.Char (ord)
import qualified Data.Primitive.ByteArray as BA
import Data.Text (Text)
import GHC.Exts (Int (..))
import GHC.Prim
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

  , primStepEntry $(textNameLit 'GHC.Prim.quotInt#) $ \case
      PrimStepContext{..} | Just (i,j) <- intLiterals args
        -> reduce $ catchDivByZero (integerToIntLiteral (i `quot` j))
      _ -> Nothing

  , primStepEntry $(textNameLit 'GHC.Prim.quotRemInt#) $ \case
      PrimStepContext{..} | Just (i,j) <- intLiterals args
        -> let (q,r) = quotRem i j
           in  reduce $ mkResultCon te sc ty tys
                 [ catchDivByZero (integerToIntLiteral q)
                 , catchDivByZero (integerToIntLiteral r) ]
      _ -> Nothing

  -- The same as the entries above, with a combinator
  , liftLit2 $(textNameLit 'GHC.Prim.andI#) intLiteral (\i j -> integerToIntLiteral (i .&. j))

  , liftLit1 $(textNameLit 'GHC.Prim.notI#) intLiteral (integerToIntLiteral . complement)

  , primStepEntry $(textNameLit '(GHC.Prim.==#)) $ \case
      PrimStepContext{..} | Just (i,j) <- intLiterals args
        -> reduce (boolToIntLiteral (i == j))
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
