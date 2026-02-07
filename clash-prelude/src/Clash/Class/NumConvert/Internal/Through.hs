{-# LANGUAGE CPP #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_HADDOCK hide #-}

#include "MachDeps.h"

{- |
Copyright  :  (C) 2026     , Martijn Bastiaan
License    :  BSD2 (see the file LICENSE)
Maintainer :  QBayLogic B.V. <devops@qbaylogic.com>
-}
module Clash.Class.NumConvert.Internal.Through where

import Clash.Sized.BitVector
import Clash.Sized.Index
import Clash.Sized.Signed
import Clash.Sized.Unsigned

import Data.Int (Int16, Int32, Int64, Int8)
import Data.Word (Word16, Word32, Word64, Word8)

-- | Type family mapping types to their canonical \"unwrapped\" Clash form.
-- This is used by 'Clash.Class.NumConvert.numConvertThrough' and
-- 'Clash.Class.NumConvert.maybeNumConvertThrough' to determine the intermediate type.
type family Through a

-- Instances for Data.Word types
type instance Through Word = Unsigned WORD_SIZE_IN_BITS
type instance Through Word64 = Unsigned 64
type instance Through Word32 = Unsigned 32
type instance Through Word16 = Unsigned 16
type instance Through Word8 = Unsigned 8

-- Instances for Data.Int types
type instance Through Int = Signed WORD_SIZE_IN_BITS
type instance Through Int64 = Signed 64
type instance Through Int32 = Signed 32
type instance Through Int16 = Signed 16
type instance Through Int8 = Signed 8

-- Bit
type instance Through Bit = BitVector 1

-- Clash types are already in canonical form
type instance Through (Unsigned n) = Unsigned n
type instance Through (Signed n) = Signed n
type instance Through (BitVector n) = BitVector n
type instance Through (Index n) = Index n
