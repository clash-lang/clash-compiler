{-# LANGUAGE CPP #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_HADDOCK hide #-}

#include "MachDeps.h"

{- |
Copyright  :  (C) 2026     , Martijn Bastiaan
License    :  BSD2 (see the file LICENSE)
Maintainer :  QBayLogic B.V. <devops@qbaylogic.com>
-}
module Clash.Class.NumConvert.Internal.Canonical where

import Clash.Sized.BitVector
import Clash.Sized.Index
import Clash.Sized.Signed
import Clash.Sized.Unsigned

import Data.Int (Int16, Int32, Int64, Int8)
import Data.Word (Word16, Word32, Word64, Word8)

-- | Type family mapping types to their canonical \"unwrapped\" Clash form.
-- This is used by 'Clash.Class.NumConvert.numConvert' and
-- 'Clash.Class.NumConvert.maybeNumConvert' to determine the intermediate type.
type family Canonical a

-- Instances for Data.Word types
type instance Canonical Word = Unsigned WORD_SIZE_IN_BITS
type instance Canonical Word64 = Unsigned 64
type instance Canonical Word32 = Unsigned 32
type instance Canonical Word16 = Unsigned 16
type instance Canonical Word8 = Unsigned 8

-- Instances for Data.Int types
type instance Canonical Int = Signed WORD_SIZE_IN_BITS
type instance Canonical Int64 = Signed 64
type instance Canonical Int32 = Signed 32
type instance Canonical Int16 = Signed 16
type instance Canonical Int8 = Signed 8

-- Bit
type instance Canonical Bit = BitVector 1

-- Clash types are already in canonical form
type instance Canonical (Unsigned n) = Unsigned n
type instance Canonical (Signed n) = Signed n
type instance Canonical (BitVector n) = BitVector n
type instance Canonical (Index n) = Index n
