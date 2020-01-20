{-|
Copyright  :  (C) 2013-2016, University of Twente
License    :  BSD2 (see the file LICENSE)
Maintainer :  Christiaan Baaij <christiaan.baaij@gmail.com>
-}

{-# LANGUAGE Trustworthy #-}

{-# OPTIONS_GHC -fplugin GHC.TypeLits.Extra.Solver -fplugin GHC.TypeLits.KnownNat.Solver #-}
{-# OPTIONS_GHC -fplugin GHC.TypeLits.Normalise  #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_HADDOCK show-extensions #-}

module Clash.Sized.Index
  (Index, bv2i, fromSNat)
where

import GHC.TypeLits               (KnownNat, type (^))
import GHC.TypeLits.Extra         (CLog) -- documentation only

import Clash.Promoted.Nat         (SNat (..), pow2SNat)
import Clash.Sized.BitVector      (BitVector)
import Clash.Sized.Internal.Index

-- | An alternative implementation of 'Clash.Class.BitPack.unpack' for the
-- 'Index' data type; for when you know the size of the 'BitVector' and want
-- to determine the size of the 'Index'.
--
-- That is, the type of 'Clash.Class.BitPack.unpack' is:
--
-- @
-- __unpack__ :: 'BitVector' ('CLog' 2 n) -> 'Index' n
-- @
--
-- And is useful when you know the size of the 'Index', and want to get a value
-- from a 'BitVector' that is large enough (@CLog 2 n@) enough to hold an
-- 'Index'. Note that 'Clash.Class.BitPack.unpack' can fail at /run-time/ when
-- the value inside the 'BitVector' is higher than 'n-1'.
--
-- 'bv2i' on the other hand will /never/ fail at run-time, because the
-- 'BitVector' argument determines the size.
bv2i :: KnownNat n => BitVector n -> Index (2^n)
bv2i = unpack#
