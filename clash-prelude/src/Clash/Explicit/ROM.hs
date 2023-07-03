{-|
Copyright  :  (C) 2015-2016, University of Twente,
                  2017     , Google Inc.
                  2019     , Myrtle Software Ltd,
                  2021-2022, QBayLogic B.V.
License    :  BSD2 (see the file LICENSE)
Maintainer :  QBayLogic B.V. <devops@qbaylogic.com>

ROMs
-}

{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}

{-# LANGUAGE Trustworthy #-}

{-# OPTIONS_GHC -fplugin GHC.TypeLits.KnownNat.Solver #-}
{-# OPTIONS_HADDOCK show-extensions #-}

module Clash.Explicit.ROM
  ( -- * Synchronous ROM synchronized to an arbitrary clock
    rom
  , romPow2
    -- * Internal
  , rom#
  )
where

import Data.Array             (listArray)
import Data.Array.Base        (unsafeAt)
import GHC.Stack              (withFrozenCallStack)
import GHC.TypeLits           (KnownNat, type (^))
import Prelude hiding         (length)

import Clash.Annotations.Primitive (hasBlackBox)
import Clash.Signal.Internal
  (Clock (..), KnownDomain, Signal (..), Enable, fromEnable)
import Clash.Sized.Unsigned   (Unsigned)
import Clash.Sized.Vector     (Vec, length, toList)
import Clash.XException       (deepErrorX, seqX, NFDataX)

-- | A ROM with a synchronous read port, with space for 2^@n@ elements
--
-- * __NB__: Read value is delayed by 1 cycle
-- * __NB__: Initial output value is /undefined/, reading it will throw an
-- 'Clash.XException.XException'
--
-- === See also:
--
-- * See "Clash.Sized.Fixed#creatingdatafiles" and "Clash.Explicit.BlockRam#usingrams"
-- for ideas on how to use ROMs and RAMs.
-- * A large 'Vec' for the content may be too inefficient, depending on how it
-- is constructed. See 'Clash.Explicit.ROM.File.romFilePow2' and
-- 'Clash.Explicit.ROM.Blob.romBlobPow2' for different approaches that scale
-- well.
romPow2
  :: (KnownDomain dom, KnownNat n, NFDataX a)
  => Clock dom
  -- ^ 'Clock' to synchronize to
  -> Enable dom
  -- ^ 'Enable' line
  -> Vec (2^n) a
  -- ^ ROM content
  --
  -- __NB__: __MUST__ be a constant
  -> Signal dom (Unsigned n)
  -- ^ Read address @r@
  -> Signal dom a
  -- ^ The value of the ROM at address @r@ from the previous clock cycle
romPow2 = rom
{-# INLINE romPow2 #-}

-- | A ROM with a synchronous read port, with space for @n@ elements
--
-- * __NB__: Read value is delayed by 1 cycle
-- * __NB__: Initial output value is /undefined/, reading it will throw an
-- 'Clash.XException.XException'
--
-- === See also:
--
-- * See "Clash.Sized.Fixed#creatingdatafiles" and "Clash.Explicit.BlockRam#usingrams"
-- for ideas on how to use ROMs and RAMs.
-- * A large 'Vec' for the content may be too inefficient, depending on how it
-- is constructed. See 'Clash.Explicit.ROM.File.romFile' and
-- 'Clash.Explicit.ROM.Blob.romBlob' for different approaches that scale well.
rom
  :: (KnownDomain dom, KnownNat n, NFDataX a, Enum addr)
  => Clock dom
  -- ^ 'Clock' to synchronize to
  -> Enable dom
  -- ^ 'Enable' line
  -> Vec n a
  -- ^ ROM content, also determines the size, @n@, of the ROM
  --
  -- __NB__: __MUST__ be a constant
  -> Signal dom addr
  -- ^ Read address @r@
  -> Signal dom a
  -- ^ The value of the ROM at address @r@ from the previous clock cycle
rom = \clk en content rd -> rom# clk en content (fromEnum <$> rd)
{-# INLINE rom #-}

-- | ROM primitive
rom#
  :: forall dom n a
   . (KnownDomain dom, KnownNat n, NFDataX a)
  => Clock dom
  -- ^ 'Clock' to synchronize to
  -> Enable dom
  -- ^ 'Enable' line
  -> Vec n a
  -- ^ ROM content, also determines the size, @n@, of the ROM
  --
  -- __NB__: __MUST__ be a constant
  -> Signal dom Int
  -- ^ Read address @rd@
  -> Signal dom a
  -- ^ The value of the ROM at address @rd@ from the previous clock cycle
rom# !_ en content =
  go
    (withFrozenCallStack (deepErrorX "rom: initial value undefined"))
    (fromEnable en)
 where
  szI = length content
  arr = listArray (0,szI-1) (toList content)

  go o (e :- es) rd@(~(r :- rs)) =
    let o1 = if e then safeAt r else o
    -- See [Note: register strictness annotations]
    in  o `seqX` o :- (rd `seq` go o1 es rs)

  safeAt :: Int -> a
  safeAt i =
    if (0 <= i) && (i < szI) then
      unsafeAt arr i
    else
      withFrozenCallStack
        (deepErrorX ("rom: address " ++ show i ++
                     " not in range [0.." ++ show szI ++ ")"))
  {-# INLINE safeAt #-}
-- See: https://github.com/clash-lang/clash-compiler/pull/2511
{-# CLASH_OPAQUE rom# #-}
{-# ANN rom# hasBlackBox #-}
