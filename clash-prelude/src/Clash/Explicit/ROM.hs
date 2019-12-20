{-|
Copyright  :  (C) 2015-2016, University of Twente,
                  2017     , Google Inc.
                  2019     , Myrtle Software Ltd
License    :  BSD2 (see the file LICENSE)
Maintainer :  Christiaan Baaij <christiaan.baaij@gmail.com>

ROMs
-}

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

import Data.Array             ((!),listArray)
import GHC.Stack              (withFrozenCallStack)
import GHC.TypeLits           (KnownNat, type (^))
import Prelude hiding         (length)

import Clash.Signal.Internal
  (Clock (..), KnownDomain, Signal (..), Enable, fromEnable)
import Clash.Sized.Unsigned   (Unsigned)
import Clash.Sized.Vector     (Vec, length, toList)
import Clash.XException       (deepErrorX, seqX, NFDataX)

-- | A ROM with a synchronous read port, with space for 2^@n@ elements
--
-- * __NB__: Read value is delayed by 1 cycle
-- * __NB__: Initial output value is 'undefined'
--
-- Additional helpful information:
--
-- * See "Clash.Sized.Fixed#creatingdatafiles" and "Clash.Explicit.BlockRam#usingrams"
-- for ideas on how to use ROMs and RAMs
romPow2
  :: (KnownDomain dom, KnownNat n, NFDataX a)
  => Clock dom
  -- ^ 'Clock' to synchronize to
  -> Enable dom
  -- ^ Global enable
  -> Vec (2^n) a
  -- ^ ROM content
  --
  -- __NB:__ must be a constant
  -> Signal dom (Unsigned n)
  -- ^ Read address @rd@
  -> Signal dom a
  -- ^ The value of the ROM at address @rd@
romPow2 = rom
{-# INLINE romPow2 #-}

-- | A ROM with a synchronous read port, with space for @n@ elements
--
-- * __NB__: Read value is delayed by 1 cycle
-- * __NB__: Initial output value is 'undefined'
--
-- Additional helpful information:
--
-- * See "Clash.Sized.Fixed#creatingdatafiles" and "Clash.Explicit.BlockRam#usingrams"
-- for ideas on how to use ROMs and RAMs
rom
  :: (KnownDomain dom, KnownNat n, NFDataX a, Enum addr)
  => Clock dom
  -- ^ 'Clock' to synchronize to
  -> Enable dom
  -- ^ Global enable
  -> Vec n a
  -- ^ ROM content
  --
  -- __NB:__ must be a constant
  -> Signal dom addr
  -- ^ Read address @rd@
  -> Signal dom a
  -- ^ The value of the ROM at address @rd@ from the previous clock cycle
rom = \clk en content rd -> rom# clk en content (fromEnum <$> rd)
{-# INLINE rom #-}

-- | ROM primitive
rom#
  :: forall dom n a
   . (KnownDomain dom, KnownNat n, NFDataX a)
  => Clock dom
  -- ^ 'Clock' to synchronize to
  -> Enable dom
  -- ^ Global enable
  -> Vec n a
  -- ^ ROM content
  --
  -- __NB:__ must be a constant
  -> Signal dom Int
  -- ^ Read address @rd@
  -> Signal dom a
  -- ^ The value of the ROM at address @rd@ from the previous clock cycle
rom# _ en content rd =
  go
    (withFrozenCallStack (deepErrorX "rom: initial value undefined"))
    (fromEnable en)
    ((arr !) <$> rd)
 where
  szI = length content
  arr = listArray (0,szI-1) (toList content)

  go o (e :- es) as@(~(x :- xs)) =
    -- See [Note: register strictness annotations]
    o `seqX` o :- (as `seq` if e then go x es xs else go o es xs)
{-# NOINLINE rom# #-}
