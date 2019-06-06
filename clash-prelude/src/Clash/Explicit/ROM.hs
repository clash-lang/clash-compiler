{-|
Copyright  :  (C) 2015-2016, University of Twente,
                  2017     , Google Inc.
License    :  BSD2 (see the file LICENSE)
Maintainer :  Christiaan Baaij <christiaan.baaij@gmail.com>

ROMs
-}

{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE GADTs         #-}
{-# LANGUAGE MagicHash     #-}
{-# LANGUAGE TypeOperators #-}

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

import Clash.Signal.Internal  (Clock (..), Signal (..))
import Clash.Sized.Unsigned   (Unsigned)
import Clash.Sized.Vector     (Vec, length, toList)
import Clash.XException       (errorX, seqX)

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
  :: KnownNat n
  => Clock domain gated         -- ^ 'Clock' to synchronize to
  -> Vec (2^n) a                -- ^ ROM content
                                --
                                -- __NB:__ must be a constant
  -> Signal domain (Unsigned n) -- ^ Read address @rd@
  -> Signal domain a            -- ^ The value of the ROM at address @rd@
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
  :: (KnownNat n, Enum addr)
  => Clock domain gated -- ^ 'Clock' to synchronize to
  -> Vec n a            -- ^ ROM content
                        --
                        -- __NB:__ must be a constant
  -> Signal domain addr -- ^ Read address @rd@
  -> Signal domain a
  -- ^ The value of the ROM at address @rd@ from the previous clock cycle
rom = \clk content rd -> rom# clk content (fromEnum <$> rd)
{-# INLINE rom #-}

-- | ROM primitive
rom#
  :: KnownNat n
  => Clock domain gated -- ^ 'Clock' to synchronize to
  -> Vec n a            -- ^ ROM content
                        --
                        -- __NB:__ must be a constant
  -> Signal domain Int  -- ^ Read address @rd@
  -> Signal domain a
  -- ^ The value of the ROM at address @rd@ from the previous clock cycle
rom# clk content rd = go clk ((arr !) <$> rd)
  where
    szI = length content
    arr = listArray (0,szI-1) (toList content)

    go :: Clock domain gated
       -> Signal domain a
       -> Signal domain a
    go Clock {} =
      \s -> withFrozenCallStack (errorX "rom: initial value undefined") :- s

    go (GatedClock _ _ en) =
      go' (withFrozenCallStack (errorX "rom: initial value undefined")) en

    go' o (e :- es) as@(~(x :- xs)) =
      -- See [Note: register strictness annotations]
      o `seqX` o :- (as `seq` if e then go' x es xs else go' o es xs)
{-# NOINLINE rom# #-}
