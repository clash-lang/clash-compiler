{-|
Copyright  :  (C) 2015-2016, University of Twente,
                  2017     , Google Inc.
                  2019     , Myrtle Software Ltd
License    :  BSD2 (see the file LICENSE)
Maintainer :  Christiaan Baaij <christiaan.baaij@gmail.com>

ROMs
-}

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}

{-# LANGUAGE Safe #-}

{-# OPTIONS_GHC -fplugin GHC.TypeLits.KnownNat.Solver #-}
{-# OPTIONS_HADDOCK show-extensions #-}

module Clash.Prelude.ROM
  ( -- * Asynchronous ROM
    asyncRom
  , asyncRomPow2
    -- * Synchronous ROM synchronized to an arbitrary clock
  , rom
  , romPow2
    -- * Internal
  , asyncRom#
  )
where

import           Data.Array           ((!),listArray)
import           GHC.TypeLits         (KnownNat, type (^))
import           Prelude              hiding (length)

import qualified Clash.Explicit.ROM   as E
import           Clash.Signal
import           Clash.Sized.Unsigned (Unsigned)
import           Clash.Sized.Vector   (Vec, length, toList)

import           Clash.XException     (NFDataX)

-- | An asynchronous/combinational ROM with space for @n@ elements
--
-- Additional helpful information:
--
-- * See "Clash.Sized.Fixed#creatingdatafiles" and "Clash.Prelude.BlockRam#usingrams"
-- for ideas on how to use ROMs and RAMs
asyncRom
  :: (KnownNat n, Enum addr)
  => Vec n a
  -- ^ ROM content
  --
  -- __NB:__ must be a constant
  -> addr
  -- ^ Read address @rd@
  -> a
  -- ^ The value of the ROM at address @rd@
asyncRom = \content rd -> asyncRom# content (fromEnum rd)
{-# INLINE asyncRom #-}

-- | An asynchronous/combinational ROM with space for 2^@n@ elements
--
-- Additional helpful information:
--
-- * See "Clash.Sized.Fixed#creatingdatafiles" and "Clash.Prelude.BlockRam#usingrams"
-- for ideas on how to use ROMs and RAMs
asyncRomPow2
  :: KnownNat n
  => Vec (2^n) a
  -- ^ ROM content
  --
  -- __NB:__ must be a constant
  -> Unsigned n
  -- ^ Read address @rd@
  -> a
  -- ^ The value of the ROM at address @rd@
asyncRomPow2 = asyncRom
{-# INLINE asyncRomPow2 #-}

-- | asyncROM primitive
asyncRom#
  :: KnownNat n
  => Vec n a
  -- ^ ROM content
  --
  -- __NB:__ must be a constant
  -> Int
  -- ^ Read address @rd@
  -> a
  -- ^ The value of the ROM at address @rd@
asyncRom# content rd = arr ! rd
  where
    szI = length content
    arr = listArray (0,szI-1) (toList content)
{-# NOINLINE asyncRom# #-}

-- | A ROM with a synchronous read port, with space for @n@ elements
--
-- * __NB__: Read value is delayed by 1 cycle
-- * __NB__: Initial output value is 'undefined'
--
-- Additional helpful information:
--
-- * See "Clash.Sized.Fixed#creatingdatafiles" and "Clash.Prelude.BlockRam#usingrams"
-- for ideas on how to use ROMs and RAMs
rom
  :: forall dom n m a
   . ( NFDataX a
     , KnownNat n
     , KnownNat m
     , HiddenClock dom
     , HiddenEnable dom  )
  => Vec n a
  -- ^ ROM content
  --
  -- __NB:__ must be a constant
  -> Signal dom (Unsigned m)
  -- ^ Read address @rd@
  -> Signal dom a
  -- ^ The value of the ROM at address @rd@
rom = hideEnable (hideClock E.rom)
{-# INLINE rom #-}

-- | A ROM with a synchronous read port, with space for 2^@n@ elements
--
-- * __NB__: Read value is delayed by 1 cycle
-- * __NB__: Initial output value is 'undefined'
--
-- Additional helpful information:
--
-- * See "Clash.Sized.Fixed#creatingdatafiles" and "Clash.Prelude.BlockRam#usingrams"
-- for ideas on how to use ROMs and RAMs
romPow2
  :: forall dom n a
   . ( KnownNat n
     , NFDataX a
     , HiddenClock dom
     , HiddenEnable dom  )
  => Vec (2^n) a
  -- ^ ROM content
  --
  -- __NB:__ must be a constant
  -> Signal dom (Unsigned n)
  -- ^ Read address @rd@
  -> Signal dom a
  -- ^ The value of the ROM at address @rd@
romPow2 = hideEnable (hideClock E.romPow2)
{-# INLINE romPow2 #-}
