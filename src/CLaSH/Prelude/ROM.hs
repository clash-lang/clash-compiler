{-|
Copyright  :  (C) 2015-2016, University of Twente
License    :  BSD2 (see the file LICENSE)
Maintainer :  Christiaan Baaij <christiaan.baaij@gmail.com>

ROMs
-}

{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE MagicHash     #-}
{-# LANGUAGE TypeOperators #-}

{-# LANGUAGE Safe #-}

{-# OPTIONS_GHC -fplugin GHC.TypeLits.KnownNat.Solver #-}
{-# OPTIONS_HADDOCK show-extensions #-}

module CLaSH.Prelude.ROM
  ( -- * Asynchronous ROM
    asyncRom
  , asyncRomPow2
    -- * Synchronous ROM synchronised to the system clock
  , rom
  , romPow2
    -- * Synchronous ROM synchronised to an arbitrary clock
  , rom'
  , romPow2'
    -- * Internal
  , asyncRom#
  , rom#
  )
where

import Data.Array             ((!),listArray)
import GHC.TypeLits           (KnownNat, type (^))
import Prelude hiding         (length)

import CLaSH.Signal           (Signal)
import CLaSH.Signal.Explicit  (Signal', SClock, systemClock)
import CLaSH.Sized.Unsigned   (Unsigned)
import CLaSH.Signal.Explicit  (register')
import CLaSH.Sized.Vector     (Vec, length, toList)

{-# INLINE asyncRom #-}
-- | An asynchronous/combinational ROM with space for @n@ elements
--
-- Additional helpful information:
--
-- * See "CLaSH.Sized.Fixed#creatingdatafiles" and "CLaSH.Prelude.BlockRam#usingrams"
-- for ideas on how to use ROMs and RAMs
asyncRom :: (KnownNat n, Enum addr)
         => Vec n a -- ^ ROM content
                    --
                    -- __NB:__ must be a constant
         -> addr    -- ^ Read address @rd@
         -> a       -- ^ The value of the ROM at address @rd@
asyncRom content rd = asyncRom# content (fromEnum rd)

{-# INLINE asyncRomPow2 #-}
-- | An asynchronous/combinational ROM with space for 2^@n@ elements
--
-- Additional helpful information:
--
-- * See "CLaSH.Sized.Fixed#creatingdatafiles" and "CLaSH.Prelude.BlockRam#usingrams"
-- for ideas on how to use ROMs and RAMs
asyncRomPow2 :: KnownNat n
             => Vec (2^n) a -- ^ ROM content
                            --
                            -- __NB:__ must be a constant
             -> Unsigned n  -- ^ Read address @rd@
             -> a           -- ^ The value of the ROM at address @rd@
asyncRomPow2 = asyncRom

{-# NOINLINE asyncRom# #-}
-- | asyncROM primitive
asyncRom# :: KnownNat n
          => Vec n a  -- ^ ROM content
                      --
                      -- __NB:__ must be a constant
          -> Int      -- ^ Read address @rd@
          -> a        -- ^ The value of the ROM at address @rd@
asyncRom# content rd = arr ! rd
  where
    szI = length content
    arr = listArray (0,szI-1) (toList content)

{-# INLINE rom #-}
-- | A ROM with a synchronous read port, with space for @n@ elements
--
-- * __NB__: Read value is delayed by 1 cycle
-- * __NB__: Initial output value is 'undefined'
--
-- Additional helpful information:
--
-- * See "CLaSH.Sized.Fixed#creatingdatafiles" and "CLaSH.Prelude.BlockRam#usingrams"
-- for ideas on how to use ROMs and RAMs
rom :: (KnownNat n, KnownNat m)
    => Vec n a               -- ^ ROM content
                             --
                             -- __NB:__ must be a constant
    -> Signal (Unsigned m)   -- ^ Read address @rd@
    -> Signal a              -- ^ The value of the ROM at address @rd@
rom = rom' systemClock

{-# INLINE romPow2 #-}
-- | A ROM with a synchronous read port, with space for 2^@n@ elements
--
-- * __NB__: Read value is delayed by 1 cycle
-- * __NB__: Initial output value is 'undefined'
--
-- Additional helpful information:
--
-- * See "CLaSH.Sized.Fixed#creatingdatafiles" and "CLaSH.Prelude.BlockRam#usingrams"
-- for ideas on how to use ROMs and RAMs
romPow2 :: KnownNat n
        => Vec (2^n) a         -- ^ ROM content
                               --
                               -- __NB:__ must be a constant
        -> Signal (Unsigned n) -- ^ Read address @rd@
        -> Signal a            -- ^ The value of the ROM at address @rd@
romPow2 = romPow2' systemClock

{-# INLINE romPow2' #-}
-- | A ROM with a synchronous read port, with space for 2^@n@ elements
--
-- * __NB__: Read value is delayed by 1 cycle
-- * __NB__: Initial output value is 'undefined'
--
-- Additional helpful information:
--
-- * See "CLaSH.Sized.Fixed#creatingdatafiles" and "CLaSH.Prelude.BlockRam#usingrams"
-- for ideas on how to use ROMs and RAMs
romPow2' :: KnownNat n
         => SClock clk               -- ^ 'Clock' to synchronize to
         -> Vec (2^n) a              -- ^ ROM content
                                     --
                                     -- __NB:__ must be a constant
         -> Signal' clk (Unsigned n) -- ^ Read address @rd@
         -> Signal' clk a            -- ^ The value of the ROM at address @rd@
romPow2' = rom'

{-# INLINE rom' #-}
-- | A ROM with a synchronous read port, with space for @n@ elements
--
-- * __NB__: Read value is delayed by 1 cycle
-- * __NB__: Initial output value is 'undefined'
--
-- Additional helpful information:
--
-- * See "CLaSH.Sized.Fixed#creatingdatafiles" and "CLaSH.Prelude.BlockRam#usingrams"
-- for ideas on how to use ROMs and RAMs
rom' :: (KnownNat n, Enum addr)
     => SClock clk       -- ^ 'Clock' to synchronize to
     -> Vec n a          -- ^ ROM content
                         --
                         -- __NB:__ must be a constant
     -> Signal' clk addr -- ^ Read address @rd@
     -> Signal' clk a
     -- ^ The value of the ROM at address @rd@ from the previous clock cycle
rom' clk content rd = rom# clk content (fromEnum <$> rd)

{-# NOINLINE rom# #-}
-- | ROM primitive
rom# :: KnownNat n
     => SClock clk      -- ^ 'Clock' to synchronize to
     -> Vec n a         -- ^ ROM content
                        --
                        -- __NB:__ must be a constant
     -> Signal' clk Int -- ^ Read address @rd@
     -> Signal' clk a
     -- ^ The value of the ROM at address @rd@ from the previous clock cycle
rom# clk content rd = register' clk undefined ((arr !) <$> rd)
  where
    szI = length content
    arr = listArray (0,szI-1) (toList content)
