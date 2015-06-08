{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators    #-}

{-# LANGUAGE Safe #-}

{-|
Copyright  :  (C) 2013-2015, University of Twente
License    :  BSD2 (see the file LICENSE)
Maintainer :  Christiaan Baaij <christiaan.baaij@gmail.com>

ROMs
-}
module CLaSH.Prelude.ROM
  ( -- * Asynchronous ROM
    asyncROM
  , asyncROMPow2
    -- * Synchronous ROM synchronised to the system clock
  , rom
  , romPow2
    -- * Synchronous ROM synchronised to an arbitrary clock
  , rom'
  , romPow2'
  )
where

import GHC.TypeLits           (KnownNat, type (^))
import Prelude                hiding ((!!))

import CLaSH.Prelude.Moore    (moore')
import CLaSH.Signal           (Signal)
import CLaSH.Signal.Explicit  (Signal', SClock, systemClock)
import CLaSH.Sized.Unsigned   (Unsigned)
import CLaSH.Sized.Vector     (Vec, (!!))

-- | An asynchronous/combinational ROM with space for @n@ elements
asyncROM :: (KnownNat n, KnownNat m)
         => Vec n a    -- ^ ROM content
                       --
                       -- __NB:__ must be a constant
         -> Unsigned m -- ^ Read address @rd@
         -> a          -- ^ The value of the ROM at address @rd@
asyncROM = (!!)

-- | An asynchronous/combinational ROM with space for 2^@n@ elements
asyncROMPow2 :: (KnownNat (2^n), KnownNat n)
             => Vec (2^n) a -- ^ ROM content
                            --
                            -- __NB:__ must be a constant
             -> Unsigned n  -- ^ Read address @rd@
             -> a           -- ^ The value of the ROM at address @rd@
asyncROMPow2 = (!!)

{-# INLINE rom #-}
-- | A ROM with a synchronous read port, with space for @n@ elements
--
-- * __NB__: Read value is delayed by 1 cycle
-- * __NB__: Initial output value is 'undefined'
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
romPow2 :: (KnownNat (2^n), KnownNat n)
        => Vec (2^n) a         -- ^ ROM content
                               --
                               -- __NB:__ must be a constant
        -> Signal (Unsigned n) -- ^ Read address @rd@
        -> Signal a            -- ^ The value of the ROM at address @rd@
romPow2 = rom' systemClock

{-# INLINE romPow2' #-}
-- | A ROM with a synchronous read port, with space for 2^@n@ elements
--
-- * __NB__: Read value is delayed by 1 cycle
-- * __NB__: Initial output value is 'undefined'
romPow2' :: (KnownNat (2^n), KnownNat n)
         => SClock clk               -- ^ 'Clock' to synchronize to
         -> Vec (2^n) a              -- ^ ROM content
                                     --
                                     -- __NB:__ must be a constant
         -> Signal' clk (Unsigned n) -- ^ Read address @rd@
         -> Signal' clk a            -- ^ The value of the ROM at address @rd@
romPow2' = rom'

{-# NOINLINE rom' #-}
-- | A ROM with a synchronous read port, with space for @n@ elements
--
-- * __NB__: Read value is delayed by 1 cycle
-- * __NB__: Initial output value is 'undefined'
rom' :: (KnownNat n, KnownNat m)
     => SClock clk               -- ^ 'Clock' to synchronize to
     -> Vec n a                  -- ^ ROM content
                                 --
                                 -- __NB:__ must be a constant
     -> Signal' clk (Unsigned m) -- ^ Read address @rd@
     -> Signal' clk a            -- ^ The value of the ROM at address @rd@
rom' clk binit rd =
    moore' clk rom'' id undefined rd
  where
    rom'' _ r = binit !! r
