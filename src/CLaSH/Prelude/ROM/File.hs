{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}

{-# LANGUAGE Unsafe #-}

{-|
Copyright  :  (C) 2013-2015, University of Twente
License    :  BSD2 (see the file LICENSE)
Maintainer :  Christiaan Baaij <christiaan.baaij@gmail.com>

ROMs initialised with a data file
-}
module CLaSH.Prelude.ROM.File
  ( -- * Asynchronous ROM
    asyncROMFile
  , asyncROMFilePow2
    -- * Synchronous ROM synchronised to the system clock
  , romFile
  , romFilePow2
    -- * Synchronous ROM synchronised to an arbitrary clock
  , romFile'
  , romFilePow2'
  )
where

import Data.Array                   (listArray,(!))
import GHC.TypeLits                 (KnownNat, type (^))

import CLaSH.Prelude.Moore          (moore')
import CLaSH.Prelude.BlockRam.File (initMem)
import CLaSH.Promoted.Nat          (SNat,snat,snatToInteger)
import CLaSH.Sized.BitVector       (BitVector)
import CLaSH.Signal                (Signal)
import CLaSH.Signal.Explicit       (Signal', SClock, systemClock)
import CLaSH.Sized.Unsigned        (Unsigned)

{-# NOINLINE asyncROMFile #-}
-- | An asynchronous/combinational ROM with space for @n@ elements
asyncROMFile :: (KnownNat k, KnownNat m)
             => SNat n      -- ^ Size of the ROM
             -> FilePath    -- ^ File describing the content of the ROM
             -> Unsigned k  -- ^ Read address @rd@
             -> BitVector m -- ^ The value of the ROM at address @rd@
asyncROMFile sz file rd = binit ! (toInteger rd)
  where
    binit = listArray (0,szI-1) bvs
    bvs   = initMem file
    szI   = snatToInteger sz

{-# INLINE asyncROMFilePow2 #-}
-- | An asynchronous/combinational ROM with space for 2^@n@ elements
asyncROMFilePow2 :: forall n m . (KnownNat m, KnownNat n, KnownNat (2^n))
                 => FilePath    -- ^ File describing the content of the ROM
                 -> Unsigned n  -- ^ Read address @rd@
                 -> BitVector m -- ^ The value of the ROM at address @rd@
asyncROMFilePow2 = asyncROMFile (snat :: SNat (2^n))

{-# INLINE romFile #-}
-- | A ROM with a synchronous read port, with space for @n@ elements
--
-- * __NB__: Read value is delayed by 1 cycle
-- * __NB__: Initial output value is 'undefined'
romFile :: (KnownNat m, KnownNat k)
        => SNat n               -- ^ Size of the ROM
        -> FilePath             -- ^ File describing the content of the ROM
        -> Signal (Unsigned k)  -- ^ Read address @rd@
        -> Signal (BitVector m) -- ^ The value of the ROM at address @rd@
romFile = romFile' systemClock

{-# INLINE romFilePow2 #-}
-- | A ROM with a synchronous read port, with space for 2^@n@ elements
--
-- * __NB__: Read value is delayed by 1 cycle
-- * __NB__: Initial output value is 'undefined'
romFilePow2 :: forall n m . (KnownNat m, KnownNat n, KnownNat (2^n))
            => FilePath             -- ^ File describing the content of the ROM
            -> Signal (Unsigned n)  -- ^ Read address @rd@
            -> Signal (BitVector m) -- ^ The value of the ROM at address @rd@
romFilePow2 = romFile' systemClock (snat :: SNat (2^n))

{-# INLINE romFilePow2' #-}
-- | A ROM with a synchronous read port, with space for 2^@n@ elements
--
-- * __NB__: Read value is delayed by 1 cycle
-- * __NB__: Initial output value is 'undefined'
romFilePow2' :: forall clk n m . (KnownNat m, KnownNat n, KnownNat (2^n))
             => SClock clk                -- ^ 'Clock' to synchronize to
             -> FilePath                  -- ^ File describing the content of the ROM
             -> Signal' clk (Unsigned n)  -- ^ Read address @rd@
             -> Signal' clk (BitVector m) -- ^ The value of the ROM at address @rd@
romFilePow2' clk = romFile' clk (snat :: SNat (2^n))

{-# NOINLINE romFile' #-}
-- | A ROM with a synchronous read port, with space for @n@ elements
--
-- * __NB__: Read value is delayed by 1 cycle
-- * __NB__: Initial output value is 'undefined'
romFile' :: (KnownNat m, KnownNat k)
         => SClock clk                -- ^ 'Clock' to synchronize to
         -> SNat n                    -- ^ Size of the ROM
         -> FilePath                  -- ^ File describing the content of the ROM
         -> Signal' clk (Unsigned k)  -- ^ Read address @rd@
         -> Signal' clk (BitVector m) -- ^ The value of the ROM at address @rd@
romFile' clk sz file rd =
    moore' clk rom'' id undefined rd
  where
    rom'' _ r = binit ! (toInteger r)

    binit = listArray (0,szI-1) bvs
    bvs   = initMem file
    szI   = snatToInteger sz
