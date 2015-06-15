{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE MagicHash           #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}

{-# LANGUAGE Unsafe #-}

{-|
Copyright  :  (C) 2015, University of Twente
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
    -- * Internal
  , asyncROMFile#
  , romFile#
  )
where

import Data.Array                  (listArray,(!))
import GHC.TypeLits                (KnownNat, type (^))

import CLaSH.Prelude.BlockRam.File (initMem)
import CLaSH.Promoted.Nat          (SNat,snat,snatToInteger)
import CLaSH.Sized.BitVector       (BitVector)
import CLaSH.Signal                (Signal)
import CLaSH.Signal.Explicit       (Signal', SClock, register', systemClock)
import CLaSH.Sized.Unsigned        (Unsigned)

{-# INLINE asyncROMFile #-}
-- | An asynchronous/combinational ROM with space for @n@ elements
--
-- __NB__: This function might not work for specific combinations of
-- code-generation backends and hardware targets. Please check the support table
-- below:
--
-- @
--                | VHDL     | Verilog  | SystemVerilog |
-- ===============+==========+==========+===============+
-- Altera/Quartus | Broken   | Works    | Works         |
-- Xilinx/ISE     | Works    | Works    | Works         |
-- ASIC           | Untested | Untested | Untested      |
-- ===============+==========+==========+===============+
-- @
asyncROMFile :: (KnownNat m, Enum addr)
             => SNat n      -- ^ Size of the ROM
             -> FilePath    -- ^ File describing the content of the ROM
             -> addr        -- ^ Read address @rd@
             -> BitVector m -- ^ The value of the ROM at address @rd@
asyncROMFile sz file rd = asyncROMFile# sz file (fromEnum rd)

{-# INLINE asyncROMFilePow2 #-}
-- | An asynchronous/combinational ROM with space for 2^@n@ elements
--
-- __NB__: This function might not work for specific combinations of
-- code-generation backends and hardware targets. Please check the support table
-- below:
--
-- @
--                | VHDL     | Verilog  | SystemVerilog |
-- ===============+==========+==========+===============+
-- Altera/Quartus | Broken   | Works    | Works         |
-- Xilinx/ISE     | Works    | Works    | Works         |
-- ASIC           | Untested | Untested | Untested      |
-- ===============+==========+==========+===============+
-- @
asyncROMFilePow2 :: forall n m . (KnownNat m, KnownNat n, KnownNat (2^n))
                 => FilePath    -- ^ File describing the content of the ROM
                 -> Unsigned n  -- ^ Read address @rd@
                 -> BitVector m -- ^ The value of the ROM at address @rd@
asyncROMFilePow2 = asyncROMFile (snat :: SNat (2^n))

{-# NOINLINE asyncROMFile# #-}
-- | asyncROMFile primitive
asyncROMFile# :: KnownNat m
              => SNat n       -- ^ Size of the ROM
              -> FilePath     -- ^ File describing the content of the ROM
              -> Int          -- ^ Read address @rd@
              -> BitVector m  -- ^ The value of the ROM at address @rd@
asyncROMFile# sz file rd = content ! rd
  where
    content = listArray (0,szI-1) (initMem file)
    szI     = fromInteger (snatToInteger sz)

{-# INLINE romFile #-}
-- | A ROM with a synchronous read port, with space for @n@ elements
--
-- * __NB__: Read value is delayed by 1 cycle
-- * __NB__: Initial output value is 'undefined'
-- * __NB__: This function might not work for specific combinations of
-- code-generation backends and hardware targets. Please check the support table
-- below:
--
-- @
--                | VHDL     | Verilog  | SystemVerilog |
-- ===============+==========+==========+===============+
-- Altera/Quartus | Broken   | Works    | Works         |
-- Xilinx/ISE     | Works    | Works    | Works         |
-- ASIC           | Untested | Untested | Untested      |
-- ===============+==========+==========+===============+
-- @
romFile :: (KnownNat m, KnownNat k)
        => SNat n               -- ^ Size of the ROM
        -> FilePath             -- ^ File describing the content of the ROM
        -> Signal (Unsigned k)  -- ^ Read address @rd@
        -> Signal (BitVector m)
        -- ^ The value of the ROM at address @rd@ from the previous clock cycle
romFile = romFile' systemClock

{-# INLINE romFilePow2 #-}
-- | A ROM with a synchronous read port, with space for 2^@n@ elements
--
-- * __NB__: Read value is delayed by 1 cycle
-- * __NB__: Initial output value is 'undefined'
-- * __NB__: This function might not work for specific combinations of
-- code-generation backends and hardware targets. Please check the support table
-- below:
--
-- @
--                | VHDL     | Verilog  | SystemVerilog |
-- ===============+==========+==========+===============+
-- Altera/Quartus | Broken   | Works    | Works         |
-- Xilinx/ISE     | Works    | Works    | Works         |
-- ASIC           | Untested | Untested | Untested      |
-- ===============+==========+==========+===============+
-- @
romFilePow2 :: forall n m . (KnownNat m, KnownNat n, KnownNat (2^n))
            => FilePath             -- ^ File describing the content of the ROM
            -> Signal (Unsigned n)  -- ^ Read address @rd@
            -> Signal (BitVector m)
            -- ^ The value of the ROM at address @rd@ from the previous clock cycle
romFilePow2 = romFile' systemClock (snat :: SNat (2^n))

{-# INLINE romFilePow2' #-}
-- | A ROM with a synchronous read port, with space for 2^@n@ elements
--
-- * __NB__: Read value is delayed by 1 cycle
-- * __NB__: Initial output value is 'undefined'
-- * __NB__: This function might not work for specific combinations of
-- code-generation backends and hardware targets. Please check the support table
-- below:
--
-- @
--                | VHDL     | Verilog  | SystemVerilog |
-- ===============+==========+==========+===============+
-- Altera/Quartus | Broken   | Works    | Works         |
-- Xilinx/ISE     | Works    | Works    | Works         |
-- ASIC           | Untested | Untested | Untested      |
-- ===============+==========+==========+===============+
-- @
romFilePow2' :: forall clk n m . (KnownNat m, KnownNat n, KnownNat (2^n))
             => SClock clk                -- ^ 'Clock' to synchronize to
             -> FilePath                  -- ^ File describing the content of
                                          -- the ROM
             -> Signal' clk (Unsigned n)  -- ^ Read address @rd@
             -> Signal' clk (BitVector m)
             -- ^ The value of the ROM at address @rd@ from the previous clock
             -- cycle
romFilePow2' clk = romFile' clk (snat :: SNat (2^n))

{-# INLINE romFile' #-}
-- | A ROM with a synchronous read port, with space for @n@ elements
--
-- * __NB__: Read value is delayed by 1 cycle
-- * __NB__: Initial output value is 'undefined'
-- * __NB__: This function might not work for specific combinations of
-- code-generation backends and hardware targets. Please check the support table
-- below:
--
-- @
--                | VHDL     | Verilog  | SystemVerilog |
-- ===============+==========+==========+===============+
-- Altera/Quartus | Broken   | Works    | Works         |
-- Xilinx/ISE     | Works    | Works    | Works         |
-- ASIC           | Untested | Untested | Untested      |
-- ===============+==========+==========+===============+
-- @
romFile' :: (KnownNat m, Enum addr)
         => SClock clk                -- ^ 'Clock' to synchronize to
         -> SNat n                    -- ^ Size of the ROM
         -> FilePath                  -- ^ File describing the content of the
                                      -- ROM
         -> Signal' clk addr          -- ^ Read address @rd@
         -> Signal' clk (BitVector m)
         -- ^ The value of the ROM at address @rd@ from the previous clock cycle
romFile' clk sz file rd = romFile# clk sz file (fromEnum <$> rd)

{-# NOINLINE romFile# #-}
-- | romFile primitive
romFile# :: KnownNat m
         => SClock clk                -- ^ 'Clock' to synchronize to
         -> SNat n                    -- ^ Size of the ROM
         -> FilePath                  -- ^ File describing the content of the
                                      -- ROM
         -> Signal' clk Int           -- ^ Read address @rd@
         -> Signal' clk (BitVector m)
         -- ^ The value of the ROM at address @rd@ from the previous clock cycle
romFile# clk sz file rd = register' clk undefined ((content !) <$> rd)
  where
    content = listArray (0,szI-1) (initMem file)
    szI     = fromInteger (snatToInteger sz)
