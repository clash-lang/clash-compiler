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

ROMs initialised with a data file. The BNF grammar for this data file is simple:

@
FILE = LINE+
LINE = BIT+
BIT  = '0'
     | '1'
@

Consecutive @LINE@s correspond to consecutive memory addresses starting at @0@.
For example, a data file @memory.bin@ containing the 9-bit unsigned number
@7@ to @13@ looks like:

@
000000111
000001000
000001001
000001010
000001011
000001100
000001101
@

We can instantiate a synchronous ROM using the content of the above file like
so:

@
topEntity :: Signal (Unsigned 3) -> Signal (Unsigned 9)
topEntity rd = 'CLaSH.Class.BitPack.unpack' '<$>' 'romFile' d7 \"memory.bin\" rd
@

And see that it works as expected:

@
__>>> import qualified Data.List as L__
__>>> L.tail $ sampleN 4 $ topEntity (fromList [3..5])__
[10,11,12]
@

However, we can also interpret the same data as a tuple of a 6-bit unsigned
number, and a 3-bit signed number:

@
topEntity2 :: Signal (Unsigned 3) -> Signal (Unsigned 6,Signed 3)
topEntity2 rd = 'CLaSH.Class.BitPack.unpack' '<$>' 'romFile' d7 \"memory.bin\" rd
@

And then we would see:

@
__>>> import qualified Data.List as L__
__>>> L.tail $ sampleN 4 $ topEntity2 (fromList [3..5])__
[(1,2),(1,3)(1,-4)]
@
-}
module CLaSH.Prelude.ROM.File
  ( -- * Asynchronous ROM
    asyncRomFile
  , asyncRomFilePow2
    -- * Synchronous ROM synchronised to the system clock
  , romFile
  , romFilePow2
    -- * Synchronous ROM synchronised to an arbitrary clock
  , romFile'
  , romFilePow2'
    -- * Internal
  , asyncRomFile#
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

{-# INLINE asyncRomFile #-}
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
asyncRomFile :: (KnownNat m, Enum addr)
             => SNat n      -- ^ Size of the ROM
             -> FilePath    -- ^ File describing the content of the ROM
             -> addr        -- ^ Read address @rd@
             -> BitVector m -- ^ The value of the ROM at address @rd@
asyncRomFile sz file rd = asyncRomFile# sz file (fromEnum rd)

{-# INLINE asyncRomFilePow2 #-}
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
asyncRomFilePow2 :: forall n m . (KnownNat m, KnownNat n, KnownNat (2^n))
                 => FilePath    -- ^ File describing the content of the ROM
                 -> Unsigned n  -- ^ Read address @rd@
                 -> BitVector m -- ^ The value of the ROM at address @rd@
asyncRomFilePow2 = asyncRomFile (snat :: SNat (2^n))

{-# NOINLINE asyncRomFile# #-}
-- | asyncROMFile primitive
asyncRomFile# :: KnownNat m
              => SNat n       -- ^ Size of the ROM
              -> FilePath     -- ^ File describing the content of the ROM
              -> Int          -- ^ Read address @rd@
              -> BitVector m  -- ^ The value of the ROM at address @rd@
asyncRomFile# sz file rd = content ! rd
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
