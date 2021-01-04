{-|
Copyright  :  (C) 2015-2016, University of Twente,
                  2017     , Google Inc.
                  2019     , Myrtle Software Ltd.
License    :  BSD2 (see the file LICENSE)
Maintainer :  Christiaan Baaij <christiaan.baaij@gmail.com>

= Initializing a ROM with a data file #usingromfiles#

ROMs initialized with a data file. The BNF grammar for this data file is simple:

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
f
  :: Clock  dom
  -> Signal dom (Unsigned 3)
  -> Signal dom (Unsigned 9)
f clk rd = 'Clash.Class.BitPack.unpack' '<$>' 'romFile' clk d7 \"memory.bin\" rd
@

And see that it works as expected:

@
__>>> import qualified Data.List as L__
__>>> L.tail $ sampleN 4 $ f systemClockGen (fromList [3..5])__
[10,11,12]
@

However, we can also interpret the same data as a tuple of a 6-bit unsigned
number, and a 3-bit signed number:

@
g
  :: Clock  dom Regular
  -> Signal dom (Unsigned 3)
  -> Signal dom (Unsigned 6,Signed 3)
g clk rd = 'Clash.Class.BitPack.unpack' '<$>' 'romFile' clk d7 \"memory.bin\" rd
@

And then we would see:

@
__>>> import qualified Data.List as L__
__>>> L.tail $ sampleN 4 $ g systemClockGen (fromList [3..5])__
[(1,2),(1,3)(1,-4)]
@
-}

{-# LANGUAGE FlexibleContexts #-}

{-# LANGUAGE Unsafe #-}

{-# OPTIONS_HADDOCK show-extensions #-}

module Clash.Explicit.ROM.File
  ( -- * Synchronous ROM synchronized to an arbitrary clock
    romFile
  , romFilePow2
    -- * Internal
  , romFile#
  )
where

import Data.Array                   (listArray,(!))
import GHC.TypeLits                 (KnownNat)
import System.IO.Unsafe             (unsafePerformIO)
--
import Clash.Explicit.BlockRam.File (initMem)
import Clash.Promoted.Nat           (SNat (..), pow2SNat, snatToNum)
import Clash.Sized.BitVector        (BitVector)
import Clash.Explicit.Signal        (Clock, Enable, Signal, KnownDomain, delay)
import Clash.Sized.Unsigned         (Unsigned)
import Clash.XException             (NFDataX(deepErrorX))


-- | A ROM with a synchronous read port, with space for 2^@n@ elements
--
-- * __NB__: Read value is delayed by 1 cycle
-- * __NB__: Initial output value is 'undefined'
-- * __NB__: This function might not work for specific combinations of
-- code-generation backends and hardware targets. Please check the support table
-- below:
--
--     @
--                    | VHDL     | Verilog  | SystemVerilog |
--     ===============+==========+==========+===============+
--     Altera/Quartus | Broken   | Works    | Works         |
--     Xilinx/ISE     | Works    | Works    | Works         |
--     ASIC           | Untested | Untested | Untested      |
--     ===============+==========+==========+===============+
--     @
--
-- Additional helpful information:
--
-- * See "Clash.Explicit.ROM.File#usingromfiles" for more information on how
-- to instantiate a ROM with the contents of a data file.
-- * See "Clash.Sized.Fixed#creatingdatafiles" for ideas on how to create your
-- own data files.
romFilePow2
  :: forall dom  n m
   . (KnownNat m, KnownNat n, KnownDomain dom)
  => Clock dom
  -- ^ 'Clock' to synchronize to
  -> Enable dom
  -- ^ Global enable
  -> FilePath
  -- ^ File describing the content of
  -- the ROM
  -> Signal dom (Unsigned n)
  -- ^ Read address @rd@
  -> Signal dom (BitVector m)
  -- ^ The value of the ROM at address @rd@ from the previous clock cycle
romFilePow2 = \clk en -> romFile clk en (pow2SNat (SNat @n))
{-# INLINE romFilePow2 #-}

-- | A ROM with a synchronous read port, with space for @n@ elements
--
-- * __NB__: Read value is delayed by 1 cycle
-- * __NB__: Initial output value is 'undefined'
-- * __NB__: This function might not work for specific combinations of
-- code-generation backends and hardware targets. Please check the support table
-- below:
--
--     @
--                    | VHDL     | Verilog  | SystemVerilog |
--     ===============+==========+==========+===============+
--     Altera/Quartus | Broken   | Works    | Works         |
--     Xilinx/ISE     | Works    | Works    | Works         |
--     ASIC           | Untested | Untested | Untested      |
--     ===============+==========+==========+===============+
--     @
--
-- Additional helpful information:
--
-- * See "Clash.Explicit.ROM.File#usingromfiles" for more information on how
-- to instantiate a ROM with the contents of a data file.
-- * See "Clash.Sized.Fixed#creatingdatafiles" for ideas on how to create your
-- own data files.
romFile
  :: (KnownNat m, Enum addr, KnownDomain dom)
  => Clock dom
  -- ^ 'Clock' to synchronize to
  -> Enable dom
  -- ^ Global enable
  -> SNat n
  -- ^ Size of the ROM
  -> FilePath
  -- ^ File describing the content of the ROM
  -> Signal dom addr
  -- ^ Read address @rd@
  -> Signal dom (BitVector m)
  -- ^ The value of the ROM at address @rd@ from the previous clock cycle
romFile = \clk en sz file rd -> romFile# clk en sz file (fromEnum <$> rd)
{-# INLINE romFile #-}

-- | romFile primitive
romFile#
  :: (KnownNat m, KnownDomain dom)
  => Clock dom
  -- ^ 'Clock' to synchronize to
  -> Enable dom
  -- ^ Global enable
  -> SNat n
  -- ^ Size of the ROM
  -> FilePath
  -- ^ File describing the content of the ROM
  -> Signal dom Int
  -- ^ Read address @rd@
  -> Signal dom (BitVector m)
  -- ^ The value of the ROM at address @rd@ from the previous clock cycle
romFile# clk en sz file rd =
  delay clk en (deepErrorX "First value of romFile is undefined") ((content !) <$> rd)
 where
  mem     = unsafePerformIO (initMem file)
  content = listArray (0,szI-1) mem
  szI     = snatToNum sz
{-# NOINLINE romFile# #-}
