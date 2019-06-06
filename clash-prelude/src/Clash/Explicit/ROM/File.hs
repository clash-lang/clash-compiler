{-|
Copyright  :  (C) 2015-2016, University of Twente,
                  2017     , Google Inc.
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
  :: Clock  domain gated
  -> Signal domain (Unsigned 3)
  -> Signal domain (Unsigned 9)
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
  :: Clock  domain Source
  -> Signal domain (Unsigned 3)
  -> Signal domain (Unsigned 6,Signed 3)
g clk rd = 'Clash.Class.BitPack.unpack' '<$>' 'romFile' clk d7 \"memory.bin\" rd
@

And then we would see:

@
__>>> import qualified Data.List as L__
__>>> L.tail $ sampleN 4 $ g systemClockGen (fromList [3..5])__
[(1,2),(1,3)(1,-4)]
@
-}

{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE MagicHash           #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeOperators       #-}

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
import Clash.Explicit.Signal        (Clock, Signal, delay)
import Clash.Sized.Unsigned         (Unsigned)
import Clash.XException             (Undefined(deepErrorX))


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
  :: forall domain gated n m
   . (KnownNat m, KnownNat n)
  => Clock domain gated        -- ^ 'Clock' to synchronize to
  -> FilePath                  -- ^ File describing the content of
                               -- the ROM
  -> Signal domain (Unsigned n)  -- ^ Read address @rd@
  -> Signal domain (BitVector m)
  -- ^ The value of the ROM at address @rd@ from the previous clock cycle
romFilePow2 = \clk -> romFile clk (pow2SNat (SNat @ n))
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
  :: (KnownNat m, Enum addr)
  => Clock domain gated
  -- ^ 'Clock' to synchronize to
  -> SNat n
  -- ^ Size of the ROM
  -> FilePath
  -- ^ File describing the content of the ROM
  -> Signal domain addr
  -- ^ Read address @rd@
  -> Signal domain (BitVector m)
  -- ^ The value of the ROM at address @rd@ from the previous clock cycle
romFile = \clk sz file rd -> romFile# clk sz file (fromEnum <$> rd)
{-# INLINE romFile #-}

-- | romFile primitive
romFile#
  :: KnownNat m
  => Clock domain gated
  -- ^ 'Clock' to synchronize to
  -> SNat n
  -- ^ Size of the ROM
  -> FilePath
  -- ^ File describing the content of the ROM
  -> Signal domain Int
  -- ^ Read address @rd@
  -> Signal domain (BitVector m)
  -- ^ The value of the ROM at address @rd@ from the previous clock cycle
romFile# clk sz file rd =
  delay clk (deepErrorX "First value of romFile is undefined") ((content !) <$> rd)
 where
  mem     = unsafePerformIO (initMem file)
  content = listArray (0,szI-1) mem
  szI     = snatToNum sz
{-# NOINLINE romFile# #-}
