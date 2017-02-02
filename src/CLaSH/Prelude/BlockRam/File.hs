{-|
Copyright  :  (C) 2015-2016, University of Twente
License    :  BSD2 (see the file LICENSE)
Maintainer :  Christiaan Baaij <christiaan.baaij@gmail.com>

= Initialising a BlockRAM with a data file #usingramfiles#

BlockRAM primitives that can be initialised with a data file. The BNF grammar
for this data file is simple:

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

We can instantiate a BlockRAM using the content of the above file like so:

@
topEntity :: Signal (Unsigned 3) -> Signal (Unsigned 9)
topEntity rd = 'CLaSH.Class.BitPack.unpack' '<$>' 'blockRamFile' d7 \"memory.bin\" rd (signal Nothing)
@

In the example above, we basically treat the BlockRAM as an synchronous ROM.
We can see that it works as expected:

@
__>>> import qualified Data.List as L__
__>>> L.tail $ sampleN 4 $ topEntity (fromList [3..5])__
[10,11,12]
@

However, we can also interpret the same data as a tuple of a 6-bit unsigned
number, and a 3-bit signed number:

@
topEntity2 :: Signal (Unsigned 3) -> Signal (Unsigned 6,Signed 3)
topEntity2 rd = 'CLaSH.Class.BitPack.unpack' '<$>' 'blockRamFile' d7 \"memory.bin\" rd (signal Nothing)
@

And then we would see:

@
__>>> import qualified Data.List as L__
__>>> L.tail $ sampleN 4 $ topEntity2 (fromList [3..5])__
[(1,2),(1,3)(1,-4)]
@

-}

{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE MagicHash           #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeOperators       #-}

{-# LANGUAGE Unsafe #-}

{-# OPTIONS_GHC -fplugin GHC.TypeLits.KnownNat.Solver #-}
{-# OPTIONS_HADDOCK show-extensions #-}

-- See: https://github.com/clash-lang/clash-compiler/commit/721fcfa9198925661cd836668705f817bddaae3c
-- as to why we need this.
{-# OPTIONS_GHC -fno-cpr-anal #-}

module CLaSH.Prelude.BlockRam.File
  ( -- * BlockRAM synchronised to the system clock
    blockRamFile
  , blockRamFilePow2
    -- * BlockRAM synchronised to an arbitrary clock
  , blockRamFile'
  , blockRamFilePow2'
    -- * Internal
  , blockRamFile#
  , initMem
  )
where

import Data.Char                    (digitToInt)
import Data.Maybe                   (fromJust, isJust, listToMaybe)
import qualified Data.Vector        as V
import GHC.TypeLits                 (KnownNat)
import Numeric                      (readInt)
import System.IO.Unsafe             (unsafePerformIO)

import CLaSH.Promoted.Nat    (SNat (..), pow2SNat)
import CLaSH.Sized.BitVector (BitVector)
import CLaSH.Signal          (Signal)
import CLaSH.Signal.Explicit (SClock, systemClock)
import CLaSH.Signal.Internal (Signal' (..))
import CLaSH.Signal.Bundle   (unbundle)
import CLaSH.Sized.Unsigned  (Unsigned)
import CLaSH.XException      (errorX)

{-# INLINE blockRamFile #-}
-- | Create a blockRAM with space for @n@ elements
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
-- * See "CLaSH.Prelude.BlockRam#usingrams" for more information on how to use a
-- Block RAM.
-- * Use the adapter 'readNew' for obtaining write-before-read semantics like this: @readNew (blockRamFile size file) rd wrM@.
-- * See "CLaSH.Prelude.BlockRam.File#usingramfiles" for more information on how
-- to instantiate a Block RAM with the contents of a data file.
-- * See "CLaSH.Sized.Fixed#creatingdatafiles" for ideas on how to create your
-- own data files.
blockRamFile :: (KnownNat m, Enum addr)
             => SNat n               -- ^ Size of the blockRAM
             -> FilePath             -- ^ File describing the initial content
                                     -- of the blockRAM
             -> Signal addr          -- ^ Read address @r@
             -> Signal (Maybe (addr, BitVector m))
             -- ^ (write address @w@, value to write)
             -> Signal (BitVector m)
             -- ^ Value of the @blockRAM@ at address @r@ from the previous clock
             -- cycle
blockRamFile = blockRamFile' systemClock

{-# INLINE blockRamFilePow2 #-}
-- | Create a blockRAM with space for 2^@n@ elements
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
-- * See "CLaSH.Prelude.BlockRam#usingrams" for more information on how to use a
-- Block RAM.
-- * Use the adapter 'readNew' for obtaining write-before-read semantics like this: @readNew (blockRamFilePow2 file) rd wrM@.
-- * See "CLaSH.Prelude.BlockRam.File#usingramfiles" for more information on how
-- to instantiate a Block RAM with the contents of a data file.
-- * See "CLaSH.Sized.Fixed#creatingdatafiles" for ideas on how to create your
-- own data files.
blockRamFilePow2 :: (KnownNat m, KnownNat n)
                 => FilePath             -- ^ File describing the initial
                                         -- content of the blockRAM
                 -> Signal (Unsigned n) -- ^ Read address @r@
                 -> Signal (Maybe (Unsigned n, BitVector m))
                 -- ^ (write address @w@, value to write)@)
                 -> Signal (BitVector m)
                 -- ^ Value of the @blockRAM@ at address @r@ from the previous
                 -- clock cycle
blockRamFilePow2 = blockRamFilePow2' systemClock

{-# INLINE blockRamFilePow2' #-}
-- | Create a blockRAM with space for 2^@n@ elements
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
-- * See "CLaSH.Prelude.BlockRam#usingrams" for more information on how to use a
-- Block RAM.
-- * Use the adapter 'readNew'' for obtaining write-before-read semantics like this: @readNew' clk (blockRamFilePow2' clk file) rd wrM@.
-- * See "CLaSH.Prelude.BlockRam.File#usingramfiles" for more information on how
-- to instantiate a Block RAM with the contents of a data file.
-- * See "CLaSH.Sized.Fixed#creatingdatafiles" for ideas on how to create your
-- own data files.
blockRamFilePow2' :: forall clk n m . (KnownNat m, KnownNat n)
                  => SClock clk                -- ^ 'Clock' to synchronize to
                  -> FilePath                  -- ^ File describing the initial
                                               -- content of the blockRAM
                  -> Signal' clk (Unsigned n)  -- ^ Read address @r@
                  -> Signal' clk (Maybe (Unsigned n, BitVector m))
                  -- ^ (write address @w@, value to write)
                  -> Signal' clk (BitVector m)
                  -- ^ Value of the @blockRAM@ at address @r@ from the previous
                  -- clock cycle
blockRamFilePow2' clk = blockRamFile' clk (pow2SNat (SNat @ n))

{-# INLINE blockRamFile' #-}
-- | Create a blockRAM with space for @n@ elements
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
-- * See "CLaSH.Prelude.BlockRam#usingrams" for more information on how to use a
-- Block RAM.
-- * Use the adapter 'readNew'' for obtaining write-before-read semantics like this: @readNew' clk (blockRamFile' clk size file) rd wrM@.
-- * See "CLaSH.Prelude.BlockRam.File#usingramfiles" for more information on how
-- to instantiate a Block RAM with the contents of a data file.
-- * See "CLaSH.Sized.Fixed#creatingdatafiles" for ideas on how to create your
-- own data files.
blockRamFile' :: (KnownNat m, Enum addr)
              => SClock clk                -- ^ 'Clock' to synchronize to
              -> SNat n                    -- ^ Size of the blockRAM
              -> FilePath                  -- ^ File describing the initial
                                           -- content of the blockRAM
              -> Signal' clk addr          -- ^ Read address @r@
              -> Signal' clk (Maybe (addr, BitVector m))
              -- ^ (write address @w@, value to write)
              -> Signal' clk (BitVector m)
              -- ^ Value of the @blockRAM@ at address @r@ from the previous
              -- clock cycle
blockRamFile' clk sz file rd wrM =
  let en       = isJust <$> wrM
      (wr,din) = unbundle (fromJust <$> wrM)
  in  blockRamFile# clk sz file (fromEnum <$> rd) en (fromEnum <$> wr) din

-- | blockRamFile primitive
blockRamFile# :: KnownNat m
              => SClock clk                -- ^ 'Clock' to synchronize to
              -> SNat n                    -- ^ Size of the blockRAM
              -> FilePath                  -- ^ File describing the initial
                                           -- content of the blockRAM
              -> Signal' clk Int           -- ^ Read address @r@
              -> Signal' clk Bool          -- ^ Write enable
              -> Signal' clk Int           -- ^ Write address @w@
              -> Signal' clk (BitVector m) -- ^ Value to write (at address @w@)
              -> Signal' clk (BitVector m)
              -- ^ Value of the @blockRAM@ at address @r@ from the previous
              -- clock cycle
blockRamFile# _clk _sz file =
    go ramI (errorX "blockRamFile#: intial value undefined")
  where
    go !ram o (r :- rs) (e :- en) (w :- wr) (d :- din) =
      let ram' = upd ram e w d
          o'   = ram V.! r
      in  o :- go ram' o' rs en wr din

    upd ram True  addr d = ram V.// [(addr,d)]
    upd ram False _    _ = ram

    content = unsafePerformIO (initMem file)
    ramI    = V.fromList content
{-# NOINLINE blockRamFile# #-}

{-# NOINLINE initMem #-}
-- | __NB:__ Not synthesisable
initMem :: KnownNat n => FilePath -> IO [BitVector n]
initMem = fmap (map parseBV . lines) . readFile
  where
    parseBV s = case parseBV' s of
                  Just i  -> fromInteger i
                  Nothing -> error ("Failed to parse: " ++ s)
    parseBV' = fmap fst . listToMaybe . readInt 2 (`elem` "01") digitToInt
