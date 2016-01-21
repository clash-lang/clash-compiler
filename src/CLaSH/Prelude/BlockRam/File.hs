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
topEntity rd = 'CLaSH.Class.BitPack.unpack' '<$>' 'blockRamFile' d7 \"memory.bin\" 0 rd (signal False) 0
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
topEntity2 rd = 'CLaSH.Class.BitPack.unpack' '<$>' 'blockRamFile' d7 \"memory.bin\" 0 rd (signal False) 0
@

And then we would see:

@
__>>> import qualified Data.List as L__
__>>> L.tail $ sampleN 4 $ topEntity2 (fromList [3..5])__
[(1,2),(1,3)(1,-4)]
@

-}

{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE MagicHash           #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}

{-# LANGUAGE Unsafe #-}

{-# OPTIONS_HADDOCK show-extensions #-}

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

import Control.Monad                (when)
import Control.Monad.ST.Lazy        (ST,runST)
import Control.Monad.ST.Lazy.Unsafe (unsafeIOToST)
import Data.Array.MArray            (newListArray,readArray,writeArray)
import Data.Array.ST                (STArray)
import Data.Char                    (digitToInt)
import Data.Maybe                   (listToMaybe)
import GHC.TypeLits                 (KnownNat, type (^))
import Numeric                      (readInt)

import CLaSH.Promoted.Nat    (SNat,snat,snatToInteger)
import CLaSH.Sized.BitVector (BitVector)
import CLaSH.Signal          (Signal)
import CLaSH.Signal.Explicit (Signal', SClock, register', systemClock)
import CLaSH.Signal.Bundle   (bundle')
import CLaSH.Sized.Unsigned  (Unsigned)

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
-- * Use the adapter 'readNew' for obtaining write-before-read semantics like this: @readNew (blockRamFile size file) wr rd en dt@.
-- * See "CLaSH.Prelude.BlockRam.File#usingramfiles" for more information on how
-- to instantiate a Block RAM with the contents of a data file.
-- * See "CLaSH.Sized.Fixed#creatingdatafiles" for ideas on how to create your
-- own data files.
blockRamFile :: (KnownNat m, Enum addr)
             => SNat n               -- ^ Size of the blockRAM
             -> FilePath             -- ^ File describing the initial content
                                     -- of the blockRAM
             -> Signal addr          -- ^ Write address @w@
             -> Signal addr          -- ^ Read address @r@
             -> Signal Bool          -- ^ Write enable
             -> Signal (BitVector m) -- ^ Value to write (at address @w@)
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
-- * Use the adapter 'readNew' for obtaining write-before-read semantics like this: @readNew (blockRamFilePow2 file) wr rd en dt@.
-- * See "CLaSH.Prelude.BlockRam.File#usingramfiles" for more information on how
-- to instantiate a Block RAM with the contents of a data file.
-- * See "CLaSH.Sized.Fixed#creatingdatafiles" for ideas on how to create your
-- own data files.
blockRamFilePow2 :: forall n m . (KnownNat m, KnownNat n, KnownNat (2^n))
                 => FilePath             -- ^ File describing the initial
                                         -- content of the blockRAM
                 -> Signal (Unsigned n)  -- ^ Write address @w@
                 -> Signal (Unsigned n)  -- ^ Read address @r@
                 -> Signal Bool          -- ^ Write enable
                 -> Signal (BitVector m) -- ^ Value to write (at address @w@)
                 -> Signal (BitVector m)
                 -- ^ Value of the @blockRAM@ at address @r@ from the previous
                 -- clock cycle
blockRamFilePow2 = blockRamFile' systemClock (snat :: SNat (2^n))

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
-- * Use the adapter 'readNew'' for obtaining write-before-read semantics like this: @readNew' clk (blockRamFilePow2' clk file) wr rd en dt@.
-- * See "CLaSH.Prelude.BlockRam.File#usingramfiles" for more information on how
-- to instantiate a Block RAM with the contents of a data file.
-- * See "CLaSH.Sized.Fixed#creatingdatafiles" for ideas on how to create your
-- own data files.
blockRamFilePow2' :: forall clk n m . (KnownNat m, KnownNat n, KnownNat (2^n))
                  => SClock clk                -- ^ 'Clock' to synchronize to
                  -> FilePath                  -- ^ File describing the initial
                                               -- content of the blockRAM
                  -> Signal' clk (Unsigned n)  -- ^ Write address @w@
                  -> Signal' clk (Unsigned n)  -- ^ Read address @r@
                  -> Signal' clk Bool          -- ^ Write enable
                  -> Signal' clk (BitVector m) -- ^ Value to write (at address @w@)
                  -> Signal' clk (BitVector m)
                  -- ^ Value of the @blockRAM@ at address @r@ from the previous
                  -- clock cycle
blockRamFilePow2' clk = blockRamFile' clk (snat :: SNat (2^n))

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
-- * Use the adapter 'readNew'' for obtaining write-before-read semantics like this: @readNew' clk (blockRamFile' clk size file) wr rd en dt@.
-- * See "CLaSH.Prelude.BlockRam.File#usingramfiles" for more information on how
-- to instantiate a Block RAM with the contents of a data file.
-- * See "CLaSH.Sized.Fixed#creatingdatafiles" for ideas on how to create your
-- own data files.
blockRamFile' :: (KnownNat m, Enum addr)
              => SClock clk                -- ^ 'Clock' to synchronize to
              -> SNat n                    -- ^ Size of the blockRAM
              -> FilePath                  -- ^ File describing the initial
                                           -- content of the blockRAM
              -> Signal' clk addr          -- ^ Write address @w@
              -> Signal' clk addr          -- ^ Read address @r@
              -> Signal' clk Bool          -- ^ Write enable
              -> Signal' clk (BitVector m) -- ^ Value to write (at address @w@)
              -> Signal' clk (BitVector m)
              -- ^ Value of the @blockRAM@ at address @r@ from the previous
              -- clock cycle
blockRamFile' clk sz file wr rd en din = blockRamFile# clk sz file
                                                       (fromEnum <$> wr)
                                                       (fromEnum <$> rd)
                                                       en din

{-# NOINLINE blockRamFile# #-}
-- | blockRamFile primitive
blockRamFile# :: KnownNat m
              => SClock clk                -- ^ 'Clock' to synchronize to
              -> SNat n                    -- ^ Size of the blockRAM
              -> FilePath                  -- ^ File describing the initial
                                           -- content of the blockRAM
              -> Signal' clk Int           -- ^ Write address @w@
              -> Signal' clk Int           -- ^ Read address @r@
              -> Signal' clk Bool          -- ^ Write enable
              -> Signal' clk (BitVector m) -- ^ Value to write (at address @w@)
              -> Signal' clk (BitVector m)
              -- ^ Value of the @blockRAM@ at address @r@ from the previous
              -- clock cycle
blockRamFile# clk sz file wr rd en din = register' clk undefined dout
  where
    szI  = fromInteger $ snatToInteger sz
    dout = runST $ do
      mem <- unsafeIOToST (initMem file)
      arr <- newListArray (0,szI-1) mem
      traverse (ramT arr) (bundle' clk (wr,rd,en,din))

    ramT :: STArray s Int e -> (Int,Int,Bool,e) -> ST s e
    ramT ram (w,r,e,d) = do
      d' <- readArray ram r
      when e (writeArray ram w d)
      return d'

{-# NOINLINE initMem #-}
-- | __NB:__ Not synthesisable
initMem :: KnownNat n => FilePath -> IO [BitVector n]
initMem = fmap (map parseBV . lines) . readFile
  where
    parseBV s = case parseBV' s of
                  Just i  -> fromInteger i
                  Nothing -> error ("Failed to parse: " ++ s)
    parseBV' = fmap fst . listToMaybe . readInt 2 (`elem` "01") digitToInt
