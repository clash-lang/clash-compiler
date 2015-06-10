{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}

{-# LANGUAGE Unsafe #-}

{-|
Copyright  :  (C) 2013-2015, University of Twente
License    :  BSD2 (see the file LICENSE)
Maintainer :  Christiaan Baaij <christiaan.baaij@gmail.com>

BlockRAM primitives that can be initialised with a data file
-}
module CLaSH.Prelude.BlockRam.File
  ( -- * BlockRAM synchronised to the system clock
    blockRamFile
  , blockRamFilePow2
    -- * BlockRAM synchronised to an arbitrary clock
  , blockRamFile'
  , blockRamFilePow2'
    -- * Internal, and not synthesisable functions
  , initMem
  )
where


import Data.Array          (listArray,(!),(//))
import Data.Char           (digitToInt)
import Data.Maybe          (listToMaybe)
import GHC.TypeLits        (KnownNat, type (^))
import Numeric             (readInt)
import System.IO.Unsafe    (unsafePerformIO)

import CLaSH.Prelude.Moore   (moore')
import CLaSH.Promoted.Nat    (SNat,snat,snatToInteger)
import CLaSH.Sized.BitVector (BitVector)
import CLaSH.Signal          (Signal)
import CLaSH.Signal.Explicit (Signal', SClock, systemClock)
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
-- @
--                | VHDL     | Verilog  | SystemVerilog |
-- ===============+==========+==========+===============+
-- Altera/Quartus | Broken   | Works    | Works         |
-- Xilinx/ISE     | Works    | Works    | Works         |
-- ASIC           | Untested | Untested | Untested      |
-- ===============+==========+==========+===============+
-- @
blockRamFile :: (KnownNat m, KnownNat k)
             => SNat n               -- ^ Size of the blockRAM
             -> FilePath             -- ^ File describing the initial content
                                     -- of the blockRAM
             -> Signal (Unsigned k)  -- ^ Write address @w@
             -> Signal (Unsigned k)  -- ^ Read address @r@
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
-- @
--                | VHDL     | Verilog  | SystemVerilog |
-- ===============+==========+==========+===============+
-- Altera/Quartus | Broken   | Works    | Works         |
-- Xilinx/ISE     | Works    | Works    | Works         |
-- ASIC           | Untested | Untested | Untested      |
-- ===============+==========+==========+===============+
-- @
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
-- @
--                | VHDL     | Verilog  | SystemVerilog |
-- ===============+==========+==========+===============+
-- Altera/Quartus | Broken   | Works    | Works         |
-- Xilinx/ISE     | Works    | Works    | Works         |
-- ASIC           | Untested | Untested | Untested      |
-- ===============+==========+==========+===============+
-- @
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

{-# NOINLINE blockRamFile' #-}
-- | Create a blockRAM with space for @n@ elements
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
blockRamFile' :: (KnownNat m, KnownNat k)
              => SClock clk                -- ^ 'Clock' to synchronize to
              -> SNat n                    -- ^ Size of the blockRAM
              -> FilePath                  -- ^ File describing the initial
                                           -- content of the blockRAM
              -> Signal' clk (Unsigned k)  -- ^ Write address @w@
              -> Signal' clk (Unsigned k)  -- ^ Read address @r@
              -> Signal' clk Bool          -- ^ Write enable
              -> Signal' clk (BitVector m) -- ^ Value to write (at address @w@)
              -> Signal' clk (BitVector m)
              -- ^ Value of the @blockRAM@ at address @r@ from the previous
              -- clock cycle
blockRamFile' clk sz file wr rd en din =
    moore' clk bram' snd (binit,undefined) (bundle' clk (wr,rd,en,din))
  where
    bram' (ram,_) (w,r,e,d) = (ram',o')
      where
        ram' | e         = ram // [(toInteger w,d)]
             | otherwise = ram
        o'               = ram ! (toInteger r)

    binit = listArray (0,szI-1) bvs
    bvs   = initMem file
    szI   = snatToInteger sz

{-|
__NB:__ Not synthesisable
-}
initMem :: KnownNat n => FilePath -> [BitVector n]
initMem = unsafePerformIO . fmap (map parseBV . lines) . readFile
  where
    parseBV s = case parseBV' s of
                  Just i  -> fromInteger i
                  Nothing -> error ("Failed to parse: " ++ s)
    parseBV' = fmap fst . listToMaybe . readInt 2 (`elem` "01") digitToInt
