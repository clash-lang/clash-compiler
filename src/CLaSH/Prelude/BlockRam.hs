{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators    #-}
module CLaSH.Prelude.BlockRam where

import GHC.TypeLits           (KnownNat, type (^))
import Prelude                hiding ((!!))

import CLaSH.Prelude.Mealy    (mealy')
import CLaSH.Signal           (Signal)
import CLaSH.Signal.Explicit  (Signal', SClock, systemClock)
import CLaSH.Signal.Bundle    (bundle')
import CLaSH.Sized.Unsigned   (Unsigned)
import CLaSH.Sized.Vector     (Vec, (!!), replace)

{-# INLINE blockRam #-}
-- | Create a blockRAM with space for @n@ elements.
--
-- * __NB__: Read value is delayed by 1 cycle
-- * __NB__: Initial output value is 'undefined'
--
-- @
-- bram40 :: Signal (Unsigned 6) -> Signal (Unsigned 6) -> Signal Bool
--        -> Signal Bit -> Signal Bit
-- bram40 = 'blockRam' ('CLaSH.Sized.Vector.replicate' d40 1)
-- @
blockRam :: (KnownNat n, KnownNat m)
         => Vec n a             -- ^ Initial content of the BRAM, also
                                -- determines the size, @n@, of the BRAM.
                                --
                                -- __NB__: __MUST__ be a constant.
         -> Signal (Unsigned m) -- ^ Write address @w@
         -> Signal (Unsigned m) -- ^ Read address @r@
         -> Signal Bool         -- ^ Write enable
         -> Signal a            -- ^ Value to write (at address @w@)
         -> Signal a
         -- ^ Value of the @blockRAM@ at address @r@ from the previous clock
         -- cycle
blockRam = blockRam' systemClock

{-# INLINE blockRamPow2 #-}
-- | Create a blockRAM with space for 2^@n@ elements
--
-- * __NB__: Read value is delayed by 1 cycle
-- * __NB__: Initial output value is 'undefined'
--
-- @
-- bram32 :: Signal (Unsigned 5) -> Signal (Unsigned 5) -> Signal Bool
--        -> Signal Bit -> Signal Bit
-- bram32 = 'blockRamPow2' ('CLaSH.Sized.Vector.replicate' d32 1)
-- @
blockRamPow2 :: (KnownNat (2^n), KnownNat n)
             => Vec (2^n) a         -- ^ Initial content of the BRAM, also
                                    -- determines the size, @2^n@, of the BRAM.
                                    --
                                    -- __NB__: __MUST__ be a constant.
             -> Signal (Unsigned n) -- ^ Write address @w@
             -> Signal (Unsigned n) -- ^ Read address @r@
             -> Signal Bool         -- ^ Write enable
             -> Signal a            -- ^ Value to write (at address @w@)
             -> Signal a
             -- ^ Value of the @blockRAM@ at address @r@ from the previous clock
             -- cycle
blockRamPow2 = blockRam

{-# NOINLINE blockRam' #-}
-- | Create a blockRAM with space for @n@ elements
--
-- * __NB__: Read value is delayed by 1 cycle
-- * __NB__: Initial output value is 'undefined'
--
-- @
-- type ClkA = Clk \"A\" 100
--
-- clkA100 :: SClock ClkA
-- clkA100 = 'CLaSH.Signal.Explicit.sclock'
--
-- bram40 :: Signal' ClkA (Unsigned 6) -> Signal' ClkA (Unsigned 6)
--        -> Signal' ClkA Bool -> Signal' ClkA Bit -> ClkA Signal' Bit
-- bram40 = 'blockRam'' clkA100 ('CLaSH.Sized.Vector.replicate' d40 1)
-- @
blockRam' :: (KnownNat n, KnownNat m)
          => SClock clk               -- ^ 'Clock' to synchronize to
          -> Vec n a                  -- ^ Initial content of the BRAM, also
                                      -- determines the size, @n@, of the BRAM.
                                      --
                                      -- __NB__: __MUST__ be a constant.
          -> Signal' clk (Unsigned m) -- ^ Write address @w@
          -> Signal' clk (Unsigned m) -- ^ Read address @r@
          -> Signal' clk Bool         -- ^ Write enable
          -> Signal' clk a            -- ^ Value to write (at address @w@)
          -> Signal' clk a
          -- ^ Value of the @blockRAM@ at address @r@ from the previous clock
          -- cycle
blockRam' clk binit wr rd en din =
    mealy' clk bram' (binit,undefined) (bundle' clk (wr,rd,en,din))
  where
    bram' (ram,o) (w,r,e,d) = ((ram',o'),o)
      where
        ram' | e         = replace ram w d
             | otherwise = ram
        o'               = ram !! r

{-# INLINE blockRamPow2' #-}
-- | Create a blockRAM with space for 2^@n@ elements
--
-- * __NB__: Read value is delayed by 1 cycle
-- * __NB__: Initial output value is 'undefined'
--
-- @
-- type ClkA = Clk \"A\" 100
--
-- clkA100 :: SClock ClkA
-- clkA100 = 'CLaSH.Signal.Explicit.sclock'
--
-- bram32 :: Signal' ClkA (Unsigned 5) -> Signal' ClkA (Unsigned 5)
--        -> Signal' ClkA Bool -> Signal' ClkA Bit -> Signal' ClkA Bit
-- bram32 = 'blockRamPow2'' clkA100 ('CLaSH.Sized.Vector.replicate' d32 1)
-- @
blockRamPow2' :: (KnownNat n, KnownNat (2^n))
              => SClock clk               -- ^ 'Clock' to synchronize to
              -> Vec (2^n) a              -- ^ Initial content of the BRAM, also
                                          -- determines the size, @2^n@, of
                                          -- the BRAM.
                                          --
                                          -- __NB__: __MUST__ be a constant.
              -> Signal' clk (Unsigned n) -- ^ Write address @w@
              -> Signal' clk (Unsigned n) -- ^ Read address @r@
              -> Signal' clk Bool         -- ^ Write enable
              -> Signal' clk a            -- ^ Value to write (at address @w@)
              -> Signal' clk a
              -- ^ Value of the @blockRAM@ at address @r@ from the previous
              -- clock cycle
blockRamPow2' = blockRam'
