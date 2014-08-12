{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators    #-}
module CLaSH.Prelude.BlockRam where

import GHC.TypeLits           (KnownNat, type (^))
import Prelude                hiding ((!!))

import CLaSH.Prelude.Mealy    (sync)
import CLaSH.Signal.Explicit  (CSignal, SClock, Wrap, cunwrap, systemClock)
import CLaSH.Signal           (Signal)
import CLaSH.Sized.Unsigned   (Unsigned)
import CLaSH.Sized.Vector     (Vec, (!!), replace)

{-# NOINLINE blockRam #-}
-- | Create a blockRAM with space for @n@ elements.
--
-- * __NB__: Read value is delayed by 1 cycle
-- * __NB__: Initial output value is `undefined`
--
-- > bram40 :: Signal (Unsigned 6) -> Signal (Unsigned 6) -> Signal Bool -> Signal Bit -> Signal Bit
-- > bram40 = blockRam (vcopy d40 H)
blockRam :: (Wrap a, KnownNat n, KnownNat m)
         => Vec n a             -- ^ Initial content of the BRAM, also determines the size ,@n@, of the BRAM.
                                -- NB: *MUST* be a constant.
         -> Signal (Unsigned m) -- ^ Write address @w@
         -> Signal (Unsigned m) -- ^ Read address @r@
         -> Signal Bool         -- ^ Write enable
         -> Signal a            -- ^ Value to write (at address @w@)
         -> Signal a            -- ^ Value of the 'blockRAM' at address @r@ from the previous clock cycle
blockRam = cblockRam systemClock

{-# INLINABLE blockRamPow2 #-}
-- | Create a blockRAM with space for 2^@n@ elements
--
-- * __NB__: Read value is delayed by 1 cycle
-- * __NB__: Initial output value is `undefined`
--
-- > bram32 :: Signal (Unsigned 5) -> Signal (Unsigned 5) -> Signal Bool -> Signal Bit -> Signal Bit
-- > bram32 = blockRamPow2 (vcopy d32 H)
blockRamPow2 :: (Wrap a, KnownNat (2^n), KnownNat n)
             => Vec (2^n) a         -- ^ Initial content of the BRAM, also determines the size ,@2^n@, of the BRAM.
                                    -- NB: *MUST* be a constant.
             -> Signal (Unsigned n) -- ^ Write address @w@
             -> Signal (Unsigned n) -- ^ Read address @r@
             -> Signal Bool         -- ^ Write enable
             -> Signal a            -- ^ Value to write (at address @w@)
             -> Signal a            -- ^ Value of the 'blockRAM' at address @r@ from the previous clock cycle
blockRamPow2 = blockRam

{-# NOINLINE cblockRam #-}
-- | Create a blockRAM with space for @n@ elements
--
-- * __NB__: Read value is delayed by 1 cycle
-- * __NB__: Initial output value is `undefined`
--
-- > clk100 = Clock d100
-- >
-- > bram40 :: CSignal 100 (Unsigned 6) -> CSignal 100 (Unsigned 6)
-- >        -> CSignal 100 Bool -> CSignal 100 Bit -> 100 CSignal Bit
-- > bram40 = cblockRam clk100 (vcopy d40 H)
cblockRam :: (Wrap a, KnownNat n, KnownNat m)
          => SClock clk               -- ^ 'Clock' to synchronize to
          -> Vec n a                  -- ^ Initial content of the BRAM, also determines the size ,@n@, of the BRAM.
                                      -- NB: *MUST* be a constant.
          -> CSignal clk (Unsigned m) -- ^ Write address @w@
          -> CSignal clk (Unsigned m) -- ^ Read address @r@
          -> CSignal clk Bool         -- ^ Write enable
          -> CSignal clk a            -- ^ Value to write (at address @w@)
          -> CSignal clk a            -- ^ Value of the 'blockRAM' at address @r@ from the previous clock cycle
cblockRam clk binit wr rd en din = cunwrap clk $ (sync clk bram' (binit,undefined)) (wr,rd,en,din)
  where
    bram' (ram,o) (w,r,e,d) = ((ram',o'),o)
      where
        ram' | e         = replace ram w d
             | otherwise = ram
        o'               = ram !! r

{-# INLINABLE cblockRamPow2 #-}
-- | Create a blockRAM with space for 2^@n@ elements
--
-- * __NB__: Read value is delayed by 1 cycle
-- * __NB__: Initial output value is `undefined`
--
-- > clk100 = Clock d100
-- >
-- > bramC32 :: CSignal 100 (Unsigned 5) -> CSignal 100 (Unsigned 5) -> CSignal 100 Bool -> CSignal 100 Bit -> CSignal 100 Bit
-- > bramC32 = cblockRamPow2 clk100 (vcopy d32 H)
cblockRamPow2 :: (Wrap a, KnownNat n, KnownNat (2^n))
              => SClock clk               -- ^ 'Clock' to synchronize to
              -> Vec (2^n) a              -- ^ Initial content of the BRAM, also determines the size ,@2^n@, of the BRAM.
                                          -- NB: *MUST* be a constant.
              -> CSignal clk (Unsigned n) -- ^ Write address @w@
              -> CSignal clk (Unsigned n) -- ^ Read address @r@
              -> CSignal clk Bool         -- ^ Write enable
              -> CSignal clk a            -- ^ Value to write (at address @w@)
              -> CSignal clk a            -- ^ Value of the 'blockRAM' at address @r@ from the previous clock cycle
cblockRamPow2 = cblockRam
