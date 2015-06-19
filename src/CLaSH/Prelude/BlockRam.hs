{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MagicHash        #-}
{-# LANGUAGE TypeOperators    #-}

{-# LANGUAGE Trustworthy #-}

{-|
Copyright  :  (C) 2013-2015, University of Twente
License    :  BSD2 (see the file LICENSE)
Maintainer :  Christiaan Baaij <christiaan.baaij@gmail.com>

BlockRAM primitives
-}
module CLaSH.Prelude.BlockRam
  ( -- * BlockRAM synchronised to the system clock
    blockRam
  , blockRamPow2
    -- * BlockRAM synchronised to an arbitrary clock
  , blockRam'
  , blockRamPow2'
    -- * Internal
  , blockRam#
  )
where

import Control.Monad          (when)
import Control.Monad.ST.Lazy  (ST,runST)
import Data.Array.MArray      (newListArray,readArray,writeArray)
import Data.Array.ST          (STArray)
import GHC.TypeLits           (KnownNat, type (^))

import CLaSH.Signal           (Signal)
import CLaSH.Signal.Explicit  (Signal', SClock, register', systemClock)
import CLaSH.Signal.Bundle    (bundle')
import CLaSH.Sized.Unsigned   (Unsigned)
import CLaSH.Sized.Vector     (Vec, maxIndex, toList)

{-# INLINE blockRam #-}
-- | Create a blockRAM with space for @n@ elements.
--
-- * __NB__: Read value is delayed by 1 cycle
-- * __NB__: Initial output value is 'undefined'
--
-- @
-- bram40 :: 'Signal' ('Unsigned' 6) -> Signal ('Unsigned' 6) -> 'Signal' Bool
--        -> 'Signal' 'CLaSH.Sized.BitVector.Bit' -> Signal 'CLaSH.Sized.BitVector.Bit'
-- bram40 = 'blockRam' ('CLaSH.Sized.Vector.replicate' d40 1)
-- @
blockRam :: (KnownNat n, Enum addr)
         => Vec n a     -- ^ Initial content of the BRAM, also
                        -- determines the size, @n@, of the BRAM.
                        --
                        -- __NB__: __MUST__ be a constant.
         -> Signal addr -- ^ Write address @w@
         -> Signal addr -- ^ Read address @r@
         -> Signal Bool -- ^ Write enable
         -> Signal a    -- ^ Value to write (at address @w@)
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
-- bram32 :: 'Signal' ('Unsigned' 5) -> Signal ('Unsigned' 5) -> 'Signal' Bool
--        -> 'Signal' 'CLaSH.Sized.BitVector.Bit' -> 'Signal' 'CLaSH.Sized.BitVector.Bit'
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

{-# INLINE blockRam' #-}
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
-- bram40 :: 'Signal'' ClkA ('Unsigned' 6) -> 'Signal'' ClkA ('Unsigned' 6)
--        -> 'Signal'' ClkA Bool -> 'Signal'' ClkA 'CLaSH.Sized.BitVector.Bit' -> ClkA 'Signal'' 'CLaSH.Sized.BitVector.Bit'
-- bram40 = 'blockRam'' clkA100 ('CLaSH.Sized.Vector.replicate' d40 1)
-- @
blockRam' :: (KnownNat n, Enum addr)
          => SClock clk       -- ^ 'Clock' to synchronize to
          -> Vec n a          -- ^ Initial content of the BRAM, also
                              -- determines the size, @n@, of the BRAM.
                              --
                              -- __NB__: __MUST__ be a constant.
          -> Signal' clk addr -- ^ Write address @w@
          -> Signal' clk addr -- ^ Read address @r@
          -> Signal' clk Bool -- ^ Write enable
          -> Signal' clk a    -- ^ Value to write (at address @w@)
          -> Signal' clk a
          -- ^ Value of the @blockRAM@ at address @r@ from the previous clock
          -- cycle
blockRam' clk content wr rd en din = blockRam# clk content (fromEnum <$> wr)
                                               (fromEnum <$> rd) en din

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
-- bram32 :: 'Signal'' ClkA ('Unsigned' 5) -> Signal' ClkA ('Unsigned' 5)
--        -> 'Signal'' ClkA Bool -> 'Signal'' ClkA 'CLaSH.Sized.BitVector.Bit' -> Signal' ClkA 'CLaSH.Sized.BitVector.Bit'
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

{-# NOINLINE blockRam# #-}
-- | blockRAM primitive
blockRam# :: KnownNat n
          => SClock clk       -- ^ 'Clock' to synchronize to
          -> Vec n a          -- ^ Initial content of the BRAM, also
                              -- determines the size, @n@, of the BRAM.
                              --
                              -- __NB__: __MUST__ be a constant.
          -> Signal' clk Int  -- ^ Write address @w@
          -> Signal' clk Int  -- ^ Read address @r@
          -> Signal' clk Bool -- ^ Write enable
          -> Signal' clk a    -- ^ Value to write (at address @w@)
          -> Signal' clk a
          -- ^ Value of the @blockRAM@ at address @r@ from the previous clock
          -- cycle
blockRam# clk content wr rd en din = register' clk undefined dout
  where
    szI  = fromInteger $ maxIndex content
    dout = runST $ do
      arr <- newListArray (0,szI) (toList content)
      traverse (ramT arr) (bundle' clk (wr,rd,en,din))

    ramT :: STArray s Int e -> (Int,Int,Bool,e) -> ST s e
    ramT ram (w,r,e,d) = do
      d' <- readArray ram r
      when e (writeArray ram w d)
      return d'
