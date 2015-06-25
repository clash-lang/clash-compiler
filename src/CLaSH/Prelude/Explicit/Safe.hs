{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators    #-}

{-# LANGUAGE Safe #-}

{-# OPTIONS_HADDOCK show-extensions #-}

{-|
Copyright  :  (C) 2013-2015, University of Twente
License    :  BSD2 (see the file LICENSE)
Maintainer :  Christiaan Baaij <christiaan.baaij@gmail.com>

__This is the <https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/safe-haskell.html Safe> API only of "CLaSH.Prelude.Explicit"__

This module defines the explicitly clocked counterparts of the functions
defined in "CLaSH.Prelude".

This module uses the explicitly clocked 'Signal'' synchronous signals, as
opposed to the implicitly clocked 'Signal' used in "CLaSH.Prelude". Take a
look at "CLaSH.Signal.Explicit" to see how you can make multi-clock designs
using explicitly clocked signals.
-}
module CLaSH.Prelude.Explicit.Safe
  ( -- * Creating synchronous sequential circuits
    mealy'
  , mealyB'
  , moore'
  , mooreB'
  , registerB'
    -- * Synchronizer circuits for safe clock domain crossing
  , dualFlipFlopSynchronizer
  , asyncFIFOSynchronizer
    -- * ROMs
  , rom'
  , romPow2'
    -- * RAM primitives with a combinational read port
  , asyncRam'
  , asyncRamPow2'
    -- * BlockRAM primitives
  , blockRam'
  , blockRamPow2'
    -- * Utility functions
  , isRising'
  , isFalling'
    -- * Exported modules
    -- ** Explicitly clocked synchronous signals
  , module CLaSH.Signal.Explicit
  )
where

import Control.Applicative        (liftA2)
import Prelude                    hiding (repeat)

import CLaSH.Prelude.BlockRam     (blockRam', blockRamPow2')
import CLaSH.Prelude.Mealy        (mealy', mealyB')
import CLaSH.Prelude.Moore        (moore', mooreB')
import CLaSH.Prelude.RAM          (asyncRam',asyncRamPow2')
import CLaSH.Prelude.ROM          (rom', romPow2')
import CLaSH.Prelude.Synchronizer (dualFlipFlopSynchronizer,
                                   asyncFIFOSynchronizer)
import CLaSH.Signal.Explicit

-- $setup
-- >>> :set -XDataKinds
-- >>> type ClkA = Clk "A" 100
-- >>> let clkA = sclock :: SClock ClkA
-- >>> let rP = registerB' clkA (8::Int,8::Int)

{-# INLINE registerB' #-}
-- | Create a 'register' function for product-type like signals (e.g.
-- @('Signal' a, 'Signal' b)@)
--
-- @
-- type ClkA = 'Clk' \"A\" 100
--
-- clkA :: 'SClock' ClkA
-- clkA = 'sclock'
--
-- rP :: ('Signal'' ClkA Int, 'Signal'' ClkA Int) -> ('Signal'' ClkA Int, 'Signal'' ClkA Int)
-- rP = 'registerB'' clkA (8,8)
-- @
--
-- >>> simulateB' clkA clkA rP [(1,1),(2,2),(3,3)] :: [(Int,Int)]
-- [(8,8),(1,1),(2,2),(3,3)...
registerB' :: Bundle a => SClock clk -> a -> Unbundled' clk a -> Unbundled' clk a
registerB' clk i = unbundle' clk Prelude.. register' clk i Prelude.. bundle' clk

{-# INLINABLE isRising' #-}
-- | Give a pulse when the 'Signal'' goes from 'minBound' to 'maxBound'
isRising' :: (Bounded a, Eq a)
          => SClock clk
          -> a -- ^ Starting value
          -> Signal' clk a
          -> Signal' clk Bool
isRising' clk is s = liftA2 edgeDetect prev s
  where
    prev = register' clk is s
    edgeDetect old new = old == minBound && new == maxBound

{-# INLINABLE isFalling' #-}
-- | Give a pulse when the 'Signal'' goes from 'maxBound' to 'minBound'
isFalling' :: (Bounded a, Eq a)
           => SClock clk
           -> a -- ^ Starting value
           -> Signal' clk a
           -> Signal' clk Bool
isFalling' clk is s = liftA2 edgeDetect prev s
  where
    prev = register' clk is s
    edgeDetect old new = old == maxBound && new == minBound
