{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators    #-}

{-# OPTIONS_HADDOCK show-extensions #-}

{-|
Copyright  :  (C) 2013-2015, University of Twente
License    :  BSD2 (see the file LICENSE)
Maintainer :  Christiaan Baaij <christiaan.baaij@gmail.com>

This module defines the explicitly clocked counterparts of the functions
defined in "CLaSH.Prelude".

This module uses the explicitly clocked 'Signal'' synchronous signals, as
opposed to the implicitly clocked 'Signal' used in "CLaSH.Prelude". Take a
look at "CLaSH.Signal.Explicit" to see how you can make multi-clock designs
using explicitly clocked signals.
-}
module CLaSH.Prelude.Explicit
  ( -- * Creating synchronous sequential circuits
    mealy'
  , mealyB'
  , registerB'
    -- * BlockRAM primitives
  , blockRam'
  , blockRamPow2'
    -- * Utility functions
  , window'
  , windowD'
  , isRising'
  , isFalling'
    -- * Testbench functions
  , stimuliGenerator'
  , outputVerifier'
    -- * Exported modules
    -- ** Explicitly clocked synchronous signals
  , module CLaSH.Signal.Explicit
  )
where

import Control.Applicative     (liftA2)
import Data.Default            (Default (..))
import GHC.TypeLits            (KnownNat, type (+), natVal)
import Prelude                 hiding (repeat)

import CLaSH.Prelude.BlockRam  (blockRam', blockRamPow2')
import CLaSH.Prelude.Mealy     (mealy', mealyB')
import CLaSH.Prelude.Testbench (stimuliGenerator', outputVerifier')
import CLaSH.Signal.Explicit
import CLaSH.Sized.Vector      (Vec (..), (+>>), asNatProxy, repeat)

{-# INLINE registerB' #-}
-- | Create a 'register' function for product-type like signals (e.g.
-- @('Signal' a, 'Signal' b)@)
--
-- @
-- type ClkA = 'Clk' \"A\" 100
--
-- clkA100 :: 'SClock' ClkA
-- clkA100 = 'sclock'
--
-- rP :: ('Signal'' ClkA Int, 'Signal'' ClkA Int) -> ('Signal'' ClkA Int, 'Signal'' ClkA Int)
-- rP = 'registerB'' clkA100 (8,8)
-- @
--
-- >>> simulateB' rP [(1,1),(2,2),(3,3),...
-- [(8,8),(1,1),(2,2),(3,3),...
registerB' :: Bundle a => SClock clk -> a -> Unbundled' clk a -> Unbundled' clk a
registerB' clk i = unbundle' clk Prelude.. register' clk i Prelude.. bundle' clk

{-# INLINABLE window' #-}
-- | Give a window over a 'Signal''
--
-- @
-- type ClkA = 'Clk' \"A\" 100
--
-- clkA100 :: 'SClock' ClkA
-- clkA100 = 'sclock'
--
-- window4 :: 'Signal'' ClkA Int -> 'Vec' 4 ('Signal'' ClkA Int)
-- window4 = 'window'' clkA100
-- @
--
-- >>> simulateB' clkA100 clkA100 window4 [1,2,3,4,5,...
-- [<1,0,0,0>, <2,1,0,0>, <3,2,1,0>, <4,3,2,1>, <5,4,3,2>,...
window' :: (KnownNat n, Default a)
        => SClock clk                  -- ^ Clock to which the incoming
                                       -- signal is synchronized
        -> Signal' clk a               -- ^ Signal to create a window over
        -> Vec (n + 1) (Signal' clk a) -- ^ Window of at least size 1
window' clk x = res
  where
    res  = x :> prev
    prev = case natVal (asNatProxy prev) of
             0 -> repeat def
             _ -> let next = x +>> prev
                  in  registerB' clk (repeat def) next

{-# INLINABLE windowD' #-}
-- | Give a delayed window over a 'Signal''
--
-- @
-- type ClkA = 'Clk' \"A\" 100
--
-- clkA100 :: 'SClock' ClkA
-- clkA100 = 'sclock'
--
-- windowD3 :: 'Signal'' ClkA Int -> 'Vec' 3 ('Signal'' ClkA Int)
-- windowD3 = 'windowD'
-- @
--
-- >>> simulateB' clkA100 clkA100 windowD3 [1,2,3,4,...
-- [<0,0,0>, <1,0,0>, <2,1,0>, <3,2,1>, <4,3,2>,...
windowD' :: (KnownNat (n + 1), Default a)
         => SClock clk                   -- ^ Clock to which the incoming signal
                                         -- is synchronized
         -> Signal' clk a                -- ^ Signal to create a window over
         -> Vec (n + 1) (Signal' clk a)  -- ^ Window of at least size 1
windowD' clk x = prev
  where
    prev = registerB' clk (repeat def) next
    next = x +>> prev

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
