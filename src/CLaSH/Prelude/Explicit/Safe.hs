{-|
Copyright  :  (C) 2013-2016, University of Twente
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

{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE TypeOperators       #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-# LANGUAGE Safe #-}

{-# OPTIONS_HADDOCK show-extensions #-}

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
  , riseEvery'
  , oscillate'
    -- * Exported modules
    -- ** Explicitly clocked synchronous signals
  , module CLaSH.Signal.Explicit
  )
where

import GHC.TypeLits
import Control.Applicative        (liftA2)
import Prelude                    hiding (repeat)

import CLaSH.Prelude.BlockRam     (blockRam', blockRamPow2')
import CLaSH.Prelude.Mealy        (mealy', mealyB')
import CLaSH.Prelude.Moore        (moore', mooreB')
import CLaSH.Prelude.RAM          (asyncRam',asyncRamPow2')
import CLaSH.Prelude.ROM          (rom', romPow2')
import CLaSH.Prelude.Synchronizer (dualFlipFlopSynchronizer,
                                   asyncFIFOSynchronizer)
import CLaSH.Promoted.Nat         (SNat(..))
import CLaSH.Sized.Index          (Index)
import CLaSH.Signal.Bundle        (Bundle(..), Unbundled')
import CLaSH.Signal.Explicit
import CLaSH.Sized.Unsigned


{- $setup
>>> :set -XDataKinds
>>> import CLaSH.Prelude
>>> type ClkA = Clk "A" 100
>>> let clkA = sclock :: SClock ClkA
>>> let rP = registerB' clkA (8::Int,8::Int)
-}

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
-- >>> simulateB rP [(1,1),(2,2),(3,3)] :: [(Int,Int)]
-- [(8,8),(1,1),(2,2),(3,3)...
-- ...
registerB' :: Bundle a => SClock clk -> a -> Unbundled' clk a -> Unbundled' clk a
registerB' clk i = unbundle Prelude.. register' clk i Prelude.. bundle

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

{-# INLINEABLE riseEvery' #-}
-- | Give a pulse every @n@ clock cycles. This is a useful helper function when
-- combined with functions like @'CLaSH.Signal.regEn'@ or @'CLaSH.Signal.mux'@,
-- in order to delay a register by a known amount.
riseEvery' :: forall clk n. KnownNat n => SClock clk -> SNat n -> Signal' clk Bool
riseEvery' clk SNat = moore' clk transfer output 0 (pure ())
  where
    output :: Index n -> Bool
    output = (== maxBound)

    transfer :: Index n -> () -> Index n
    transfer s _ = if (s == maxBound) then 0 else s+1

{-# INLINEABLE oscillate' #-}
-- | Oscillate a @'Bool'@ for a given number of cycles, given the starting state.
oscillate' :: forall clk n. KnownNat n => SClock clk -> Bool -> SNat n -> Signal' clk Bool
oscillate' clk begin SNat = moore' clk transfer snd (0, begin) (pure ())
  where
    transfer :: (Index n, Bool) -> () -> (Index n, Bool)
    transfer (s, i) _ =
      if s == maxBound
        then (0,   not i) -- reset state and oscillate output
        else (s+1, i)     -- hold current output
