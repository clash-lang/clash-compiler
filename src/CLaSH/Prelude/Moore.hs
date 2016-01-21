{-|
  Copyright  :  (C) 2013-2016, University of Twente
  License    :  BSD2 (see the file LICENSE)
  Maintainer :  Christiaan Baaij <christiaan.baaij@gmail.com>

  Whereas the output of a Mealy machine depends on /current transition/, the
  output of a Moore machine depends on the /previous state/.

  Moore machines are strictly less expressive, but may impose laxer timing
  requirements.
-}

{-# LANGUAGE Safe #-}

module CLaSH.Prelude.Moore
  ( -- * Moore machine synchronised to the system clock
    moore
  , mooreB
    -- * Moore machine synchronised to an arbitrary clock
  , moore'
  , mooreB'
  )
where

import CLaSH.Signal          (Signal, Unbundled)
import CLaSH.Signal.Explicit (Signal', SClock, register', systemClock)
import CLaSH.Signal.Bundle   (Bundle (..), Unbundled')

{- $setup
>>> :set -XDataKinds
>>> import CLaSH.Prelude
>>> :{
let mac s (x,y) = x * y + s
    topEntity = moore mac id 0
:}

>>> import CLaSH.Prelude.Explicit
>>> type ClkA = Clk "A" 100
>>> let clkA = sclock :: SClock ClkA
>>> let mac s (x,y) = x * y + s

>>> let topEntity = moore' clkA mac id 0
-}

{-# INLINE moore #-}
-- | Create a synchronous function from a combinational function describing
-- a moore machine
--
-- @
-- mac :: Int        -- Current state
--     -> (Int,Int)  -- Input
--     -> Int        -- Updated state
-- mac s (x,y) = x * y + s
--
-- topEntity :: 'Signal' (Int, Int) -> 'Signal' Int
-- topEntity = 'moore' mac id 0
-- @
--
-- >>> simulate topEntity [(1,1),(2,2),(3,3),(4,4)]
-- [0,1,5,14...
--
-- Synchronous sequential functions can be composed just like their
-- combinational counterpart:
--
-- @
-- dualMac :: ('Signal' Int, 'Signal' Int)
--         -> ('Signal' Int, 'Signal' Int)
--         -> 'Signal' Int
-- dualMac (a,b) (x,y) = s1 + s2
--   where
--     s1 = 'moore' mac id 0 ('CLaSH.Signal.bundle' (a,x))
--     s2 = 'moore' mac id 0 ('CLaSH.Signal.bundle' (b,y))
-- @
moore :: (s -> i -> s) -- ^ Transfer function in moore machine form:
                       -- @state -> input -> newstate@
      -> (s -> o)      -- ^ Output function in moore machine form:
                       -- @state -> output@
      -> s             -- ^ Initial state
      -> (Signal i -> Signal o)
      -- ^ Synchronous sequential function with input and output matching that
      -- of the moore machine
moore = moore' systemClock

{-# INLINE mooreB #-}
-- | A version of 'moore' that does automatic 'Bundle'ing
--
-- Given a functions @t@ and @o@ of types:
--
-- @
-- __t__ :: Int -> (Bool, Int) -> Int
-- __o__ :: Int -> (Int, Bool)
-- @
--
-- When we want to make compositions of @t@ and @o@ in @g@ using 'moore', we have to
-- write:
--
-- @
-- g a b c = (b1,b2,i2)
--   where
--     (i1,b1) = 'CLaSH.Signal.unbundle' ('moore' t o 0 ('CLaSH.Signal.bundle' (a,b)))
--     (i2,b2) = 'CLaSH.Signal.unbundle' ('moore' t o 3 ('CLaSH.Signal.bundle' (i1,c)))
-- @
--
-- Using 'mooreB' however we can write:
--
-- @
-- g a b c = (b1,b2,i2)
--   where
--     (i1,b1) = 'mooreB' t o 0 (a,b)
--     (i2,b2) = 'mooreB' t o 3 (i1,c)
-- @
mooreB :: (Bundle i, Bundle o)
      => (s -> i -> s) -- ^ Transfer function in moore machine form:
                       -- @state -> input -> newstate@
      -> (s -> o)      -- ^ Output function in moore machine form:
                       -- @state -> output@
      -> s             -- ^ Initial state
      -> (Unbundled i -> Unbundled o)
       -- ^ Synchronous sequential function with input and output matching that
       -- of the moore machine
mooreB = mooreB' systemClock

{-# INLINABLE moore' #-}
-- | Create a synchronous function from a combinational function describing
-- a moore machine
--
-- @
-- mac :: Int        -- Current state
--     -> (Int,Int)  -- Input
--     -> (Int,Int)  -- Updated state
-- mac s (x,y) = x * y + s
--
-- type ClkA = 'CLaSH.Signal.Explicit.Clk' \"A\" 100
--
-- clkA :: 'SClock' ClkA
-- clkA = 'CLaSH.Signal.Explicit.sclock'
--
-- topEntity :: 'Signal'' ClkA (Int, Int) -> 'Signal'' ClkA Int
-- topEntity = 'moore'' clkA mac id 0
-- @
--
-- >>> simulate topEntity [(1,1),(2,2),(3,3),(4,4)]
-- [0,1,5,14...
--
-- Synchronous sequential functions can be composed just like their
-- combinational counterpart:
--
-- @
-- dualMac :: ('Signal'' clkA Int, 'Signal'' clkA Int)
--         -> ('Signal'' clkA Int, 'Signal'' clkA Int)
--         -> 'Signal'' clkA Int
-- dualMac (a,b) (x,y) = s1 + s2
--   where
--     s1 = 'moore'' clkA mac id 0 ('CLaSH.Signal.Explicit.bundle'' clkA (a,x))
--     s2 = 'moore'' clkA mac id 0 ('CLaSH.Signal.Explicit.bundle'' clkA (b,y))
-- @
moore' :: SClock clk    -- ^ 'Clock' to synchronize to
       -> (s -> i -> s) -- ^ Transfer function in moore machine form:
                        -- @state -> input -> newstate@
       -> (s -> o)      -- ^ Output function in moore machine form:
                        -- @state -> output@
       -> s             -- ^ Initial state
       -> (Signal' clk i -> Signal' clk o)
       -- ^ Synchronous sequential function with input and output matching that
       -- of the moore machine
moore' clk ft fo iS = \i -> let s' = ft <$> s <*> i
                                s  = register' clk iS s'
                        in fo <$> s

{-# INLINE mooreB' #-}
-- | A version of 'moore'' that does automatic 'Bundle'ing
--
-- Given a functions @t@ and @o@ of types:
--
-- @
-- __t__ :: Int -> (Bool, Int) -> Int
-- __o__ :: Int -> (Int, Bool)
-- @
--
-- When we want to make compositions of @t@ and @o@ in @g@ using 'moore'', we have to
-- write:
--
-- @
-- g clk a b c = (b1,b2,i2)
--   where
--     (i1,b1) = 'CLaSH.Signal.Explicit.unbundle'' clk (moore' clk t o 0 ('CLaSH.Signal.Explicit.bundle'' clk (a,b)))
--     (i2,b2) = 'CLaSH.Signal.Explicit.unbundle'' clk (moore' clk t o 3 ('CLaSH.Signal.Explicit.bundle'' clk (i1,c)))
-- @
--
-- Using 'mooreB'' however we can write:
--
-- @
-- g clk a b c = (b1,b2,i2)
--   where
--     (i1,b1) = 'mooreB'' clk t o 0 (a,b)
--     (i2,b2) = 'mooreB'' clk to 3 (i1,c)
-- @
mooreB' :: (Bundle i, Bundle o)
        => SClock clk
        -> (s -> i -> s) -- ^ Transfer function in moore machine form:
                         -- @state -> input -> newstate@
        -> (s -> o)      -- ^ Output function in moore machine form:
                         -- @state -> output@
        -> s             -- ^ Initial state
        -> (Unbundled' clk i -> Unbundled' clk o)
        -- ^ Synchronous sequential function with input and output matching that
        -- of the moore machine
mooreB' clk ft fo iS i = unbundle' clk (moore' clk ft fo iS (bundle' clk i))
