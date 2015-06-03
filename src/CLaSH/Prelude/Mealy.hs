{-# LANGUAGE Safe #-}

{-|
  Copyright  :  (C) 2013-2015, University of Twente
  License    :  BSD2 (see the file LICENSE)
  Maintainer :  Christiaan Baaij <christiaan.baaij@gmail.com>

  Whereas the output of a Moore machine depends on the /previous state/, the
  outputof a Mealy machine depends on /current transition/.

  Mealy machines are strictly more expressive, but may impose stricter timing
  requirements.
-}
module CLaSH.Prelude.Mealy
  ( -- * Mealy machine synchronised to the system clock
    mealy
  , mealyB
  , (<^>)
    -- * Mealy machine synchronised to an arbitrary clock
  , mealy'
  , mealyB'
  )
where

import CLaSH.Signal          (Signal, Unbundled)
import CLaSH.Signal.Explicit (Signal', SClock, register', systemClock)
import CLaSH.Signal.Bundle   (Bundle (..), Unbundled')

-- $setup
-- >>> :set -XDataKinds
-- >>> import CLaSH.Prelude
-- >>> :{
-- let mac s (x,y) = (s',s)
--       where
--         s' = x * y + s
--     topEntity = mealy mac 0
-- :}
--
-- >>> import CLaSH.Prelude.Explicit
-- >>> type ClkA = Clk "A" 100
-- >>> let clkA = sclock :: SClock ClkA
-- >>> :{
-- let mac s (x,y) = (s',s)
--       where
--         s' = x * y + s
-- :}
--
-- >>> let topEntity = mealy' clkA mac 0

{-# INLINE mealy #-}
-- | Create a synchronous function from a combinational function describing
-- a mealy machine
--
-- @
-- mac :: Int        -- Current state
--     -> (Int,Int)  -- Input
--     -> (Int,Int)  -- (Updated state, output)
-- mac s (x,y) = (s',s)
--   where
--     s' = x * y + s
--
-- topEntity :: 'Signal' (Int, Int) -> 'Signal' Int
-- topEntity = 'mealy' mac 0
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
--     s1 = 'mealy' mac 0 ('CLaSH.Signal.bundle' (a,x))
--     s2 = 'mealy' mac 0 ('CLaSH.Signal.bundle' (b,y))
-- @
mealy :: (s -> i -> (s,o)) -- ^ Transfer function in mealy machine form:
                           -- @state -> input -> (newstate,output)@
      -> s                 -- ^ Initial state
      -> (Signal i -> Signal o)
      -- ^ Synchronous sequential function with input and output matching that
      -- of the mealy machine
mealy = mealy' systemClock

{-# INLINE mealyB #-}
-- | A version of 'mealy' that does automatic 'Bundle'ing
--
-- Given a function @f@ of type:
--
-- @
-- __f__ :: Int -> (Bool, Int) -> (Int, (Int, Bool))
-- @
--
-- When we want to make compositions of @f@ in @g@ using 'mealy', we have to
-- write:
--
-- @
-- g a b c = (b1,b2,i2)
--   where
--     (i1,b1) = 'CLaSH.Signal.unbundle' ('mealy' f 0 ('CLaSH.Signal.bundle' (a,b)))
--     (i2,b2) = 'CLaSH.Signal.unbundle' ('mealy' f 3 ('CLaSH.Signal.bundle' (i1,c)))
-- @
--
-- Using 'mealyB' however we can write:
--
-- @
-- g a b c = (b1,b2,i2)
--   where
--     (i1,b1) = 'mealyB' f 0 (a,b)
--     (i2,b2) = 'mealyB' f 3 (i1,c)
-- @
mealyB :: (Bundle i, Bundle o)
       => (s -> i -> (s,o)) -- ^ Transfer function in mealy machine form:
                            -- @state -> input -> (newstate,output)@
       -> s                 -- ^ Initial state
       -> (Unbundled i -> Unbundled o)
       -- ^ Synchronous sequential function with input and output matching that
       -- of the mealy machine
mealyB = mealyB' systemClock

{-# INLINE (<^>) #-}
-- | Infix version of 'mealyB'
(<^>) :: (Bundle i, Bundle o)
      => (s -> i -> (s,o)) -- ^ Transfer function in mealy machine form:
                           -- @state -> input -> (newstate,output)@
      -> s                 -- ^ Initial state
      -> (Unbundled i -> Unbundled o)
      -- ^ Synchronous sequential function with input and output matching that
      -- of the mealy machine
(<^>) = mealyB

{-# INLINABLE mealy' #-}
-- | Create a synchronous function from a combinational function describing
-- a mealy machine
--
-- @
-- mac :: Int        -- Current state
--     -> (Int,Int)  -- Input
--     -> (Int,Int)  -- (Updated state, output)
-- mac s (x,y) = (s',s)
--   where
--     s' = x * y + s
--
-- type ClkA = 'CLaSH.Signal.Explicit.Clk' \"A\" 100
--
-- clkA :: 'SClock' ClkA
-- clkA = 'CLaSH.Signal.Explicit.sclock'
--
-- topEntity :: 'Signal'' ClkA (Int, Int) -> 'Signal'' ClkA Int
-- topEntity = 'mealy'' clkA mac 0
-- @
--
-- >>> simulate topEntity [(1,1),(2,2),(3,3),(4,4)]
-- [0,1,5,14...
--
-- Synchronous sequential functions can be composed just like their
-- combinational counterpart:
--
-- @
-- dualMac :: ('Signal'' clkA100 Int, 'Signal'' clkA100 Int)
--         -> ('Signal'' clkA100 Int, 'Signal'' clkA100 Int)
--         -> 'Signal'' clkA100 Int
-- dualMac (a,b) (x,y) = s1 + s2
--   where
--     s1 = 'mealy'' clkA100 mac 0 ('CLaSH.Signal.Explicit.bundle'' clkA100 (a,x))
--     s2 = 'mealy'' clkA100 mac 0 ('CLaSH.Signal.Explicit.bundle'' clkA100 (b,y))
-- @
mealy' :: SClock clk        -- ^ 'Clock' to synchronize to
       -> (s -> i -> (s,o)) -- ^ Transfer function in mealy machine form:
                            -- @state -> input -> (newstate,output)@
       -> s                 -- ^ Initial state
       -> (Signal' clk i -> Signal' clk o)
       -- ^ Synchronous sequential function with input and output matching that
       -- of the mealy machine
mealy' clk f iS = \i -> let (s',o) = unbundle' clk $ f <$> s <*> i
                            s      = register' clk iS s'
                        in  o

{-# INLINE mealyB' #-}
-- | A version of 'mealy'' that does automatic 'Bundle'ing
--
-- Given a function @f@ of type:
--
-- @
-- __f__ :: Int -> (Bool,Int) -> (Int,(Int,Bool))
-- @
--
-- When we want to make compositions of @f@ in @g@ using 'mealy'', we have to
-- write:
--
-- @
-- g clk a b c = (b1,b2,i2)
--   where
--     (i1,b1) = 'CLaSH.Signal.Explicit.unbundle'' clk (mealy' clk f 0 ('CLaSH.Signal.Explicit.bundle'' clk (a,b)))
--     (i2,b2) = 'CLaSH.Signal.Explicit.unbundle'' clk (mealy' clk f 3 ('CLaSH.Signal.Explicit.bundle'' clk (i1,c)))
-- @
--
-- Using 'mealyB'' however we can write:
--
-- @
-- g clk a b c = (b1,b2,i2)
--   where
--     (i1,b1) = 'mealyB'' clk f 0 (a,b)
--     (i2,b2) = 'mealyB'' clk f 3 (i1,c)
-- @
mealyB' :: (Bundle i, Bundle o)
        => SClock clk
        -> (s -> i -> (s,o)) -- ^ Transfer function in mealy machine form:
                     -- @state -> input -> (newstate,output)@
        -> s                 -- ^ Initial state
        -> (Unbundled' clk i -> Unbundled' clk o)
        -- ^ Synchronous sequential function with input and output matching that
        -- of the mealy machine
mealyB' clk f iS i = unbundle' clk (mealy' clk f iS (bundle' clk i))
