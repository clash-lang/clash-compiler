module CLaSH.Prelude.Mealy
  ( -- * Mealy machine synchronised to the system clock
    mealy
  , mealyB
  , (<^>)
    -- * Mealy machine synchronised to an arbitrary clock
  , cmealy
  , cmealyB
  )
where

import Control.Applicative   ((<$>), (<*>))

import CLaSH.Signal          (Signal, Unbundled')
import CLaSH.Signal.Explicit (CSignal, SClock, cregister, systemClock)
import CLaSH.Signal.Bundle   (Bundle (..), Unbundled)

{-# INLINE mealy #-}
-- | Create a synchronous function from a combinational function describing
-- a mealy machine
--
-- > mac :: Int        -- Current state
-- >     -> (Int,Int)  -- Input
-- >     -> (Int,Int)  -- (Updated state, output)
-- > mac s (x,y) = (s',s)
-- >   where
-- >     s' = x * y + s
-- >
-- > topEntity :: Signal (Int, Int) -> Signal Int
-- > topEntity = mealy mac 0
--
-- >>> simulate topEntity [(1,1),(2,2),(3,3),(4,4),...
-- [0,1,5,14,30,...
--
-- Synchronous sequential functions can be composed just like their
-- combinational counterpart:
--
-- > dualMac :: (Signal Int, Signal Int)
-- >         -> (Signal Int, Signal Int)
-- >         -> Signal Int
-- > dualMac (a,b) (x,y) = s1 + s2
-- >   where
-- >     s1 = mealy mac 0 (bundle' (a,x))
-- >     s2 = mealy mac 0 (bundle' (b,y))
mealy :: (s -> i -> (s,o)) -- ^ Transfer function in mealy machine form:
                           -- @state -> input -> (newstate,output)@
      -> s                 -- ^ Initial state
      -> (Signal i -> Signal o)
      -- ^ Synchronous sequential function with input and output matching that
      -- of the mealy machine
mealy = cmealy systemClock

{-# INLINE mealyB #-}
-- | A version of 'mealy' that does automatic 'Bundle'ing
--
-- Given a function @f@ of type:
--
-- > f :: Int -> (Bool, Int) -> (Int, (Int, Bool))
--
-- When we want to make compositions of @f@ in @g@ using 'mealy', we have to
-- write:
--
-- @
-- g a b c = (b1,b2,i2)
--   where
--     (i1,b1) = 'CLaSH.Signal.unbundle'' (mealy f 0 ('CLaSH.Signal.bundle'' (a,b)))
--     (i2,b2) = 'CLaSH.Signal.unbundle'' (mealy f 3 ('CLaSH.Signal.bundle'' (i1,c)))
-- @
--
-- Using 'mealyB' however we can write:
--
-- > g a b c = (b1,b2,i2)
-- >   where
-- >     (i1,b1) = mealyB f 0 (a,b)
-- >     (i2,b2) = mealyB f 3 (i1,c)
mealyB :: (Bundle i, Bundle o)
       => (s -> i -> (s,o)) -- ^ Transfer function in mealy machine form:
                            -- @state -> input -> (newstate,output)@
       -> s                 -- ^ Initial state
       -> (Unbundled' i -> Unbundled' o)
       -- ^ Synchronous sequential function with input and output matching that
       -- of the mealy machine
mealyB = cmealyB systemClock

{-# INLINE (<^>) #-}
-- | Infix version of 'mealyB'
(<^>) :: (Bundle i, Bundle o)
      => (s -> i -> (s,o)) -- ^ Transfer function in mealy machine form:
                           -- @state -> input -> (newstate,output)@
      -> s                 -- ^ Initial state
      -> (Unbundled' i -> Unbundled' o)
      -- ^ Synchronous sequential function with input and output matching that
      -- of the mealy machine
(<^>) = mealyB

{-# INLINABLE cmealy #-}
-- | Create a synchronous function from a combinational function describing
-- a mealy machine
--
-- > mac :: Int        -- Current state
-- >     -> (Int,Int)  -- Input
-- >     -> (Int,Int)  -- (Updated state, output)
-- > mac s (x,y) = (s',s)
-- >   where
-- >     s' = x * y + s
-- >
-- > clk100 = Clock d100
-- >
-- > topEntity :: CSignal 100 (Int, Int) -> CSignal 100 Int
-- > topEntity = cmealy clk100 mac 0
--
-- >>> csimulate clk100 clk100 topEntity [(1,1),(2,2),(3,3),(4,4),...
-- [0,1,5,14,30,...
--
-- Synchronous sequential functions can be composed just like their
-- combinational counterpart:
--
-- > dualMac :: (CSignal 100 Int, CSignal 100 Int)
-- >         -> (CSignal 100 Int, CSignal 100 Int)
-- >         -> CSignal 100 Int
-- > dualMac (a,b) (x,y) = s1 + s2
-- >   where
-- >     s1 = cmealy clk100 mac 0 (bundle clk100 (a,x))
-- >     s2 = cmealy clk100 mac 0 (bundle clk100 (b,y))
cmealy :: SClock clk        -- ^ 'Clock' to synchronize to
       -> (s -> i -> (s,o)) -- ^ Transfer function in mealy machine form:
                            -- @state -> input -> (newstate,output)@
       -> s                 -- ^ Initial state
       -> (CSignal clk i -> CSignal clk o)
       -- ^ Synchronous sequential function with input and output matching that
       -- of the mealy machine
cmealy clk f iS = \i -> let (s',o) = unbundle clk $ f <$> s <*> i
                            s      = cregister clk iS s'
                        in  o

{-# INLINE cmealyB #-}
-- | A version of 'cmealy' that does automatic 'Bundle'ing
--
-- Given a function @f@ of type:
--
-- > f :: Int -> (Bool,Int) -> (Int,(Int,Bool))
--
-- When we want to make compositions of @f@ in @g@ using 'cmealy', we have to
-- write:
--
-- @
-- g clk a b c = (b1,b2,i2)
--   where
--     (i1,b1) = 'unbundle' clk (cmealy clk f 0 ('bundle' clk (a,b)))
--     (i2,b2) = 'unbundle' clk (cmealy clk f 3 ('bundle' clk (i1,c)))
-- @
--
-- Using 'cmealyB' however we can write:
--
-- > g a b c = (b1,b2,i2)
-- >   where
-- >     (i1,b1) = cmealyB clk f 0 (a,b)
-- >     (i2,b2) = cmealyB clk f 3 (i1,c)
cmealyB :: (Bundle i, Bundle o)
        => SClock clk
        -> (s -> i -> (s,o)) -- ^ Transfer function in mealy machine form:
                     -- @state -> input -> (newstate,output)@
        -> s                 -- ^ Initial state
        -> (Unbundled clk i -> Unbundled clk o)
        -- ^ Synchronous sequential function with input and output matching that
        -- of the mealy machine
cmealyB clk f iS i = unbundle clk (cmealy clk f iS (bundle clk i))
