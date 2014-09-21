module CLaSH.Prelude.Mealy
  ( -- * Mealy machine synchronised to the system clock
    (<^>)
    -- * Mealy machine synchronised to an arbitrary clock
  , sync
  )
where

import Control.Applicative   ((<$>), (<*>))

import CLaSH.Signal          (SBundled)
import CLaSH.Signal.Explicit (SClock, cregister, systemClock)
import CLaSH.Signal.Bundle   (Bundle (..), Bundled)

{-# INLINE (<^>) #-}
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
-- > topEntity :: (Signal Int, Signal Int) -> Signal Int
-- > topEntity = mac <^> 0
--
-- >>> simulateP topEntity [(1,1),(2,2),(3,3),(4,4),...
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
-- >     s1 = (mac <^> 0) (a,b)
-- >     s2 = (mac <^> 0) (x,y)
(<^>) :: (Bundle i, Bundle o)
      => (s -> i -> (s,o)) -- ^ Transfer function in mealy machine form:
                           -- @state -> input -> (newstate,output)@
      -> s                 -- ^ Initial state
      -> (SBundled i -> SBundled o)
      -- ^ Synchronous sequential function with input and output matching that
      -- of the mealy machine
(<^>) = sync systemClock

{-# INLINABLE sync #-}
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
-- > topEntity :: (CSignal 100 Int, CSignal 100 Int) -> CSignal 100 Int
-- > topEntity = sync clk100 mac 0
--
-- >>> csimulateW clk100 clk100 topEntity [(1,1),(2,2),(3,3),(4,4),...
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
-- >     s1 = sync clk100 mac 0 (a,b)
-- >     s2 = sync clk100 mac 0 (x,y)
sync :: (Bundle i, Bundle o)
     => SClock clk        -- ^ 'Clock' to synchronize to
     -> (s -> i -> (s,o)) -- ^ Transfer function in mealy machine form:
                          -- @state -> input -> (newstate,output)@
     -> s                 -- ^ Initial state
     -> (Bundled clk i -> Bundled clk o)
    -- ^ Synchronous sequential function with input and output matching that
    -- of the mealy machine
sync clk f iS = \i -> let (s',o) = unbundle clk $ f <$> s <*> bundle clk i
                          s      = cregister clk iS s'
                      in unbundle clk o
