{-|
  Copyright  :  (C) 2013-2016, University of Twente,
                    2017     , Google Inc.
                    2019     , Myrtle Software Ltd
  License    :  BSD2 (see the file LICENSE)
  Maintainer :  Christiaan Baaij <christiaan.baaij@gmail.com>

  Whereas the output of a Moore machine depends on the /previous state/, the
  output of a Mealy machine depends on /current transition/.

  Mealy machines are strictly more expressive, but may impose stricter timing
  requirements.
-}

{-# LANGUAGE Safe #-}

module Clash.Explicit.Mealy
  ( -- * Mealy machines with explicit clock and reset ports
    mealy
  , mealyB
  )
where

import           Clash.Explicit.Signal
  (KnownDomain, Bundle (..), Clock, Reset, Signal, Enable, register)
import           Clash.XException      (NFDataX)

{- $setup
>>> :set -XDataKinds -XTypeApplications
>>> import Clash.Explicit.Prelude
>>> import qualified Data.List as L
>>> :{
let macT s (x,y) = (s',s)
      where
        s' = x * y + s
:}

>>> let mac clk rst en = mealy clk rst en macT 0
-}

-- | Create a synchronous function from a combinational function describing
-- a mealy machine
--
-- @
-- import qualified Data.List as L
--
-- macT
--   :: Int        -- Current state
--   -> (Int,Int)  -- Input
--   -> (Int,Int)  -- (Updated state, output)
-- macT s (x,y) = (s',s)
--   where
--     s' = x * y + s
--
-- mac
--   :: 'KnownDomain' dom
--   => 'Clock' dom
--   -> 'Reset' dom
--   -> 'Enable' dom
--   -> 'Signal' dom (Int, Int)
--   -> 'Signal' dom Int
-- mac clk rst en = 'mealy' clk rst en macT 0
-- @
--
-- >>> simulate (mac systemClockGen systemResetGen enableGen) [(0,0),(1,1),(2,2),(3,3),(4,4)]
-- [0,0,1,5,14...
-- ...
--
-- Synchronous sequential functions can be composed just like their
-- combinational counterpart:
--
-- @
-- dualMac
--   :: 'KnownDomain' dom
--   => 'Clock' dom
--   -> 'Reset' dom
--   -> 'Enable' dom
--   -> ('Signal' dom Int, 'Signal' dom Int)
--   -> ('Signal' dom Int, 'Signal' dom Int)
--   -> 'Signal' dom Int
-- dualMac clk rst en (a,b) (x,y) = s1 + s2
--   where
--     s1 = 'mealy' clk rst en mac 0 ('bundle' (a,x))
--     s2 = 'mealy' clk rst en mac 0 ('bundle' (b,y))
-- @
mealy
  :: ( KnownDomain dom
     , NFDataX s )
  => Clock dom
  -- ^ 'Clock' to synchronize to
  -> Reset dom
  -> Enable dom
  -- ^ Global enable
  -> (s -> i -> (s,o))
  -- ^ Transfer function in mealy machine form: @state -> input -> (newstate,output)@
  -> s
  -- ^ Initial state
  -> (Signal dom i -> Signal dom o)
  -- ^ Synchronous sequential function with input and output matching that
  -- of the mealy machine
mealy clk rst en f iS =
  \i -> let (s',o) = unbundle $ f <$> s <*> i
            s      = register clk rst en iS s'
        in  o
{-# INLINABLE mealy #-}

-- | A version of 'mealy' that does automatic 'Bundle'ing
--
-- Given a function @f@ of type:
--
-- @
-- __f__ :: Int -> (Bool,Int) -> (Int,(Int,Bool))
-- @
--
-- When we want to make compositions of @f@ in @g@ using 'mealy', we have to
-- write:
--
-- @
-- g clk rst en a b c = (b1,b2,i2)
--   where
--     (i1,b1) = 'unbundle' (mealy clk rst en f 0 ('bundle' (a,b)))
--     (i2,b2) = 'unbundle' (mealy clk rst en f 3 ('bundle' (c,i1)))
-- @
--
-- Using 'mealyB' however we can write:
--
-- @
-- g clk rst en a b c = (b1,b2,i2)
--   where
--     (i1,b1) = 'mealyB' clk rst en f 0 (a,b)
--     (i2,b2) = 'mealyB' clk rst en f 3 (c,i1)
-- @
mealyB
  :: ( KnownDomain dom
     , NFDataX s
     , Bundle i
     , Bundle o )
  => Clock dom
  -> Reset dom
  -> Enable dom
  -> (s -> i -> (s,o))
  -- ^ Transfer function in mealy machine form: @state -> input -> (newstate,output)@
  -> s
  -- ^ Initial state
  -> (Unbundled dom i -> Unbundled dom o)
 -- ^ Synchronous sequential function with input and output matching that
 -- of the mealy machine
mealyB clk rst en f iS i = unbundle (mealy clk rst en f iS (bundle i))
{-# INLINE mealyB #-}
