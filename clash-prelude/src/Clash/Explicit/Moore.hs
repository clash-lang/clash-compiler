{-|
  Copyright  :  (C) 2013-2016, University of Twente,
                    2017     , Google Inc.
                    2019     , Myrtle Software Ltd
  License    :  BSD2 (see the file LICENSE)
  Maintainer :  Christiaan Baaij <christiaan.baaij@gmail.com>

  Whereas the output of a Mealy machine depends on /current transition/, the
  output of a Moore machine depends on the /previous state/.

  Moore machines are strictly less expressive, but may impose laxer timing
  requirements.
-}

{-# LANGUAGE Safe #-}

module Clash.Explicit.Moore
  ( -- * Moore machines with explicit clock and reset ports
    moore
  , mooreB
  , medvedev
  , medvedevB
  )
where

import           Clash.Explicit.Signal
  (KnownDomain, Bundle (..), Clock, Reset, Signal, Enable, register)
import           Clash.XException                 (NFDataX)

{- $setup
>>> :set -XDataKinds -XTypeApplications
>>> import Clash.Explicit.Prelude
>>> let macT s (x,y) = x * y + s
>>> let mac clk rst en = moore clk rst en macT id 0
-}

-- | Create a synchronous function from a combinational function describing
-- a moore machine
--
-- @
-- macT
--   :: Int        -- Current state
--   -> (Int,Int)  -- Input
--   -> (Int,Int)  -- Updated state
-- macT s (x,y) = x * y + s
--
-- mac
--   :: 'KnownDomain' dom
--   => 'Clock' dom
--   -> 'Reset' dom
--   -> 'Enable' dom
--   -> 'Signal' dom (Int, Int)
--   -> 'Signal' dom Int
-- mac clk rst en = 'moore' clk rst en macT id 0
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
--     s1 = 'moore' clk rst en macT id 0 ('bundle' (a,x))
--     s2 = 'moore' clk rst en macT id 0 ('bundle' (b,y))
-- @
moore
  :: ( KnownDomain dom
     , NFDataX s )
  => Clock dom
  -- ^ 'Clock' to synchronize to
  -> Reset dom
  -> Enable dom
  -> (s -> i -> s)
  -- ^ Transfer function in moore machine form: @state -> input -> newstate@
  -> (s -> o)
  -- ^ Output function in moore machine form: @state -> output@
  -> s
  -- ^ Initial state
  -> (Signal dom i -> Signal dom o)
  -- ^ Synchronous sequential function with input and output matching that
  -- of the moore machine
moore clk rst en ft fo iS =
  \i -> let s' = ft <$> s <*> i
            s  = register clk rst en iS s'
        in fo <$> s
{-# INLINABLE moore #-}

-- | Create a synchronous function from a combinational function describing
-- a moore machine without any output logic
medvedev
  :: ( KnownDomain dom
     , NFDataX s )
  => Clock dom
  -> Reset dom
  -> Enable dom
  -> (s -> i -> s)
  -> s
  -> (Signal dom i -> Signal dom s)
medvedev clk rst en tr st = moore clk rst en tr id st
{-# INLINE medvedev #-}

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
-- g clk rst en a b c = (b1,b2,i2)
--   where
--     (i1,b1) = 'unbundle' (moore clk rst en t o 0 ('bundle' (a,b)))
--     (i2,b2) = 'unbundle' (moore clk rst en t o 3 ('bundle' (c,i1)))
-- @
--
-- Using 'mooreB' however we can write:
--
-- @
-- g clk rst en a b c = (b1,b2,i2)
--   where
--     (i1,b1) = 'mooreB' clk rst en t o 0 (a,b)
--     (i2,b2) = 'mooreB' clk rst en t o 3 (c,i1)
-- @
mooreB
  :: ( KnownDomain dom
     , NFDataX s
     , Bundle i
     , Bundle o )
  => Clock dom
  -> Reset dom
  -> Enable dom
  -> (s -> i -> s)
  -- ^ Transfer function in moore machine form:
  -- @state -> input -> newstate@
  -> (s -> o)
  -- ^ Output function in moore machine form:
  -- @state -> output@
  -> s
  -- ^ Initial state
  -> (Unbundled dom i -> Unbundled dom o)
  -- ^ Synchronous sequential function with input and output matching that
  -- of the moore machine
mooreB clk rst en ft fo iS i = unbundle (moore clk rst en ft fo iS (bundle i))
{-# INLINE mooreB #-}

-- | A version of 'medvedev' that does automatic 'Bundle'ing
medvedevB
  :: ( KnownDomain dom
     , NFDataX s
     , Bundle i
     , Bundle s )
  => Clock dom
  -> Reset dom
  -> Enable dom
  -> (s -> i -> s)
  -> s
  -> (Unbundled dom i -> Unbundled dom s)
medvedevB clk rst en tr st = mooreB clk rst en tr id st
{-# INLINE medvedevB #-}
