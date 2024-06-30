{-|
  Copyright  :  (C) 2013-2016, University of Twente
                    2017     , Google Inc.
                    2019     , Myrtle Software Ltd
  License    :  BSD2 (see the file LICENSE)
  Maintainer :  Christiaan Baaij <christiaan.baaij@gmail.com>

  Whereas the output of a Mealy machine depends on /current transition/, the
  output of a Moore machine depends on the /previous state/.

  Moore machines are strictly less expressive, but may impose laxer timing
  requirements.
-}

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoGeneralizedNewtypeDeriving #-}

{-# LANGUAGE Safe #-}

module Clash.Prelude.Moore
  ( -- * Moore machine
    moore
  , mooreB
  , medvedev
  , medvedevB
  )
where

import qualified Clash.Explicit.Moore as E
import           Clash.Signal
import           Clash.XException                     (NFDataX)

{- $setup
>>> :set -XDataKinds -XTypeApplications
>>> :m -Clash.Explicit.Prelude
>>> :m -Clash.Explicit.Prelude.Safe
>>> import Clash.Prelude
>>> :{
let macT s (x,y) = x * y + s
    mac = moore macT id 0
:}

-}


-- | Create a synchronous function from a combinational function describing
-- a moore machine
--
-- @
-- macT
--   :: Int        -- Current state
--   -> (Int,Int)  -- Input
--   -> Int        -- Updated state
-- macT s (x,y) = x * y + s
--
-- mac
--   :: HiddenClockResetEnable dom
--   => 'Signal' dom (Int, Int)
--   -> 'Signal' dom Int
-- mac = 'moore' mac id 0
-- @
--
-- >>> simulate @System mac [(0,0),(1,1),(2,2),(3,3),(4,4)]
-- [0,0,1,5,14,30,...
-- ...
--
-- Synchronous sequential functions can be composed just like their
-- combinational counterpart:
--
-- @
-- dualMac
--   :: HiddenClockResetEnable dom
--   => ('Signal' dom Int, 'Signal' dom Int)
--   -> ('Signal' dom Int, 'Signal' dom Int)
--   -> 'Signal' dom Int
-- dualMac (a,b) (x,y) = s1 + s2
--   where
--     s1 = 'moore' macT id 0 ('Clash.Signal.bundle' (a,x))
--     s2 = 'moore' macT id 0 ('Clash.Signal.bundle' (b,y))
-- @
moore
  :: forall dom s i o
   . ( HiddenClockResetEnable dom
     , NFDataX s )
  => (s -> i -> s)
  -- ^ Transfer function in moore machine form: @state -> input -> newstate@
  -> (s -> o)
  -- ^ Output function in moore machine form: @state -> output@
  -> s
  -- ^ Initial state
  -> (Signal dom i -> Signal dom o)
  -- ^ Synchronous sequential function with input and output matching that
  -- of the moore machine
moore = hideClockResetEnable @dom E.moore
{-# INLINE moore #-}


-- | Create a synchronous function from a combinational function describing
-- a moore machine without any output logic
medvedev
  :: ( HiddenClockResetEnable dom
     , NFDataX s )
  => (s -> i -> s)
  -> s
  -> (Signal dom i -> Signal dom s)
medvedev tr st = moore tr id st
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
-- g a b c = (b1,b2,i2)
--   where
--     (i1,b1) = 'Clash.Signal.unbundle' ('moore' t o 0 ('Clash.Signal.bundle' (a,b)))
--     (i2,b2) = 'Clash.Signal.unbundle' ('moore' t o 3 ('Clash.Signal.bundle' (c,i1)))
-- @
--
-- Using 'mooreB' however we can write:
--
-- @
-- g a b c = (b1,b2,i2)
--   where
--     (i1,b1) = 'mooreB' t o 0 (a,b)
--     (i2,b2) = 'mooreB' t o 3 (c,i1)
-- @
mooreB
  :: forall dom s i o
   . ( HiddenClockResetEnable dom
     , NFDataX s
     , Bundle i
     , Bundle o )
  => (s -> i -> s)
  -- ^ Transfer function in moore machine form: @state -> input -> newstate@
  -> (s -> o)
  -- ^ Output function in moore machine form: @state -> output@
  -> s
  -- ^ Initial state
  -> (Unbundled dom i -> Unbundled dom o)
   -- ^ Synchronous sequential function with input and output matching that
   -- of the moore machine
mooreB = hideClockResetEnable @dom E.mooreB
{-# INLINE mooreB #-}

-- | A version of 'medvedev' that does automatic 'Bundle'ing
medvedevB
  :: ( HiddenClockResetEnable dom
     , NFDataX s
     , Bundle i
     , Bundle s )
  => (s -> i -> s)
  -> s
  -> (Unbundled dom i -> Unbundled dom s)
medvedevB tr st = mooreB tr id st
{-# INLINE medvedevB #-}
