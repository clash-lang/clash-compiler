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

{-# LANGUAGE FlexibleContexts #-}

{-# LANGUAGE Safe #-}

module Clash.Prelude.Mealy
  ( -- * Mealy machine synchronized to the system clock
    mealy
  , mealyB
  , (<^>)
  )
where

import qualified Clash.Explicit.Mealy as E
import           Clash.Signal
import           Clash.XException           (NFDataX)

{- $setup
>>> :set -XDataKinds -XTypeApplications
>>> :m -Clash.Explicit.Prelude
>>> :m -Clash.Explicit.Prelude.Safe
>>> :m -Clash.Prelude.Safe
>>> import Clash.Prelude
>>> :{
let macT s (x,y) = (s',s)
      where
        s' = x * y + s
    mac = mealy macT 0
:}

-}

-- | Create a synchronous function from a combinational function describing
-- a mealy machine
--
-- @
-- macT
--   :: Int        -- Current state
--   -> (Int,Int)  -- Input
--   -> (Int,Int)  -- (Updated state, output)
-- macT s (x,y) = (s',s)
--   where
--     s' = x * y + s
--
-- mac :: HiddenClockResetEnable dom  => 'Signal' dom (Int, Int) -> 'Signal' dom Int
-- mac = 'mealy' macT 0
-- @
--
-- >>> simulate @System mac [(0,0),(1,1),(2,2),(3,3),(4,4)]
-- [0,0,1,5,14...
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
--     s1 = 'mealy' mac 0 ('Clash.Signal.bundle' (a,x))
--     s2 = 'mealy' mac 0 ('Clash.Signal.bundle' (b,y))
-- @
mealy
  :: ( HiddenClockResetEnable dom
     , NFDataX s )
  => (s -> i -> (s,o))
  -- ^ Transfer function in mealy machine form: @state -> input -> (newstate,output)@
  -> s
  -- ^ Initial state
  -> (Signal dom i -> Signal dom o)
  -- ^ Synchronous sequential function with input and output matching that
  -- of the mealy machine
mealy = hideClockResetEnable E.mealy
{-# INLINE mealy #-}

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
--     (i1,b1) = 'Clash.Signal.unbundle' ('mealy' f 0 ('Clash.Signal.bundle' (a,b)))
--     (i2,b2) = 'Clash.Signal.unbundle' ('mealy' f 3 ('Clash.Signal.bundle' (c,i1)))
-- @
--
-- Using 'mealyB' however we can write:
--
-- @
-- g a b c = (b1,b2,i2)
--   where
--     (i1,b1) = 'mealyB' f 0 (a,b)
--     (i2,b2) = 'mealyB' f 3 (c,i1)
-- @
mealyB
  :: ( HiddenClockResetEnable dom
     , NFDataX s
     , Bundle i
     , Bundle o )
  => (s -> i -> (s,o))
  -- ^ Transfer function in mealy machine form: @state -> input -> (newstate,output)@
  -> s
  -- ^ Initial state
  -> (Unbundled dom i -> Unbundled dom o)
  -- ^ Synchronous sequential function with input and output matching that
  -- of the mealy machine
mealyB = hideClockResetEnable E.mealyB
{-# INLINE mealyB #-}

-- | Infix version of 'mealyB'
(<^>)
  :: ( HiddenClockResetEnable dom
     , NFDataX s
     , Bundle i
     , Bundle o )
  => (s -> i -> (s,o))
  -- ^ Transfer function in mealy machine form: @state -> input -> (newstate,output)@
  -> s
  -- ^ Initial state
 -> (Unbundled dom i -> Unbundled dom o)
 -- ^ Synchronous sequential function with input and output matching that
 -- of the mealy machine
(<^>) = mealyB
{-# INLINE (<^>) #-}
