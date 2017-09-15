{-|
  Copyright  :  (C) 2013-2016, University of Twente,
                    2017     , Google Inc.
  License    :  BSD2 (see the file LICENSE)
  Maintainer :  Christiaan Baaij <christiaan.baaij@gmail.com>

  Whereas the output of a Moore machine depends on the /previous state/, the
  output of a Mealy machine depends on /current transition/.

  Mealy machines are strictly more expressive, but may impose stricter timing
  requirements.
-}

{-# LANGUAGE Safe #-}

module Clash.Prelude.Mealy
  ( -- * Mealy machine synchronised to the system clock
    mealy
  , mealyB
  , (<^>)
  )
where

import qualified Clash.Explicit.Mealy as E
import           Clash.Signal

{- $setup
>>> :set -XDataKinds -XTypeApplications
>>> import Clash.Prelude
>>> :{
let mac s (x,y) = (s',s)
      where
        s' = x * y + s
    topEntity = mealy mac 0
:}

-}

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
-- topEntity :: HasSystemClockAndReset => 'Signal' System (Int, Int) -> 'Signal' System Int
-- topEntity = 'mealy' mac 0
-- @
--
-- >>> simulate topEntity [(1,1),(2,2),(3,3),(4,4)]
-- [0,1,5,14...
-- ...
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
--     s1 = 'mealy' mac 0 ('Clash.Signal.bundle' (a,x))
--     s2 = 'mealy' mac 0 ('Clash.Signal.bundle' (b,y))
-- @
mealy :: HasClockReset domain gated synchronous
      => (s -> i -> (s,o)) -- ^ Transfer function in mealy machine form:
                           -- @state -> input -> (newstate,output)@
      -> s                 -- ^ Initial state
      -> (Signal domain i -> Signal domain o)
      -- ^ Synchronous sequential function with input and output matching that
      -- of the mealy machine
mealy = E.mealy hasClock hasReset
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
--     (i2,b2) = 'Clash.Signal.unbundle' ('mealy' f 3 ('Clash.Signal.bundle' (i1,c)))
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
mealyB :: (Bundle i, Bundle o, HasClockReset domain gated synchronous)
       => (s -> i -> (s,o)) -- ^ Transfer function in mealy machine form:
                            -- @state -> input -> (newstate,output)@
       -> s                 -- ^ Initial state
       -> (Unbundled domain i -> Unbundled domain o)
       -- ^ Synchronous sequential function with input and output matching that
       -- of the mealy machine
mealyB = E.mealyB hasClock hasReset
{-# INLINE mealyB #-}

-- | Infix version of 'mealyB'
(<^>) :: (Bundle i, Bundle o, HasClockReset domain gated synchronous)
      => (s -> i -> (s,o)) -- ^ Transfer function in mealy machine form:
                           -- @state -> input -> (newstate,output)@
      -> s                 -- ^ Initial state
      -> (Unbundled domain i -> Unbundled domain o)
      -- ^ Synchronous sequential function with input and output matching that
      -- of the mealy machine
(<^>) = mealyB
{-# INLINE (<^>) #-}
