{-|
  Copyright  :  (C) 2013-2016, University of Twente,
                    2017     , Google Inc.
                    2019     , Myrtle Software Ltd
                    2023     , Alex Mason
  License    :  BSD2 (see the file LICENSE)
  Maintainer :  Christiaan Baaij <christiaan.baaij@gmail.com>

  Whereas the output of a Moore machine depends on the /previous state/, the
  output of a Mealy machine depends on /current transition/.

  Mealy machines are strictly more expressive, but may impose stricter timing
  requirements.
-}

{-# LANGUAGE NoGeneralizedNewtypeDeriving #-}

{-# LANGUAGE Safe #-}

module Clash.Explicit.Mealy
  ( -- * Mealy machines with explicit clock and reset ports
    mealy
  , mealyS
  , mealyB
  )
where

import           Clash.Explicit.Signal
  (KnownDomain, Bundle (..), Clock, Reset, Signal, Enable, register)
import           Clash.XException      (NFDataX)

import           Control.Monad.State.Strict
  (State, runState)

{- $setup
>>> :set -XDataKinds -XTypeApplications -XDeriveGeneric -XDeriveAnyClass
>>> import Clash.Explicit.Prelude as C
>>> import Clash.Explicit.Mealy (mealyS)
>>> import qualified Data.List as L
>>> import Control.Lens (Lens', (%=), (-=), uses, use)
>>> import Control.Monad.State.Strict (State)
>>> :{
let macT s (x,y) = (s',s)
      where
        s' = x * y + s
:}

>>> mac clk rst en = mealy clk rst en macT 0

>>> :{
data DelayState = DelayState { _history :: Vec 4 Int , _untilValid :: Index 4 } deriving (Generic,NFDataX)
:}

>>> :{
history :: Lens' DelayState (Vec 4 Int)
history f = \(DelayState d u) -> (`DelayState` u) <$> f d
:}

>>> :{
untilValid :: Lens' DelayState (Index 4)
untilValid f = \(DelayState d u) -> DelayState d <$> f u
:}

>>> :{
delayS :: Int -> State DelayState (Maybe Int)
delayS n = do
  history   %= (n +>>)
  remaining <- use untilValid
  if remaining > 0
  then do
     untilValid -= 1
     return Nothing
   else do
     out <- uses history C.last
     return (Just out)
:}

>>> let initialDelayState = DelayState (C.repeat 0) maxBound

>>> :{
delayTop :: Clock System -> Reset System -> Enable System -> Signal System Int -> Signal System (Maybe Int)
delayTop clk rst en = mealyS clk rst en delayS initialDelayState
:}

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
--     s1 = 'mealy' clk rst en macT 0 ('bundle' (a,x))
--     s2 = 'mealy' clk rst en macT 0 ('bundle' (b,y))
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

-- | Create a synchronous function from a combinational function describing
-- a mealy machine using the state monad. This can be particularly useful
-- when combined with lenses or optics to replicate imperative algorithms.
--
-- @
-- data DelayState = DelayState
--   { _history    :: Vec 4 Int
--   , _untilValid :: Index 4
--   }
-- makeLenses 'DelayedState
--
-- initialDelayState = DelayState (repeat 0) maxBound
--
-- delayS :: Int -> State DelayState (Maybe Int)
-- delayS n = do
--   history   %= (n +>>)
--   remaining <- use untilValid
--   if remaining > 0
--   then do
--      remaining -= 1
--      return Nothing
--    else do
--      out <- uses history last
--      return (Just out)
--
-- delayTop ::'KnownDomain' dom
--   => 'Clock' dom
--   -> 'Reset' dom
--   -> 'Enable' dom
--   -> ('Signal' dom Int -> 'Signal' dom (Maybe Int))
-- delayTop clk rst en = 'mealyS' clk rst en delayS initialDelayState
-- @
--
-- >>> L.take 7 $ simulate (delayTop systemClockGen systemResetGen enableGen) [-100,1,2,3,4,5,6,7,8]
-- [Nothing,Nothing,Nothing,Nothing,Just 1,Just 2,Just 3]
--
mealyS
  :: ( KnownDomain dom
     , NFDataX s )
  => Clock dom
  -- ^ 'Clock' to synchronize to
  -> Reset dom
  -> Enable dom
  -- ^ Global enable
  -> (i -> State s o)
  -- ^ Transfer function in mealy machine handling inputs using @Control.Monad.Strict.State s@.
  -> s
  -- ^ Initial state
  -> (Signal dom i -> Signal dom o)
  -- ^ Synchronous sequential function with input and output matching that
  -- of the mealy machine
mealyS clk rst en f iS =
  \i -> let (o,s') = unbundle $ (runState . f) <$> i <*> s
            s      = register clk rst en iS s'
        in o
{-# INLINABLE mealyS #-}

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
