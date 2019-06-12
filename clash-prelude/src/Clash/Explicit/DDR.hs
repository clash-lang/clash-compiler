{-|
Copyright  :  (C) 2017, Google Inc
License    :  BSD2 (see the file LICENSE)
Maintainer :  Christiaan Baaij <christiaan.baaij@gmail.com>

We simulate DDR signal by using 'Signal's which have exactly half the period
(or double the speed) of our normal 'Signal's.

The primitives in this module can be used to produce of consume DDR signals.

DDR signals are not meant to be used internally in a design,
but only to communicate with the outside world.

In some cases hardware specific DDR IN registers can be infered by synthesis tools
from these generic primitives. But to be sure your design will synthesize to
dedicated hardware resources use the functions from "Clash.Intel.DDR"
or "Clash.Xilinx.DDR".
-}

{-# LANGUAGE CPP                 #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE MagicHash           #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}
#if __GLASGOW_HASKELL__ >= 806
{-# LANGUAGE NoStarIsType #-}
#endif

module Clash.Explicit.DDR
  ( ddrIn
  , ddrOut
    -- * Internal
  , ddrIn#
  , ddrOut#
  )
where

import GHC.Stack (HasCallStack, withFrozenCallStack)

import Clash.Explicit.Prelude
import Clash.Signal.Internal


-- | DDR input primitive
--
-- Consumes a DDR input signal and produces a regular signal containing a pair
-- of values.
--
-- >>> printX $ sampleN 5 $ ddrIn systemClockGen syncResetGen (-1,-2,-3) (fromList [0..10])
-- [(X,X),((-1),(-2)),((-3),2),(3,4),(5,6)]
ddrIn
  :: ( HasCallStack
     , Undefined a
     , fast ~ 'Dom n pFast
     , slow ~ 'Dom n (2*pFast))
  => Clock slow gated
  -- ^ clock
  -> Reset slow synchronous
  -- ^ reset
  -> (a, a, a)
  -- ^ reset values
  -> Signal fast a
  -- ^ DDR input signal
  -> Signal slow (a,a)
  -- ^ normal speed output pairs
ddrIn clk rst (i0,i1,i2) = withFrozenCallStack $ ddrIn# clk rst i0 i1 i2


-- For details about all the seq's en seqX's
-- see the [Note: register strictness annotations] in Clash.Signal.Internal
ddrIn#
  :: forall a slow fast n pFast gated synchronous
   . ( HasCallStack
     , Undefined a
     , fast ~ 'Dom n pFast
     , slow ~ 'Dom n (2*pFast))
  => Clock slow gated
  -> Reset slow synchronous
  -> a
  -> a
  -> a
  -> Signal fast a
  -> Signal slow (a,a)
ddrIn# (Clock {}) (Sync rst) i0 i1 i2 =
  go ((deepErrorX "ddrIn: initial value 0 undefined")
     ,(deepErrorX "ddrIn: initial value 1 undefined")
     ,(deepErrorX "ddrIn: initial value 2 undefined"))
     rst
  where
    go :: (a,a,a) -> Signal slow Bool -> Signal fast a -> Signal slow (a,a)
    go (o0,o1,o2) rt@(~(r :- rs)) as@(~(x0 :- x1 :- xs)) =
      let (o0',o1',o2') = if r then (i0,i1,i2) else (o2,x0,x1)
      in o0 `seqX` o1 `seqX` (o0,o1) :- (rt `seq` as `seq` go (o0',o1',o2') rs xs)

ddrIn# (Clock {}) (Async rst) i0 i1 i2 =
  go ((deepErrorX "ddrIn: initial value 0 undefined")
     ,(deepErrorX "ddrIn: initial value 1 undefined")
     ,(deepErrorX "ddrIn: initial value 2 undefined"))
     rst
  where
    go :: (a,a,a) -> Signal slow Bool -> Signal fast a -> Signal slow (a,a)
    go (o0,o1,o2) ~(r :- rs) as@(~(x0 :- x1 :- xs)) =
      let (o0',o1',o2',o3',o4') = if r then (i0,i1,i0,i1,i2) else (o0,o1,o2,x0,x1)
      in o0' `seqX` o1' `seqX`(o0',o1') :- (as `seq` go (o2',o3',o4') rs xs)

ddrIn# (GatedClock _ _ ena) (Sync rst) i0 i1 i2 =
  go ((deepErrorX "ddrIn: initial value 0 undefined")
     ,(deepErrorX "ddrIn: initial value 1 undefined")
     ,(deepErrorX "ddrIn: initial value 2 undefined"))
     rst
     ena
  where
    go :: (a,a,a) -> Signal slow Bool -> Signal slow Bool -> Signal fast a -> Signal slow (a,a)
    go (o0,o1,o2) rt@(~(r :- rs)) ~(e :- es) as@(~(x0 :- x1 :- xs)) =
      let (o0',o1',o2') = if r then (i0,i1,i2) else (o2,x0,x1)
      in o0 `seqX` o1 `seqX` (o0,o1)
           :- (rt `seq` as `seq` if e then go (o0',o1',o2') rs es xs
                                      else go (o0 ,o1 ,o2)  rs es xs)

ddrIn# (GatedClock _ _ ena) (Async rst) i0 i1 i2 =
  go ((deepErrorX "ddrIn: initial value 0 undefined")
     ,(deepErrorX "ddrIn: initial value 1 undefined")
     ,(deepErrorX "ddrIn: initial value 2 undefined"))
     rst
     ena
  where
    go :: (a,a,a) -> Signal slow Bool -> Signal slow Bool -> Signal fast a -> Signal slow (a,a)
    go (o0,o1,o2) ~(r :- rs) ~(e :- es) as@(~(x0 :- x1 :- xs)) =
      let (o0',o1',o2',o3',o4') = if r then (i0,i1,i0,i1,i2) else (o0,o1,o2,x0,x1)
      in o0' `seqX` o1' `seqX` (o0',o1')
           :- (as `seq` if e then go (o2',o3',o4') rs es xs
                             else go (o0',o1',o2') rs es xs)
{-# NOINLINE ddrIn# #-}

-- | DDR output primitive
--
-- Produces a DDR output signal from a normal signal of pairs of input.
--
-- >>> sampleN 7 $ ddrOut systemClockGen asyncResetGen (-1) (fromList [(0,1),(2,3),(4,5)])
-- [-1,-1,-1,2,3,4,5]
ddrOut
  :: ( HasCallStack
     , Undefined a
     , fast ~ 'Dom n pFast
     , slow ~ 'Dom n (2*pFast))
  => Clock slow gated            -- ^ clock
  -> Reset slow synchronous      -- ^ reset
  -> a                           -- ^ reset value
  -> Signal slow (a,a)           -- ^ normal speed input pairs
  -> Signal fast a               -- ^ DDR output signal
ddrOut clk rst i0 = uncurry (withFrozenCallStack $ ddrOut# clk rst i0) . unbundle


ddrOut#
  :: ( HasCallStack
     , Undefined a
     , fast ~ 'Dom n pFast
     , slow ~ 'Dom n (2*pFast))
  => Clock slow gated
  -> Reset slow synchronous
  -> a
  -> Signal slow a
  -> Signal slow a
  -> Signal fast a
ddrOut# clk rst i0 xs ys =
    -- We only observe one reset value, because when the mux switches on the
    -- next clock level, the second register will already be outputting its
    -- first input.
    --
    -- That is why we drop the first value of the stream.
    let (_ :- out) = zipSig xs' ys' in out
  where
    xs' = register# clk rst (error "ddrOut: unreachable error") i0 xs
    ys' = register# clk rst (deepErrorX "ddrOut: initial value undefined") i0 ys
    zipSig (a :- as) (b :- bs) = a :- b :- zipSig as bs
{-# NOINLINE ddrOut# #-}
