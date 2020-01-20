{-|
Copyright   :  (C) 2015-2016, University of Twente,
                   2016-2019, Myrtle Software Ltd,
                   2017     , Google Inc.
License     :  BSD2 (see the file LICENSE)
Maintainer  :  Christiaan Baaij <christiaan.baaij@gmail.com>

Synchronizer circuits for safe clock domain crossings
-}

{-# LANGUAGE CPP #-}
{-# LANGUAGE TypeFamilies #-}

{-# LANGUAGE Safe #-}

{-# OPTIONS_GHC -fplugin GHC.TypeLits.Normalise       #-}
{-# OPTIONS_GHC -fplugin GHC.TypeLits.KnownNat.Solver #-}

{-# OPTIONS_HADDOCK show-extensions #-}

module Clash.Explicit.Synchronizer
  ( -- * Bit-synchronizers
    dualFlipFlopSynchronizer
    -- * Word-synchronizers
  , asyncFIFOSynchronizer
  )
where

import Control.Applicative         (liftA2)
import Data.Bits                   (complement, shiftR, xor)
import Data.Constraint             ((:-)(..), Dict (..))
import Data.Constraint.Nat         (leTrans)
import Data.Maybe                  (isJust)
import GHC.TypeLits                (type (+), type (-), type (<=))

import Clash.Class.BitPack         (boolToBV)
import Clash.Class.Resize          (truncateB)
import Clash.Prelude.BitIndex      (slice)
import Clash.Explicit.Mealy        (mealyB)
import Clash.Explicit.RAM          (asyncRam)
import Clash.Explicit.Signal
  (Clock, Reset, Signal, Enable, register, unsafeSynchronizer)
import Clash.Promoted.Nat          (SNat (..), pow2SNat)
import Clash.Promoted.Nat.Literals (d0)
import Clash.Signal                (mux, KnownDomain)
import Clash.Sized.BitVector       (BitVector, (++#))
import Clash.XException            (NFDataX)

-- * Dual flip-flop synchronizer

-- | Synchronizer based on two sequentially connected flip-flops.
--
--  * __NB__: This synchronizer can be used for __bit__-synchronization.
--
--  * __NB__: Although this synchronizer does reduce metastability, it does
--  not guarantee the proper synchronization of a whole __word__. For
--  example, given that the output is sampled twice as fast as the input is
--  running, and we have two samples in the input stream that look like:
--
--      @[0111,1000]@
--
--      But the circuit driving the input stream has a longer propagation delay
--      on __msb__ compared to the __lsb__s. What can happen is an output stream
--      that looks like this:
--
--      @[0111,0111,0000,1000]@
--
--      Where the level-change of the __msb__ was not captured, but the level
--      change of the __lsb__s were.
--
--      If you want to have /safe/ __word__-synchronization use
--      'asyncFIFOSynchronizer'.
dualFlipFlopSynchronizer
  :: ( NFDataX a
     , KnownDomain dom1
     , KnownDomain dom2 )
  => Clock dom1
  -- ^ 'Clock' to which the incoming  data is synchronized
  -> Clock dom2
  -- ^ 'Clock' to which the outgoing data is synchronized
  -> Reset dom2
  -- ^ 'Reset' for registers on the outgoing domain
  -> Enable dom2
  -- ^ 'Enable' for registers on the outgoing domain
  -> a
  -- ^ Initial value of the two synchronization registers
  -> Signal dom1 a
  -- ^ Incoming data
  -> Signal dom2 a
  -- ^ Outgoing, synchronized, data
dualFlipFlopSynchronizer clk1 clk2 rst en i =
  register clk2 rst en i
    . register clk2 rst en i
    . unsafeSynchronizer clk1 clk2

-- * Asynchronous FIFO synchronizer

fifoMem
  :: ( KnownDomain wdom
     , KnownDomain rdom )
  => Clock wdom
  -> Clock rdom
  -> Enable wdom
  -> SNat addrSize
  -> Signal wdom Bool
  -> Signal rdom (BitVector addrSize)
  -> Signal wdom (Maybe (BitVector addrSize, a))
  -> Signal rdom a
fifoMem wclk rclk en addrSize@SNat full raddr writeM =
  asyncRam
    wclk rclk en
    (pow2SNat addrSize)
    raddr
    (mux full (pure Nothing) writeM)

ptrCompareT
  :: SNat addrSize
  -> (BitVector (addrSize + 1) -> BitVector (addrSize + 1) -> Bool)
  -> ( BitVector (addrSize + 1)
     , BitVector (addrSize + 1)
     , Bool )
  -> ( BitVector (addrSize + 1)
     , Bool )
  -> ( ( BitVector (addrSize + 1)
       , BitVector (addrSize + 1)
       , Bool )
     , ( Bool
       , BitVector addrSize
       , BitVector (addrSize + 1)
       )
     )
ptrCompareT SNat flagGen (bin, ptr, flag) (s_ptr, inc) =
  ((bin', ptr', flag'), (flag, addr, ptr))
 where
  -- GRAYSTYLE2 pointer
  bin' = bin + boolToBV (inc && not flag)
  ptr' = (bin' `shiftR` 1) `xor` bin'
  addr = truncateB bin

  flag' = flagGen ptr' s_ptr

-- FIFO full: when next pntr == synchonized {~wptr[addrSize:addrSize-1],wptr[addrSize-1:0]}
isFull
  :: forall addrSize
   . (2 <= addrSize)
  => SNat addrSize
  -> BitVector (addrSize + 1)
  -> BitVector (addrSize + 1)
  -> Bool
isFull addrSize@SNat ptr s_ptr =
  case leTrans @1 @2 @addrSize of
    Sub Dict ->
      let a1 = SNat @(addrSize - 1)
          a2 = SNat @(addrSize - 2)
      in  ptr == (complement (slice addrSize a1 s_ptr) ++# slice a2 d0 s_ptr)

-- | Synchronizer implemented as a FIFO around an asynchronous RAM. Based on the
-- design described in "Clash.Tutorial#multiclock", which is itself based on the
-- design described in <http://www.sunburst-design.com/papers/CummingsSNUG2002SJ_FIFO1.pdf>.
--
-- __NB__: This synchronizer can be used for __word__-synchronization.
asyncFIFOSynchronizer
  :: ( KnownDomain wdom
     , KnownDomain rdom
     , 2 <= addrSize )
  => SNat addrSize
  -- ^ Size of the internally used addresses, the  FIFO contains @2^addrSize@
  -- elements.
  -> Clock wdom
  -- ^ 'Clock' to which the write port is synchronized
  -> Clock rdom
  -- ^ 'Clock' to which the read port is synchronized
  -> Reset wdom
  -> Reset rdom
  -> Enable wdom
  -> Enable rdom
  -> Signal rdom Bool
  -- ^ Read request
  -> Signal wdom (Maybe a)
  -- ^ Element to insert
  -> (Signal rdom a, Signal rdom Bool, Signal wdom Bool)
  -- ^ (Oldest element in the FIFO, @empty@ flag, @full@ flag)
asyncFIFOSynchronizer addrSize@SNat wclk rclk wrst rrst wen ren rinc wdataM =
  (rdata, rempty, wfull)
 where
  s_rptr = dualFlipFlopSynchronizer rclk wclk wrst wen 0 rptr
  s_wptr = dualFlipFlopSynchronizer wclk rclk rrst ren 0 wptr

  rdata =
    fifoMem
      wclk rclk wen
      addrSize wfull raddr
      (liftA2 (,) <$> (pure <$> waddr) <*> wdataM)

  (rempty, raddr, rptr) =
    mealyB
      rclk rrst ren
      (ptrCompareT addrSize (==))
      (0, 0, True)
      (s_wptr, rinc)

  (wfull, waddr, wptr) =
    mealyB
      wclk wrst wen
      (ptrCompareT addrSize (isFull addrSize))
      (0, 0, False)
      (s_rptr, isJust <$> wdataM)
