{-|
Copyright   :  (C) 2019, Myrtle Software Ltd,
License     :  BSD2 (see the file LICENSE)
Maintainer  :  Christiaan Baaij <christiaan.baaij@gmail.com>

Synchronizer circuits for safe clock domain crossings
-}

{-# LANGUAGE TypeFamilies #-}

module Clash.Prelude.Synchronizer
  ( -- * Bit-synchronizers
    dualFlipFlopSynchronizer
    -- * Word-synchronizers
  , asyncFIFOSynchronizer
  ) where

import qualified Clash.Explicit.Synchronizer as E
import           Clash.Promoted.Nat          (SNat)
import           Clash.Signal
  (HiddenClockResetEnable, HiddenClock, Signal, hasClock, hasReset, hasEnable)
import           Clash.XException            (NFDataX)
import           GHC.TypeLits                (type (<=))
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
     , HiddenClock dom1
     , HiddenClockResetEnable dom2
     )
  => a
  -- ^ Initial value of the two synchronization registers
  -> Signal dom1 a
  -- ^ Incoming data
  -> Signal dom2 a
  -- ^ Outgoing, synchronized, data
dualFlipFlopSynchronizer =
  E.dualFlipFlopSynchronizer hasClock hasClock hasReset hasEnable

-- | Synchronizer implemented as a FIFO around an asynchronous RAM. Based on the
-- design described in "Clash.Tutorial#multiclock", which is itself based on the
-- design described in <http://www.sunburst-design.com/papers/CummingsSNUG2002SJ_FIFO1.pdf>.
--
-- __NB__: This synchronizer can be used for __word__-synchronization.
asyncFIFOSynchronizer
  :: ( HiddenClockResetEnable rdom
     , HiddenClockResetEnable wdom
     , 2 <= addrSize
     , NFDataX a)
  => SNat addrSize
  -- ^ Size of the internally used addresses, the  FIFO contains @2^addrSize@
  -- elements.
  -> Signal rdom Bool
  -- ^ Read request
  -> Signal wdom (Maybe a)
  -- ^ Element to insert
  -> (Signal rdom a, Signal rdom Bool, Signal wdom Bool)
  -- ^ (Oldest element in the FIFO, @empty@ flag, @full@ flag)
asyncFIFOSynchronizer addrSize =
  E.asyncFIFOSynchronizer
    addrSize
    hasClock  -- wdom
    hasClock  -- rdom
    hasReset  -- wdom
    hasReset  -- rdom
    hasEnable  -- wdom
    hasEnable  -- rdom
