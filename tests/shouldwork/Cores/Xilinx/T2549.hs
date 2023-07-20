module T2549 where

import Clash.Prelude
import Clash.Cores.Xilinx.VIO
import GHC.Magic

topEntity :: Clock System -> Signal System Bit
topEntity c = hwSeqX probe v -- improper use of hwSeqX, the first argument of
                             -- hwSeqX should not have a function type. When
                             -- the first argument has a function type, it will
                             -- not be rendered.
  where
    probe :: Signal System Bit -> Signal System ()
    probe = vioProbe ("v1" :> "v2" :> Nil) Nil () c v
    {-# INLINE probe #-}

    v = pure high
