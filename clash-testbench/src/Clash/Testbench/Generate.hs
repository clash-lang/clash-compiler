module Clash.Testbench.Generate where

import Hedgehog
import Hedgehog.Gen

import Clash.Prelude (KnownDomain(..), BitPack(..), NFDataX)

import Clash.Testbench.Signal
import Clash.Testbench.Internal.ID
import Clash.Testbench.Internal.Signal hiding (TBSignal, TBClock, TBReset, TBEnable)
import Clash.Testbench.Internal.Monad

generate ::
  (NFDataX a, BitPack a, KnownDomain dom) =>
  Gen a -> TB (TBSignal dom a)
generate generator =
  mindSignal Generator
    { signalId     = NoID
    , signalCurVal = sample generator
    , signalPrint  = Nothing
    , ..
    }

 
