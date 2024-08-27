module T2781
  ( fullMeshSwCcTest
  ) where

import Clash.Explicit.Prelude
import Clash.Cores.Xilinx.Ila (IlaConfig(..), Depth(..), ila, ilaConfig)

fullMeshHwTestDummy ::
  Clock System ->
  (  Signal System Bool
  ,  Vec 1 (Signal System Bool)
  )
fullMeshHwTestDummy sysClk =
  fincFdecIla `hwSeqX`
  ( pure False
  , repeat (pure True)
  )
 where
  fincFdecIla :: Signal System ()
  fincFdecIla = ila
    (ilaConfig ("trigger_0" :> Nil))
    sysClk
    (pure True :: Signal System Bool)

-- | Top entity for this test. See module documentation for more information.
fullMeshSwCcTest ::
  Clock System ->
  (Signal System Bool
  )
fullMeshSwCcTest sysClk = spiDone
 where
  (spiDone
    , ugnsStable
    ) = fullMeshHwTestDummy sysClk
{-# ANN fullMeshSwCcTest (defSyn "fullMeshSwCcTest") #-}
