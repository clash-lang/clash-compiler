module Clash.Testbench.Internal.Auto where

import Clash.Testbench.Signal
import Clash.Testbench.Internal.ID
import Clash.Testbench.Internal.Signal hiding (TBClock, TBReset, TBEnable)
import qualified Clash.Testbench.Internal.Signal as Internal

import Clash.Prelude
  ( KnownDomain(..), SDomainConfiguration(..)
  , clockGen, resetGen, enableGen, ssymbolToString
  )

-- | Signals that are implicitly available inside 'Clash.Testbench.Simulate.TB' and can be
--   driven by the simulator automatically.
class AutoTB a where
  auto :: a

instance KnownDomain dom => AutoTB (TBClock dom) where
  auto = case knownDomain @dom of
    SDomainConfiguration domainName _ _ _ _ _ ->
      Internal.TBClock
        { clock       = clockGen
        , clockId     = ClockID $ AutoDom $ ssymbolToString domainName
        , clockSource = return clockGen
        }

instance KnownDomain dom => AutoTB (TBReset dom) where
  auto = case knownDomain @dom of
    SDomainConfiguration domainName _ _ _ _ _ ->
      Internal.TBReset
        { reset       = resetGen
        , resetId     = ResetID $ AutoDom $ ssymbolToString domainName
        , resetCurVal = return False
        }

instance KnownDomain dom => AutoTB (TBEnable dom) where
  auto = case knownDomain @dom of
    SDomainConfiguration domainName _ _ _ _ _ ->
      Internal.TBEnable
        { enable       = enableGen
        , enableId     = EnableID $ AutoDom $ ssymbolToString domainName
        , enableCurVal = return True
        }
