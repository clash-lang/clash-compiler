{-# LANGUAGE LambdaCase #-}

module ResetSynchronizer where

import qualified Prelude as P

import Clash.Explicit.Prelude
import Clash.Explicit.Testbench
import Data.Proxy

data ResetCount = RRRRRRR | Count (Index 8)
  deriving (Generic, Show, Eq, ShowX, NFDataX)

succResetCount :: ResetCount -> ResetCount
succResetCount RRRRRRR = Count 0
succResetCount (Count n) = Count (succ n)

testReset ::
  ( KnownDomain circuitDom
  , KnownDomain testDom ) =>
  Clock testDom ->
  Reset testDom ->
  Clock circuitDom ->
  Reset circuitDom
testReset tbClk tbRst cClk =
    unsafeFromHighPolarity
  $ unsafeSynchronizer tbClk cClk
  $ stimuliGenerator tbClk tbRst
    ( True

      -- Reset synchronizer introduces a delay of two, but lets asynchronous
      -- asserts through untouched. This means that for 'topEntity' we'll see
      -- [R, R, R, R] for these cycles. Where the first two zeroes are caused by
      -- 'resetSynchronizer's induced latency, the next zero by the first cycle
      -- not in reset, and the last zero because we're in reset again (async
      -- behavior).
      --
      -- We should be able to see a difference between asynchronous and synchronous
      -- resets here: the counter is driven by a register whose synchronicity
      -- is imposed by the 'KnownDomain' constraint. Synchronous counters will
      -- only see the reset asserted on the /next/ cycle, hence outputting
      -- [R, R, R, 0] instead.
   :> False :> False :> False :> True

      -- Similar to ^, but now we're out of of reset one cycle longer so we should
      -- start seeing [R, R, R, 0, R] for asynchronous counters, and
      -- [R, R, R, 0, 1] for synchronous ones.
   :> False :> False :> False :>  False :> True
   :> replicate d20 False )

polyTopEntity
  :: KnownDomain dom
  => Clock dom
  -> Reset dom
  -> Signal dom ResetCount
polyTopEntity clk asyncRst = counter
 where
  ena = enableGen
  counter = register clk rst ena RRRRRRR (fmap succResetCount counter)
  rst = resetSynchronizer clk asyncRst ena

topEntity :: Clock System -> Reset System -> Signal System ResetCount
topEntity = polyTopEntity @System

-- | Doing this case inline trips GHC 8.4 due to dead code. We sometimes
-- want to run our whole testsuite with a different TopDomain domain though, so
-- it's not _really_ dead code.
rOr :: Index 8 -> SResetKind dom -> ResetCount
rOr n = \case {SAsynchronous -> RRRRRRR; SSynchronous -> Count n}

polyTestBench ::
  forall circuitDom.
  KnownDomain circuitDom =>
  Proxy (circuitDom :: Domain) ->
  (Signal System Bool, [ResetCount])
polyTestBench Proxy = (done, sampleN 20 (polyTopEntity cClk cRst))
 where
  rKind = resetKind @circuitDom
  expectedOutput = outputVerifier tbClk tbRst
    ( RRRRRRR :> RRRRRRR :> RRRRRRR :> RRRRRRR :> rOr 0 rKind
   :> RRRRRRR :> RRRRRRR :> RRRRRRR :> Count 0 :> rOr 1 rKind
   :> RRRRRRR :> RRRRRRR :> RRRRRRR :> Count 0 :> Count 1 :> Count 2
   :> Nil )
  done = expectedOutput (polyTopEntity cClk cRst)
  (tbClk, cClk) = biTbClockGen @System @circuitDom (not <$> done)
  (tbRst, cRst) = (resetGen, testReset tbClk tbRst cClk)

testBench :: Signal System Bool
testBench = fst (polyTestBench (Proxy @System))
