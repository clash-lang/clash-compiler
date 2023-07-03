{-# LANGUAGE CPP #-}
{-# LANGUAGE LambdaCase #-}

module ResetSynchronizer where

import Clash.Explicit.Prelude
import Clash.Explicit.Testbench

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

      -- Asynchronous resets:
      --
      --   Resets are asserted asynchronously and deasserted synchronously with
      --   a delay of two cycles. This means that for 'topEntity' we'll see
      --   [R, R, R, R] for the first batch of reset cycles. The first two Rs
      --   are caused by  'resetSynchronizer's induced latency, the next zero
      --   by the first cycle not in reset, and the last zero because we're in
      --   reset again (async assertion behavior).
      --
      --   The second batch of resets is similar to ^, but the reset is held
      --   a cycle longer so we should be able to see a count output.
      --
      -- Synchronous resets:
      --
      --   Synchronous resets are simply delayed for two cycles. Hence, we
      --   should count up to the number of deasserted cycles.
      --
   :> False :> False :> False :> True
   :> False :> False :> False :>  False :> True
   :> replicate d20 False )

polyTopEntity
  :: KnownDomain dom
  => Clock dom
  -> Reset dom
  -> Signal dom ResetCount
polyTopEntity clk asyncRst = counter
 where
  counter = register clk rst enableGen RRRRRRR (fmap succResetCount counter)
  rst = resetSynchronizer clk asyncRst

topEntityAsync :: Clock System -> Reset System -> Signal System ResetCount
topEntityAsync = polyTopEntity @System
-- See: https://github.com/clash-lang/clash-compiler/pull/2511
{-# CLASH_OPAQUE topEntityAsync #-}
{-# ANN topEntityAsync (defSyn "topEntityAsync") #-}

topEntitySync :: Clock XilinxSystem -> Reset XilinxSystem -> Signal XilinxSystem ResetCount
topEntitySync = polyTopEntity @XilinxSystem
-- See: https://github.com/clash-lang/clash-compiler/pull/2511
{-# CLASH_OPAQUE topEntitySync #-}
{-# ANN topEntitySync (defSyn "topEntitySync") #-}

-- | Doing this case inline trips GHC 8.4 due to dead code. We sometimes
-- want to run our whole testsuite with a different TopDomain domain though, so
-- it's not _really_ dead code.
rOr :: Index 8 -> SResetKind dom -> ResetCount
rOr n = \case {SAsynchronous -> RRRRRRR; SSynchronous -> Count n}

polyTestBench ::
  forall circuitDom.
  KnownDomain circuitDom =>
  (Clock circuitDom -> Reset circuitDom -> Signal circuitDom ResetCount) ->
  (Signal System Bool, [ResetCount])
polyTestBench top = (done, sampleN 20 (top cClk cRst))
 where
  rOr' = flip rOr (resetKind @circuitDom)
  expectedOutput = outputVerifier tbClk cClk tbRst
    ( RRRRRRR :> RRRRRRR :> RRRRRRR :> RRRRRRR :> rOr' 0
   :> rOr' 1  :> rOr' 2  :> RRRRRRR :> Count 0 :> rOr' 1
   :> rOr' 2  :> rOr' 3  :> RRRRRRR :> Count 0 :> Count 1 :> Count 2
   :> Nil )
  done = expectedOutput (top cClk cRst)
  (tbClk, cClk) = biTbClockGen @System @circuitDom (not <$> done)
  (tbRst, cRst) = (resetGen, testReset tbClk tbRst cClk)

testBenchAsync :: Signal System Bool
testBenchAsync = fst (polyTestBench topEntityAsync)
-- See: https://github.com/clash-lang/clash-compiler/pull/2511
{-# CLASH_OPAQUE testBenchAsync #-}
{-# ANN testBenchAsync (TestBench 'topEntityAsync) #-}

testBenchSync :: Signal System Bool
testBenchSync = fst (polyTestBench topEntitySync)
-- See: https://github.com/clash-lang/clash-compiler/pull/2511
{-# CLASH_OPAQUE testBenchSync #-}
{-# ANN testBenchSync (TestBench 'topEntitySync) #-}
