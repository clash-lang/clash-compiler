-- | Block RAMs should neither read nor write while disabled
module ReadEnable where

import Clash.Explicit.Prelude
import Clash.Explicit.Testbench

type Addr = Index 4
type Payload = BitVector 8

-- | Enable, read address, and write
type Input = (Bool, Addr, Maybe (Addr, Payload))

-- | Outputs of 'blockRam', 'blockRamU', and 'blockRam1'
type Output = (Payload, Payload, Payload)

topEntity ::
  Clock System ->
  Signal System Input ->
  Signal System Output
topEntity clk inp =
  bundle
    ( blockRam clk en (replicate d4 0) rd wr
    , blockRamU clk noReset en NoClearOnReset d4 rd wr
    , blockRam1 clk noReset en NoClearOnReset d4 0 rd wr
    )
 where
  (enB, rd, wr) = unbundle inp
  en = toEnable enB
{-# OPAQUE topEntity #-}

testBench :: Signal System Bool
testBench = done
 where
  testInput = stimuliGenerator clk noReset inputs
  inputs :: Vec 6 Input
  inputs =
       (True, 0, Just (0, 1))
    :> (True, 0, Just (1, 2))
       -- Disabled: keep the output of the previous read
    :> (False, 1, Nothing)
    :> (True, 1, Nothing)
       -- Disabled: neither write, nor change the output
    :> (False, 0, Just (0, 3))
    :> (True, 0, Nothing)
    :> Nil

  r :: Payload -> Output
  r x = (x, x, x)

  -- The first two outputs are undefined for 'blockRamU'
  expected :: Vec 7 Output
  expected = r 0 :> r 0 :> r 1 :> r 1 :> r 2 :> r 2 :> r 1 :> Nil

  done =
    outputVerifier' clk noReset expected
      (ignoreFor clk noReset enableGen d2 (r 0) (topEntity clk testInput))
  clk = tbSystemClockGen (not <$> done)
{-# OPAQUE testBench #-}
