{-# OPTIONS_GHC -fconstraint-solver-iterations=10 #-}
{-# OPTIONS_GHC -Wno-orphans #-}

{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE ViewPatterns #-}

module TdpBlockRam where

import Clash.Cores.Xilinx.BlockRam (tdpbram)
import Clash.Explicit.Prelude
import Clash.Explicit.Testbench

createDomain vXilinxSystem{vName="A", vPeriod=hzToPeriod 10e6 }
createDomain vXilinxSystem{vName="B", vPeriod=hzToPeriod 7e6 }

topEntity ::
  Clock A ->
  Clock B ->
  Signal A (Bool, Index 500, BitVector 2, Unsigned 16) ->
  Signal B (Bool, Index 500, BitVector 2, Unsigned 16) ->
  (Signal A (Unsigned 16), Signal B (Unsigned 16))
topEntity
  clkA clkB
  (unbundle -> (enA, addrA, byteEnaA, datA))
  (unbundle -> (enB, addrB, byteEnaB, datB)) =
  tdpbram
    clkA (toEnable enA) addrA byteEnaA datA
    clkB (toEnable enB) addrB byteEnaB datB
-- See: https://github.com/clash-lang/clash-compiler/pull/2511
{-# CLASH_OPAQUE topEntity #-}

noRstA :: Reset A
noRstA = unsafeFromActiveHigh (pure False)

noRstB :: Reset B
noRstB = unsafeFromActiveHigh (pure False)

tb ::
  ( KnownNat n0, KnownNat n1, KnownNat n2, KnownNat n3
  , 1 <= n0, 1 <= n1, 1 <= n2, 1 <= n3 ) =>
  -- | Input on port A
  Vec n0 (Bool, Index 500, BitVector 2, Unsigned 16)->
  -- | Expected data from port A
  Vec n1 (Unsigned 16) ->
  -- | Input on port B
  Vec n2 (Bool, Index 500, BitVector 2, Unsigned 16) ->
  -- | Expected data from port B
  Vec n3 (Unsigned 16) ->
  Signal A Bool
tb inputA expectedA inputB expectedB =
  strictAnd <$> doneA <*> (unsafeSynchronizer clkB clkA doneB)
 where
  strictAnd !a !b = (&&) a b

  -- topEntity output
  (actualA0, actualB0) =
    topEntity
      clkA clkB
      (stimuliGenerator clkA noRstA inputA)
      (stimuliGenerator clkB noRstB inputB)

  actualA1 = ignoreFor clkA noRstA enableGen d1 0 actualA0
  actualB1 = ignoreFor clkB noRstB enableGen d1 0 actualB0

  -- Verification
  outputVerifierA = outputVerifierWith
   (\clk rst -> assert clk rst "outputVerifier Port A")
  outputVerifierB = outputVerifierWith
   (\clk rst -> assert clk rst "outputVerifier Port B")

  doneA  = outputVerifierA clkA clkA noRstA expectedA actualA1
  doneB  = outputVerifierB clkB clkB noRstB expectedB actualB1

  -- Testbench clocks
  clkA :: Clock A
  clkA = tbClockGen (not <$> doneA)
  clkB :: Clock B
  clkB = tbClockGen (not <$> doneB)

-- See: https://github.com/clash-lang/clash-compiler/pull/2511
{-# CLASH_OPAQUE normalWritesTB #-}
{-# ANN normalWritesTB (TestBench 'topEntity) #-}
-- | Test bench doing some (non-overlapping) writes and reads on two ports, either
-- with the byte enable fully set, or fully unset.
normalWritesTB :: Signal A Bool
normalWritesTB = tb inputA expectedA inputB expectedB
 where
  -- Note that the initial value coming from the blockram is undefined, but we
  -- mask it using 'ignoreFor'.
  initVal = 0

  expectedA =
    (initVal :> 55 :> 66 :> 55 :> 66 :> Nil) ++
    (repeat @10 66) ++
    (77 :> 88 :> Nil)
  expectedB =
    (initVal :> 77 :> 88 :> 77 :> 88 :> Nil) ++
    (repeat @10 88) ++
    (55 :> 66 :> Nil)

  doWrite = maxBound
  noWrite = 0
  noOp = (False, 0, 0, 0)

  inputA =
    (  (True, 0, doWrite, 55)
    :> (True, 1, doWrite, 66)
    :> (True, 0, noWrite, 0)
    :> (True, 1, noWrite, 0)
    :> Nil
    ) ++ repeat @10 noOp ++
    (
       (True, 2, noWrite, 0)
    :> (True, 3, noWrite, 0)
    :> Nil
    )
    ++ repeat @10 noOp

  inputB =
    (  (True, 2, doWrite, 77)
    :> (True, 3, doWrite, 88)
    :> (True, 2, noWrite, 0)
    :> (True, 3, noWrite, 0)
    :> Nil
    ) ++ repeat @10 noOp ++
    (
       (True, 0, noWrite, 0)
    :> (True, 1, noWrite, 0)
    :> Nil
    ) ++ repeat @10 noOp

-- See: https://github.com/clash-lang/clash-compiler/pull/2511
{-# CLASH_OPAQUE writeEnableWritesTB #-}
{-# ANN writeEnableWritesTB (TestBench 'topEntity) #-}
-- | Test bench doing some (non-overlapping) writes and reads on two ports, with
-- varying byte enables.
writeEnableWritesTB :: Signal A Bool
writeEnableWritesTB = tb inputA expectedA inputB expectedB
 where
  -- Note that the initial value coming from the blockram is undefined, but we
  -- mask it using 'ignoreFor'.
  initVal = 0

  expectedA =
       initVal
    :> 0
    :> 0
    :> 0
    :> 0

    :> 0
    :> 0x00AA
    :> 0xAA00
    :> 0xAAAA

    :> 0
    :> 0x00AA
    :> 0xAA00
    :> 0xAAAA
    :> Nil

  expectedB =
       initVal
    :> 0
    :> 0
    :> 0
    :> 0

    :> 0
    :> 0x00AA
    :> 0xAA00
    :> 0xAAAA

    :> 0
    :> 0x00AA
    :> 0xAA00
    :> 0xAAAA
    :> Nil

  noWrite = 0

  inputA =
    (  (True, 0, 0b11,    0     )
    :> (True, 1, 0b11,    0     )
    :> (True, 2, 0b11,    0     )
    :> (True, 3, 0b11,    0     )

    :> (True, 0, 0b00,    0xAAAA)
    :> (True, 1, 0b01,    0xAAAA)
    :> (True, 2, 0b10,    0xAAAA)
    :> (True, 3, 0b11,    0xAAAA)

    :> (True, 0, noWrite, 0     )
    :> (True, 1, noWrite, 0     )
    :> (True, 2, noWrite, 0     )
    :> (True, 3, noWrite, 0     )
    :> Nil
    )

  inputB =
    (  (True, 4, 0b11,    0     )
    :> (True, 5, 0b11,    0     )
    :> (True, 6, 0b11,    0     )
    :> (True, 7, 0b11,    0     )
    :> (True, 4, 0b00,    0xAAAA)
    :> (True, 5, 0b01,    0xAAAA)
    :> (True, 6, 0b10,    0xAAAA)
    :> (True, 7, 0b11,    0xAAAA)
    :> (True, 4, noWrite, 0     )
    :> (True, 5, noWrite, 0     )
    :> (True, 6, noWrite, 0     )
    :> (True, 7, noWrite, 0     )
    :> Nil
    )
