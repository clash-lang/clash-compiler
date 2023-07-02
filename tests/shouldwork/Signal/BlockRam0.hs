{-# LANGUAGE CPP #-}

module BlockRam0 where

import Clash.Prelude
import Clash.Explicit.Testbench
import qualified Clash.Explicit.Prelude as Explicit

zeroAt0
  :: HiddenClockResetEnable dom
  => Signal dom (Unsigned 10)
  -> Signal dom (Unsigned 10)
zeroAt0 a = mux en a 0
  where
    en =
      delay False $
      delay False $
      (pure True)

topEntity
  :: Clock System
  -> Reset System
  -> Enable System
  -> Signal System (Index 1024)
  -> Signal System (Maybe (Index 1024, Unsigned 10))
  -> Signal System (Unsigned 10)
topEntity = exposeClockResetEnable go where

  go rd wr = zeroAt0 dout where
    dout =
      blockRamU
        ClearOnReset
        (SNat @1024)
        ((+22) . unpack . pack :: Index 1024 -> Unsigned 10)
        rd
        wr
-- See: https://github.com/clash-lang/clash-compiler/pull/2511
{-# CLASH_OPAQUE topEntity #-}

testBench :: Signal System Bool
testBench = done
  where
    (rst0, rd, wr) =
      unbundle $ stimuliGenerator
        clk rst
        (    (True,  0, Nothing)

          -- Write some values
          :> (False, 0, Just (0, 8))
          :> (False, 0, Just (1, 9))
          :> (False, 0, Just (2, 10))
          :> (False, 0, Just (3, 11))

          -- Read written values
          :> (False, 0, Nothing)
          :> (False, 1, Nothing)
          :> (False, 2, Nothing)
          :> (False, 3, Nothing)

          -- Reset for two cycles
          :> (True, 0, Nothing)
          :> (True, 0, Nothing)

          -- Check whether first two values were reset
          :> (False, 0, Nothing)
          :> (False, 1, Nothing)
          :> (False, 2, Nothing)
          :> (False, 3, Nothing)

          :> Nil

          )

    expectedOutput =
      outputVerifier' clk rst
        (    0 :> 22

          -- Read address zero while writing data
          :> 22
          :> 22
          :> 22
          :> 22

          -- Read written values back from BRAM
          :> 22
          :> 9
          :> 10
          :> 11

          -- Reset for two cycles
          :> 22
          :> 22

          -- Check whether reset worked
          :> 22
          :> 23
          :> 10
          :> 11

          :> Nil)

    done           = expectedOutput (topEntity clk (unsafeFromHighPolarity rst0) enableGen rd wr)
    clk            = tbSystemClockGen (not <$> done)
    rst            = systemResetGen
