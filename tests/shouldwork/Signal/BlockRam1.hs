{-# LANGUAGE CPP #-}

module BlockRam1 where

import Clash.Prelude
import Clash.Explicit.Testbench
import qualified Clash.Explicit.Prelude as Explicit

zeroAt0
  :: HiddenClockResetEnable dom
  => Signal dom (Unsigned 8)
  -> Signal dom (Unsigned 8)
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
  -> Signal System (Maybe (Index 1024, Unsigned 8))
  -> Signal System (Unsigned 8)
topEntity = exposeClockResetEnable go where

  go rd wr = zeroAt0 dout where
    dout =
      blockRam1
        (ClearOnReset ())
        (SNat @1024)
        (3 :: Unsigned 8)
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

          -- Confirm initial values
          :> (False, 0, Nothing)
          :> (False, 1, Nothing)
          :> (False, 2, Nothing)
          :> (False, 3, Nothing)

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
        (    0 :> 3

          -- Initial values should be all threes
          :> 3
          :> 3
          :> 3
          :> 3

          -- Read address zero while writing data
          :> 3
          :> 8
          :> 8
          :> 8

          -- Read written values back from BRAM
          :> 8
          :> 9
          :> 10
          :> 11

          -- Reset for two cycles
          :> 8
          :> 3

          -- Check whether reset worked
          :> 3
          :> 3
          :> 10
          :> 11

          :> Nil)

    done           = expectedOutput (topEntity clk (unsafeFromActiveHigh rst0) enableGen rd wr)
    clk            = tbSystemClockGen (not <$> done)
    rst            = systemResetGen
