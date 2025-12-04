{-# LANGUAGE CPP #-}

module Resize3 where

import Clash.Prelude
import Clash.Explicit.Testbench

topEntity :: Signed 8
topEntity = unpack (resize (pack (-2 :: Signed 4)))
{-# OPAQUE topEntity #-}

testBench :: Signal System Bool
testBench = done
  where
    expectedOutput =
      outputVerifier'
        clk
        rst
        (    14
          :> $(lift (unpack (resize (pack (-2 :: Signed 4))) :: Signed 8))
          :>  Nil)
    done           = expectedOutput (pure topEntity)
    clk            = tbSystemClockGen (not <$> done)
    rst            = systemResetGen
