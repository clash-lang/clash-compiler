{-|
Copyright  :  (C) 2019, Myrtle Software Ltd
License    :  BSD2 (see the file LICENSE)
Maintainer :  Christiaan Baaij <christiaan.baaij@gmail.com>
-}

{-# LANGUAGE CPP #-}

module Signum where

import Clash.Prelude
import Clash.Explicit.Testbench

topEntity
  :: (Integer, Int, Index 5, Signed 5)
  -> (Integer, Int, Index 5, Signed 5)
topEntity (a, b, c, d) = (signum a, signum b, signum c, signum d)
-- See: https://github.com/clash-lang/clash-compiler/pull/2511
{-# CLASH_OPAQUE topEntity #-}

testBench :: Signal System Bool
testBench = done
  where
    testInput =
      stimuliGenerator
        clk
        rst
        ( (0, 0, 0, 0) :>
          (-2, -2, 0, -2) :>
          (2, 2, 2, 2) :>
          Nil
        )

    expectedOutput =
      outputVerifier'
        clk
        rst
        ( (0, 0, 0, 0) :>
          (-1, -1, 0, -1) :>
          (1, 1, 1, 1) :>
          Nil
        )

    done = expectedOutput (topEntity <$> testInput)
    clk  = tbSystemClockGen (not <$> done)
    rst  = systemResetGen
