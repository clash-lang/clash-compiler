{-|
Copyright  :  (C) 2019, Myrtle Software Ltd
License    :  BSD2 (see the file LICENSE)
Maintainer :  Christiaan Baaij <christiaan.baaij@gmail.com>
-}

{-# LANGUAGE CPP #-}

module BitInteger where

import Clash.Prelude
import Clash.Explicit.Testbench

import Data.Bits

topEntity :: Int -> Integer
topEntity = bit
-- See: https://github.com/clash-lang/clash-compiler/pull/2511
{-# CLASH_OPAQUE topEntity #-}

testBench :: Signal System Bool
testBench = done
  where
    testInput =
      stimuliGenerator
        clk
        rst
        ( 0 :>
          1 :>
          2 :>
          3 :>
          4 :>
          5 :>
          Nil
        )

    expectedOutput =
      outputVerifier'
        clk
        rst
        ( 1 :>
          2 :>
          4 :>
          8 :>
          16 :>
          32 :>
          Nil
        )

    done = expectedOutput (topEntity <$> testInput)
    clk  = tbSystemClockGen (not <$> done)
    rst  = systemResetGen
