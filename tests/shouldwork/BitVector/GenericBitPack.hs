{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE UndecidableInstances #-}

module GenericBitPack where

import GenericBitPackTypes
import Clash.Prelude
import Clash.Explicit.Testbench

topEntity
  :: ( FooProduct U1 U2
     , FooSum
     , FooSum
     , FooSum
     , FooSum
     , FooSP1 U1 U2
     , FooSP1 U1 U2
     , FooSP2 U1 U2
     , FooSP2 U1 U2
     , FooSP2 U1 U2
     , (U1, U2)
     )
  -> BitVector 152
topEntity (a, b, c, d, e, f, g, h, i, j, k) =
  packed ++# packed
 where
  packed =
    pack $
      ( pack a
      , pack b
      , pack c
      , pack d
      , pack e
      , pack f
      , pack g
      , pack h
      , pack i
      , pack j
      , pack k
      )
{-# NOINLINE topEntity #-}

testBench :: Signal System Bool
testBench = done
  where
    testInput =
      stimuliGenerator
        clk
        rst
        ( ( aT
          , bT
          , cT
          , dT
          , eT
          , fT
          , gT
          , hT
          , iT
          , jT
          , kT
          ) :> Nil)

    expectedOutput =
      outputVerifierBitVector'
        clk
        rst
        -- This used to be one big tuple. We split it up so we can get away with
        -- -DMAX_TUPLE_SIZE=12, which considerably improves compilation speed of
        -- clash-prelude
        (pack ( ( $(lift (pack aT))
                , $(lift (pack bT))
                , $(lift (pack cT))
                , $(lift (pack dT))
                , $(lift (pack eT))
                , $(lift (pack fT))
                , $(lift (pack gT))
                , $(lift (pack hT))
                , $(lift (pack iT))
                , $(lift (pack jT))
                , $(lift (pack kT))
                )
              , ( $(bLit "00100010")
                , $(bLit "000")
                , $(bLit "001")
                , $(bLit "010")
                , $(bLit "110")
                , $(bLit "000100010")
                , $(bLit "100001010")
                , $(bLit "0001000001")
                , $(bLit "01010.....")
                , $(bLit "1000001...")
                , $(bLit "00100010")
                )
              ) :> Nil)

    done           = expectedOutput (topEntity <$> testInput)
    clk            = tbSystemClockGen (not <$> done)
    rst            = systemResetGen
