module PackGHCNums where

import BitPackGHCNumsConstants

import Clash.Prelude
import Clash.Explicit.Testbench

import Data.Int
import Data.Word
import Foreign.C.Types

topEntity
  :: ( Int
     , Int8
     , Int16
     , Int32
     , Int64
     , Word
     , Word8
     , Word16
     , Word32
     , Word64
     , CUShort
     )
  -> BitVector 768
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
        ( ( i
          , i8
          , i16
          , i32
          , i64
          , w
          , w8
          , w16
          , w32
          , w64
          , cu
          ) :> Nil)

    expectedOutput =
      outputVerifier'
        clk
        rst
        -- This used to be one big tuple. We split it up so we can get away with
        -- -DMAX_TUPLE_SIZE=12, which considerably improves compilation speed of
        -- clash-prelude
        (pack ( ( $(lift (pack i))
                , $(lift (pack i8))
                , $(lift (pack i16))
                , $(lift (pack i32))
                , $(lift (pack i64))
                , $(lift (pack w))
                , $(lift (pack w8))
                , $(lift (pack w16))
                , $(lift (pack w32))
                , $(lift (pack w64))
                , $(lift (pack cu))
                )
              , ( $(bLit "0000000000000000000000000000000000000000000000000000000110100101")
                , $(bLit "10000101")
                , $(bLit "0010010111010101")
                , $(bLit "11111011100100101010010000001111")
                , $(bLit "1111111111111111111111111111111011100001000000111100001110110100")
                , $(bLit "0000000000000000000000000000000000000000000000010101011000000110")
                , $(bLit "11010101")
                , $(bLit "0010000110001001")
                , $(bLit "00000000000000000001010101101101")
                , $(bLit "0000000000000000000010110011000010011000010101101000001111010101")
                , $(bLit "1010010000001011")
                )
              ) :> Nil)

    done           = expectedOutput (topEntity <$> testInput)
    clk            = tbSystemClockGen (not <$> done)
    rst            = systemResetGen
