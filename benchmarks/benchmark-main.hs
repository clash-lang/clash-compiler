module Main where

import Criterion.Main

import qualified BenchBitVector as BV
import qualified BenchSigned    as S

main :: IO ()
main =
  defaultMain
  [
    bgroup "BitVector"
        [ BV.addBench
        , BV.negateBench
        , BV.subBench
        , BV.multBench
        , BV.plusBench
        , BV.minusBench
        , BV.timesBench
        , BV.msbBench
        , BV.msbBenchL
        , BV.appendBench
        , BV.appendBenchL
        , BV.splitBench
        , BV.splitBenchL
        ]
  , bgroup "Signed"
        [ S.fromIntegerBench
        , S.addBench
        , S.negateBench
        , S.absBench
        , S.subBench
        , S.multBench
        , S.plusBench
        , S.minusBench
        , S.timesBench
        ]
  ]
