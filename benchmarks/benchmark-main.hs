module Main where

import Criterion.Main

import BenchBitVector
import BenchSigned

main :: IO ()
main =
  defaultMain
  [
    bgroup "BitVector"
        [ addBench
        , negateBench
        , subBench
        , multBench
        , msbBench
        , msbBenchL
        , appendBench
        , appendBenchL
        , splitBench
        , splitBenchL
        ]
  , bgroup "Signed"
        [ fromIntegerBench
        ]
  ]
