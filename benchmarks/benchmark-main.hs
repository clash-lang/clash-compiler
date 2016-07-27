module Main where

import Criterion.Main

import BenchBitVector

main :: IO ()
main =
  defaultMain
  [
    bgroup "BitVector"
        [ msbBench
        , msbBenchL
        , appendBench
        , appendBenchL
        , splitBench
        , splitBenchL
        ]
  ]
