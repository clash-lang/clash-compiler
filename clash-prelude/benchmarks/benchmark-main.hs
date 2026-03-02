module Main where

import Criterion.Main

import BenchBitVector
import BenchFixed
import BenchRAM
import BenchSigned
import BenchUnsigned
import BenchVector

main :: IO ()
main =
  defaultMain
    [ ramBench
    , bitVectorBench
    , fixedBench
    , signedBench
    , unsignedBench
    , vectorBench
    ]
