module Main where

import BenchBitVector
import BenchFixed
import BenchRAM
import BenchSigned
import BenchUnsigned
import BenchVector
import Criterion.Main

main :: IO ()
main =
  defaultMain
    [ ramBench,
      bitVectorBench,
      fixedBench,
      signedBench,
      unsignedBench,
      vectorBench
    ]
