module Main where

import Criterion.Main

import BenchRAM
import BenchBitVector
import BenchFixed
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
