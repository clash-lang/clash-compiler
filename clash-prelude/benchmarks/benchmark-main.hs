module Main where

import Criterion.Main

import BenchRAM
import BenchBitVector
import BenchFixed
import BenchSigned
import BenchUnsigned

main :: IO ()
main =
  defaultMain
  [ ramBench
  , bitVectorBench
  , fixedBench
  , signedBench
  , unsignedBench
  ]
