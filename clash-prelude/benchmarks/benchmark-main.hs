module Main where

import Criterion.Main

import qualified BenchRAM       as RAM
import qualified BenchBitVector as BV
import qualified BenchFixed     as F
import qualified BenchSigned    as S

main :: IO ()
main =
  defaultMain
  [
    bgroup "RAMs"
        [ RAM.asyncRamBench
        , RAM.asyncRomBench
        , RAM.blockRamBench
        , RAM.blockRamROBench
        , RAM.romBench
        ]
  , bgroup "BitVector"
        [ BV.addBench
        , BV.negateBench
        , BV.subBench
        , BV.multBench
        , BV.plusBench
        , BV.minusBench
        , BV.timesBench
        , BV.boundedAddBench
        , BV.boundedSubBench
        , BV.boundedMulBench
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
        , S.packBench
        , S.unpackBench
        ]
  , bgroup "Fixed"
        [ F.fromRationalBench
        , F.addBench
        , F.subBench
        , F.multBench
        , F.multBench_wrap
        ]
  ]
