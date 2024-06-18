{-|
Copyright  :  (C) 2022, Google Inc.
License    :  BSD2 (see the file LICENSE)
Maintainer :  QBayLogic B.V. <devops@qbaylogic.com>
-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE TemplateHaskell #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module DynamicClocks where

import Clash.Explicit.Prelude
import Clash.Signal.Internal (Femtoseconds (..), tbDynamicClockGen, mapFemtoseconds)
import Clash.Explicit.Testbench

createDomain vSystem{vName="Static", vPeriod=1000}
createDomain vSystem{vName="Dynamic", vPeriod=1000}

counter :: KnownDomain dom => Clock dom -> Reset dom -> Enable dom -> Signal dom Int
counter clk rst ena = let counter0 = register clk rst ena 0 (counter0 + 1) in counter0
-- See: https://github.com/clash-lang/clash-compiler/pull/2511
{-# CLASH_OPAQUE counter #-}

topEntity ::
  Clock Static ->
  Clock Dynamic -> Reset Dynamic -> Enable Dynamic ->
  Signal Static Int
topEntity clkStatic clkDyn rstDyn enaDyn =
  unsafeSynchronizer clkDyn clkStatic (counter clkDyn rstDyn enaDyn)
-- See: https://github.com/clash-lang/clash-compiler/pull/2511
{-# CLASH_OPAQUE topEntity #-}

testBench :: Signal Static (Bit, Int)
testBench = bundle (boolToBit <$> doneStatic, actual)
 where
  actual = topEntity clkStatic clkDynamic rstDynamic enaDynamic

  expected =
    0  :> 1  :> 2  :> 3  :> 4  :> 5  :> 5  :> 6  :> 7  :> 7  :> 8  :> 9  :> 9  :>
    10 :> 10 :> 11 :> 11 :> 12 :> 12 :> 13 :> 13 :> 14 :> 14 :> 14 :> 15 :> 15 :>
    16 :> 16 :> 16 :> 17 :> 17 :> 18 :> 18 :> 18 :> 19 :> 19 :> 19 :> 20 :> 20 :>
    20 :> 21 :> 21 :> 21 :> 22 :> 22 :> 22 :> 23 :> 23 :> 23 :> 24 :> 24 :> 24 :>
    25 :> 25 :> 25 :> 25 :> 26 :> 26 :> 26 :> 27 :> 27 :> 27 :> 27 :> 28 :> 28 :>
    28 :> 29 :> 29 :> 29 :> 29 :> 30 :> 30 :> 30 :> 30 :> 31 :> 31 :> 31 :> 31 :>
    32 :> 32 :> 32 :> 32 :> 33 :> 33 :> 33 :> 33 :> 34 :> 34 :> 34 :> 34 :> 34 :>
    35 :> 35 :> 35 :> 35 :> 36 :> 36 :> 36 :> 36 :> 36 :> 37 :> 37 :> 37 :> 37 :>
    38 :> 38 :> 38 :> 38 :> 38 :> 39 :> 39 :> 39 :> 39 :> 39 :> 40 :> 40 :> 40 :>
    40 :> 40 :> 41 :> 41 :> 41 :> 41 :> 41 :> 42 :> 42 :> 42 :> 42 :> 42 :> 43 :>
    43 :> 43 :> 43 :> 43 :> 44 :> 44 :> 44 :> 44 :> 44 :> 45 :> 45 :> 45 :> 45 :>
    45 :> 45 :> 46 :> 46 :> 46 :> 46 :> 46 :> 47 :> 47 :> 47 :> 47 :> 47 :> 47 :>
    48 :> 48 :> 48 :> 48 :> 48 :> 49 :> 49 :> 49 :> 49 :> 49 :> 49 :> 50 :> 50 :>
    50 :> 50 :> 50 :> 50 :> 51 :> 51 :> 51 :> 51 :> 51 :> 51 :> 52 :> 52 :> 52 :>
    52 :> 52 :> 52 :> 53 :> 53 :> 53 :> 53 :> 53 :> 53 :> 54 :> 54 :> 54 :> 54 :>
    54 :> 54 :> 54 :> 55 :> 55 :> 55 :> 55 :> 55 :> 55 :> 56 :> 56 :> 56 :> 56 :>
    56 :> 56 :> 56 :> 57 :> 57 :> 57 :> 57 :> 57 :> 57 :> 58 :> 58 :> 58 :> 58 :>
    58 :> 58 :> 58 :> 59 :> 59 :> 59 :> 59 :> 59 :> 59 :> 59 :> 60 :> 60 :> 60 :>
    60 :> 60 :> 60 :> 60 :> 61 :> 61 :> 61 :> 61 :> 61 :> 61 :> 61 :> 62 :> 62 :>
    62 :> 62 :> 62 :> 62 :> 62 :> 63 :> 63 :> 63 :> 63 :> 63 :> 63 :> 63 :> 64 :>
    64 :> 64 :> 64 :> 64 :> 64 :> 64 :> 65 :> 65 :> 65 :> 65 :> 65 :> 65 :> 65 :>
    65 :> 66 :> 66 :> 66 :> 66 :> 66 :> 66 :> 66 :> 67 :> 67 :> 67 :> 67 :> 67 :>
    67 :> 67 :> 67 :> 68 :> 68 :> 68 :> 68 :> 68 :> 68 :> 68 :> 69 :> 69 :> 69 :>
    69 :> 69 :> 69 :> 69 :> 69 :> 70 :> 70 :> 70 :> 70 :> 70 :> 70 :> 70 :> 70 :>
    71 :> 71 :> 71 :> 71 :> 71 :> 71 :> 71 :> 71 :> 72 :> 72 :> 72 :> 72 :> 72 :>
    72 :> 72 :> 72 :> 73 :> 73 :> 73 :> 73 :> 73 :> 73 :> 73 :> 73 :> 74 :> 74 :>
    74 :> 74 :> 74 :> 74 :> 74 :> 74 :> 74 :> 75 :> 75 :> 75 :> 75 :> 75 :> 75 :>
    75 :> 75 :> 76 :> 76 :> 76 :> 76 :> 76 :> 76 :> 76 :> 76 :> 76 :> 77 :> 77 :>
    77 :> 77 :> 77 :> 77 :> 77 :> 77 :> 78 :> 78 :> 78 :> 78 :> 78 :> 78 :> 78 :>
    78 :> 78 :> 79 :> 79 :> 79 :> 79 :> 79 :> 79 :> 79 :> 79 :> 79 :> 80 :> 80 :>
    80 :> 80 :> 80 :> 80 :> 80 :> 80 :> 80 :> 81 :> 81 :> 81 :> 81 :> 81 :> 81 :>
    81 :> 81 :> 81 :> 82 :> 82 :> 82 :> 82 :> 82 :> 82 :> 82 :> 82 :> 82 :> 83 :>
    83 :> 83 :> 83 :> 83 :> 83 :> 83 :> 83 :> 83 :> 84 :> 84 :> 84 :> 84 :> 84 :>
    84 :> 84 :> 84 :> 84 :> 85 :> 85 :> 85 :> 85 :> 85 :> 85 :> 85 :> 85 :> 85 :>
    85 :> 86 :> 86 :> 86 :> 86 :> 86 :> 86 :> 86 :> 86 :> 86 :> 87 :> 87 :> 87 :>
    87 :> 87 :> 87 :> 87 :> 87 :> 87 :> 87 :> 88 :> 88 :> 88 :> 88 :> 88 :> 88 :>
    88 :> 88 :> 88 :> 89 :> 89 :> 89 :> 89 :> 89 :> 89 :> 89 :> 89 :> 89 :> 89 :>
    90 :> 90 :> 90 :> 90 :> 90 :> 90 :> 90 :> 90 :> 90 :> 90 :> 91 :> 91 :> 91 :>
    91 :> 91 :> 91 :> 91 :> 91 :> Nil

  doneStatic = outputVerifier' clkStatic rstStatic expected actual

  periods =
    stimuliGenerator clkDynamic rstDynamic $
      $(lift (iterate d500 (mapFemtoseconds (+100_000)) (Femtoseconds 1_000_000)))

  notDoneStatic  = not <$> doneStatic
  notDoneDynamic = unsafeSynchronizer clkStatic clkDynamic notDoneStatic
  clkDynamic     = tbDynamicClockGen @"Dynamic" periods notDoneDynamic
  clkStatic      = tbClockGen @"Static" notDoneStatic

  rstStatic  = resetGen @"Static"
  rstDynamic = resetGen @"Dynamic"

  enaDynamic = enableGen @"Dynamic"
-- See: https://github.com/clash-lang/clash-compiler/pull/2511
{-# CLASH_OPAQUE testBench #-}
