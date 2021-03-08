module T1669_DEC where
import Clash.Prelude
import Clash.Explicit.Testbench

data AB = A | B

{-# NOINLINE topEntity #-}
topEntity :: AB -> Int -> Int
topEntity ab x = case ab of
    A -> f 3 x 0 1 2 3 4 5 6 7 8  9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 27 28 29 30 31 32 33 34 35 36 37 38 39 40 41 42 43 44 45 46 47 48 49 50 51 52 53 54 55 56 57 58 59 60 61 62 63 64
    B -> f x x 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 27 28 29 30 31 32 33 34 35 36 37 38 39 40 41 42 43 44 45 46 47 48 49 50 51 52 53 54 55 56 57 58 59 60 61 62 63 64 0

{-# NOINLINE f #-}
f z0 z1 z2 z3 z4 z5 z6 z7 z8 z9 z10 z11 z12 z13 z14 z15 z16 z17 z18 z19 z20 z21 z22 z23 z24 z25 z26 z27 z28 z29 z30 z31 z32 z33 z34 z35 z36 z37 z38 z39 z40 z41 z42 z43 z44 z45 z46 z47 z48 z49 z50 z51 z52 z53 z54 z55 z56 z57 z58 z59 z60 z61 z62 z63 z64 z65 z66
  = foldl (\b (a,i) -> b - a*i) 0 $ zip args ixs
 where
   args = z0:>z1:>z2:>z3:>z4:>z5:>z6:>z7:>z8:>z9:>z10:>z11:>z12:>z13:>z14:>z15:>z16:>z17:>z18:>z19:>z20:>z21:>z22:>z23:>z24:>z25:>z26:>z27:>z28:>z29:>z30:>z31:>z32:>z33:>z34:>z35:>z36:>z37:>z38:>z39:>z40:>z41:>z42:>z43:>z44:>z45:>z46:>z47:>z48:>z49:>z50:>z51:>z52:>z53:>z54:>z55:>z56:>z57:>z58:>z59:>z60:>z61:>z62:>z63:>z64:>z65:>z66:>Nil
   ixs = generateI (+1) 0

testBench :: Signal System Bool
testBench = done
  where
    testInput      = stimuliGenerator clk rst ((A,1669):>(B,42):>Nil)
    expectedOutput = outputVerifier' clk rst (-99021 :> -93726 :> Nil)
    done           = expectedOutput (fmap (uncurry topEntity) testInput)
    clk            = tbSystemClockGen (not <$> done)
    rst            = systemResetGen
