{-# LANGUAGE CPP #-}
{-# LANGUAGE UndecidableInstances #-}
module DTFold where

import Clash.Prelude
import Clash.Explicit.Testbench
import Data.Singletons hiding (type (+))
import Data.Proxy
import Data.Kind (Type)

data IIndex (f :: TyFun Nat Type) :: Type
type instance Apply IIndex l = Index ((2^l)+1)

populationCount :: (KnownNat k, KnownNat (2^k))
                => BitVector (2^k) -> Index ((2^k)+1)
populationCount bv = dtfold (Proxy :: Proxy IIndex)
                            fromIntegral
                            (\_ x y -> add x y)
                            (bv2v bv)

topEntity :: BitVector 16 -> Index 17
topEntity = populationCount
-- See: https://github.com/clash-lang/clash-compiler/pull/2511
{-# CLASH_OPAQUE topEntity #-}

testBench :: Signal System Bool
testBench = done
  where
    testInput      = stimuliGenerator clk rst $(listToVecTH ([0..20]::[BitVector 16]))
    expectedOutput = outputVerifier'   clk rst $(listToVecTH ([0,1,1,2,1,2,2,3,1,2,2,3,2,3,3,4,1,2,2,3,2]::[Index 17]))
    done           = expectedOutput (topEntity <$> testInput)
    clk            = tbSystemClockGen (not <$> done)
    rst            = systemResetGen
