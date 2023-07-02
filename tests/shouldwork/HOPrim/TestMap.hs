{-# LANGUAGE CPP #-}

module TestMap where

import Clash.Explicit.Prelude
import Clash.Explicit.Testbench

createDomain vSystem{vName="System50", vPeriod=50000}

type Word1 = Signed 18 -- SFixed 9 9
type RomFunction = (Unsigned 9 -> Word1)

coref
  :: RomFunction
  -> (Signal System50 Word1, Signal System50 Bool)
  -> Signal System50 Word1
coref romf (bk, _) = (*) <$> bk <*> (pure $ romf 0)

mf
  :: (KnownNat n, KnownNat n1, KnownNat n2)
  => Clock System50
  -> Reset System50
  -> Enable System50
  -> (Vec n RomFunction, SNat n1, SNat n2)
  -> (Signal System50 Word1, Signal System50 Bool)
  -> Signal System50 Word1
mf clk rst en (romFunctions, nI, coefN) (bk, dPulse) = (!!) <$> (bundle results) <*> curCore
  where

    g (mem, coreIndex) = initCore (mem, curCore .==. coreIndex)
    results = map g $ zip romFunctions (iterateI (+1) 0)

    initCore (romF, readSel) = (coref romF) (bk, readSel)

    read = dPulse

    cores = length results
    curCore :: Signal System50 (Unsigned 1)
    curCore = regEn clk rst en 0 read (curCore + 1)

romF1 :: RomFunction
romF1 addr = 0
romF2 :: RomFunction
romF2 addr = 1

topEntity
  :: Clock System50
  -> Reset System50
  -> Enable System50
  -> (Signal System50 Word1, Signal System50 Bool)
  -> Signal System50 Word1
topEntity clk rst en = mf clk rst en ((romF1 :> romF2 :> Nil), d2, d8)
-- See: https://github.com/clash-lang/clash-compiler/pull/2511
{-# CLASH_OPAQUE topEntity #-}

testBench :: Signal System50 Bool
testBench = done
  where
    testInput      = stimuliGenerator clk50 rst50 ((5,True):>(4,False):>(2,True):>Nil)
    expectedOutput = outputVerifier'   clk50 rst50 (0:>4:>2:>0:>2:>Nil)
    done           = expectedOutput (topEntity clk50 rst50 enableGen (unbundle testInput))
    clk50          = tbClockGen @System50 (not <$> done)
    rst50          = resetGen @System50
