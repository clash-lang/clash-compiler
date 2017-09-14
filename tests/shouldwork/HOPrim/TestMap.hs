module TestMap where

import Clash.Explicit.Prelude

type System50 = Dom "system" 50
type Sig1 = Signal System50


type Word1 = Signed 18 -- SFixed 9 9
type RomFunction = (Unsigned 9 -> Word1)

coref :: RomFunction -> (Sig1 Word1, Sig1 Bool) -> Sig1 Word1
coref romf (bk, _) = (*) <$> bk <*> (pure $ romf 0)

mf
  :: (KnownNat n, KnownNat n1, KnownNat n2)
  => Clock System50 gated
  -> Reset System50 synchronous
  -> (Vec n RomFunction, SNat n1, SNat n2)
  -> (Sig1 Word1, Sig1 Bool)
  -> Sig1 Word1
mf clk rst (romFunctions, nI, coefN) (bk, dPulse) = (!!) <$> (bundle results) <*> curCore
  where

    g (mem, coreIndex) = initCore (mem, curCore .==. coreIndex)
    results = map g $ zip romFunctions (iterateI (+1) 0)

    initCore (romF, readSel) = (coref romF) (bk, readSel)

    read = dPulse

    cores = length results
    curCore :: Sig1 (Unsigned 1)
    curCore = regEn clk rst 0 read (curCore + 1)

romF1 :: RomFunction
romF1 addr = 0
romF2 :: RomFunction
romF2 addr = 1

topEntity
  :: Clock System50 Source
  -> Reset System50 Asynchronous
  -> (Sig1 Word1, Sig1 Bool)
  -> Sig1 Word1
topEntity clk rst = mf clk rst ((romF1 :> romF2 :> Nil), d2, d8)
{-# NOINLINE topEntity #-}

testBench :: Sig1 Bool
testBench = done
  where
    testInput      = stimuliGenerator clk50 rst50 ((5,True):>(4,False):>(2,True):>Nil)
    expectedOutput = outputVerifier   clk50 rst50 (0:>4:>2:>0:>2:>Nil)
    done           = expectedOutput (topEntity clk50 rst50 (unbundle testInput))
    clk50          = tbClockGen @System50 (not <$> done)
    rst50          = asyncResetGen @System50
