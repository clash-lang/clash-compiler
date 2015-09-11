module TestMap
  where

import CLaSH.Prelude
import CLaSH.Prelude.Explicit

type System50 = Clk "system" 72
system50 :: SClock System50
system50 = sclock
type Sig1 = Signal' System50
regEn1 = regEn' system50
bundle1 = bundle' system50

type Word1 = Signed 18 -- SFixed 9 9
type RomFunction = (Unsigned 9 -> Word1)

coref :: RomFunction -> (Sig1 Word1, Sig1 Bool) -> Sig1 Word1
coref romf (bk, _) = (*) <$> bk <*> (signal $ romf 0)

mf ::
  (KnownNat n, KnownNat n1, KnownNat n2)
  => (Vec n RomFunction, SNat n1, SNat n2)
  -> (Sig1 Word1, Sig1 Bool)
  -> Sig1 Word1
mf (romFunctions, nI, coefN) (bk, dPulse) = (!!) <$> (bundle1 results) <*> curCore
  where

    g (mem, coreIndex) = initCore (mem, curCore .==. coreIndex)
    results = map g $ zip romFunctions (iterateI (+1) 0)

    initCore (romF, readSel) = (coref romF) (bk, readSel)

    read = dPulse

    cores = length results
    curCore :: Sig1 (Unsigned 1)
    curCore = regEn1 0 read (curCore + 1)

romF1 :: RomFunction
romF1 addr = 0
romF2 :: RomFunction
romF2 addr = 1

topEntity = mf ((romF1 :> romF2 :> Nil), d2, d8)

testInput :: (Sig1 Word1, Sig1 Bool)
testInput = unbundle' system50 (stimuliGenerator' system50 ((5,True):>(4,False):>(2,True):>Nil))

expectedOutput :: Sig1 Word1 -> Sig1 Bool
expectedOutput = outputVerifier' system50 (0:>4:>2:>0:>2:>Nil)
