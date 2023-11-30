{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -fforce-recomp #-}
module XpmTestCommon where

import Clash.Explicit.Prelude
import Language.Haskell.TH (runIO)
import System.Random


createDomain vXilinxSystem{vName="D3",  vPeriod=hzToPeriod 30e6}
createDomain vXilinxSystem{vName="D5",  vPeriod=hzToPeriod 50e6}
createDomain vXilinxSystem{vName="D10", vPeriod=hzToPeriod 100e6}
createDomain vXilinxSystem{vName="D11", vPeriod=hzToPeriod 110e6}

randomSeed :: Int
randomSeed = $(runIO (randomIO @Int) >>= lift)

genTestData :: forall dom a z. (KnownDomain dom, BitPack a, (BitSize a + z) ~ 64) => Int -> Clock dom -> Signal dom a
genTestData seed clk = (unpack . truncateB @BitVector @(BitSize a) . pack) <$> out
 where
  (out,gen) = unbundle $ genWord64 <$> delay clk enableGen (mkStdGen seed) gen

-- dummy implementation
instance NFDataX StdGen where
  deepErrorX = errorX
  hasUndefined = const False
  ensureSpine = id
  rnfX = const ()
