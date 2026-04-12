{-|
Copyright  :  (C) 2021-2022, QBayLogic B.V.,
                  2022     , Google Inc.,
License    :  BSD2 (see the file LICENSE)
Maintainer :  QBayLogic B.V. <devops@qbaylogic.com>
-}

{-# OPTIONS_GHC -fconstraint-solver-iterations=10 -Wall -Werror #-}

module T3185 where

import Clash.Prelude hiding (Ordering(..))
import Clash.Explicit.Testbench

import T3185.Annotations
import T3185.TH
import T3185.Types

import qualified Clash.Explicit.Prelude as CEP

topEntity :: Bool
topEntity = False

playSampleRom
  :: forall n a dom
   . ( KnownDomain dom
     , KnownNat n
     , BitPack a
     , 1 <= n
     )
  => Clock dom
  -> Reset dom
  -> MemBlob n (BitSize a)
  -> (Signal dom Bool, Signal dom a)
playSampleRom clk rst content = (done, out)
 where
  out = unpack . asyncRomBlob content <$> cnt
  done = CEP.register clk rst enableGen False $ (== maxBound) <$> cnt
  cnt :: Signal dom (Index n)
  cnt = CEP.register clk rst enableGen 0 $ satSucc SatBound <$> cnt

basicRomTB
  :: forall d n x y z
   . ( KnownNat n
     , KnownNat d
     , BitPack x
     , BitPack y
     , Eq z, ShowX z, BitPack z
     , 1 <= n
     )
  => (   Clock XilinxSystem
      -> DSignal XilinxSystem 0 x
      -> DSignal XilinxSystem 0 y
      -> DSignal XilinxSystem d z
     )
  -> z
  -> MemBlob n (BitSize (x, y, z))
  -> Signal XilinxSystem Bool
basicRomTB comp resDef sampleBlob = done
 where
  (done0, samples) = playSampleRom clk rst sampleBlob
  (inputX, inputY, expectedOutput) = unbundle samples
  -- Only assert while not finished
  done = mux done0 done0
    $ assert clk rst "basicRomTB" out expectedOutput
             done0
  out =
      ignoreFor clk rst en (SNat @d) resDef
    . toSignal $ comp clk (fromSignal inputX) (fromSignal inputY)
  clk = tbClockGen (not <$> done)
  rst = resetGen
  en = enableGen
{-# INLINE basicRomTB #-}

compareBasic
  :: Clock XilinxSystem
  -> DSignal XilinxSystem 0 Float
  -> DSignal XilinxSystem 0 Float
  -> DSignal XilinxSystem 2 Ordering
compareBasic !_clk !_x !_y = pure LT
{-# OPAQUE compareBasic #-}
{-# ANN compareBasic (binaryTopAnn "compareBasic") #-}

testBench  :: Signal XilinxSystem Bool
testBench  =
  basicRomTB compareBasic LT $(memBlobTH Nothing compareBasicSamples)
{-# ANN testBench (TestBench 'compareBasic) #-}
