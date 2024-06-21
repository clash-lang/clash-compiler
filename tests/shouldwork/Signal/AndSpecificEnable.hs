{-# LANGUAGE TemplateHaskell #-}

module AndSpecificEnable where

import Clash.Prelude

import qualified Clash.Explicit.Prelude as E
import Clash.Explicit.Testbench

createDomain vSystem{vName="SystemSlow", vPeriod=2 * vPeriod vSystem}

topEntity
  :: Clock System
  -> Clock SystemSlow
  -> Reset System
  -> Reset SystemSlow
  -> Enable System
  -> Enable SystemSlow
  -> Signal SystemSlow Bool
  -> Signal System (Unsigned 8)
  -> Signal SystemSlow (Unsigned 8)
  -> ( Signal System (Unsigned 8)
       -- ^ Only reacts to @Enable System@
     , Signal SystemSlow ( Unsigned 8
                           -- ^ Only reacts to @Enable SystemSlow@
                         , Unsigned 8
                           -- ^ Also reacts to @Signal SystemSlow Bool@
                         )
     )
topEntity clk1 clk2 rst1 rst2 en1 en2 =
    withSpecificClockResetEnable clk1 rst1 en1
  $ withSpecificClockResetEnable clk2 rst2 en2 f

f :: forall dom1 dom2
   . ( HiddenClockResetEnable dom1
     , HiddenClockResetEnable dom2)
  => Signal dom2 Bool
  -> Signal dom1 (Unsigned 8)
  -> Signal dom2 (Unsigned 8)
  -> (Signal dom1 (Unsigned 8), Signal dom2 (Unsigned 8, Unsigned 8))
f enM i1 i2 = (o1, o2)
 where
  (o1, o2_2) = andSpecificEnable @dom2 enM $ f0 i1 i2
  o2_1 = register 0 i2
  o2 = bundle (o2_1, o2_2)

f0
  :: ( HiddenClockResetEnable dom1
     , HiddenClockResetEnable dom2)
  => Signal dom1 (Unsigned 8)
  -> Signal dom2 (Unsigned 8)
  -> (Signal dom1 (Unsigned 8), Signal dom2 (Unsigned 8))
f0 i1 i2 = (register 0 i1, register 0 i2)

testBench
  :: Signal System Bool
testBench = done
 where
  -- Enable System
  en1 = True :> False :> True :> Nil
  -- Enable SystemSlow
  en2 = True :> True :> False :> True :> True  :> True :> False :> True :> Nil
  -- Signal SystemSlow Bool
  enM = True :> True :> False :> True :> False :> True :> Nil
  en1_0 = toEnable $ stimuliGenerator clk1 rst1 en1
  en2_0 = toEnable $ stimuliGenerator clk2 rst2 en2
  enM0 = stimuliGenerator clk2 rst2 enM
  inp1 = E.delay clk1 enableGen 0 $ inp1 + 1
  inp2 = E.delay clk2 enableGen 0 $ inp2 + 1
  (o1, o2) = topEntity clk1 clk2 rst1 rst2 en1_0 en2_0 enM0 inp1 inp2
  checked = bundle (o1, E.unsafeSynchronizer clk2 clk1 o2)
  expected1 = 0 :> 1 :> 1 :> $(listToVecTH [3 :: Unsigned 8 .. 17])
  expected2 =
       (0, 0) :> (1, 1) :> (2, 2) :> (2, 2) :> (4, 4) :>  (5, 4) :> (6, 6)
    :> (6, 6) :> (8, 8) :> Nil
  expected = zip expected1 $ concatMap (replicate d2) expected2
  done = outputVerifier' clk1 rst1 expected checked
  (clk1, clk2) = biTbClockGen (not <$> done)
  rst1 = resetGen
  rst2 = resetGen
