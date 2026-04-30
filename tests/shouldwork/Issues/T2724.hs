{-# LANGUAGE TemplateHaskell #-}

module T2724 where

import Clash.Annotations.BitRepresentation
import Clash.Explicit.Testbench
import Clash.Prelude

-- From #2724: DataTag on a CustomSum type crashes the Verilog backend.
data Animal = Turtle | Wombat | Bear
    deriving (Generic, NFDataX, ShowX, Eq)

{-# ANN module (DataReprAnn
                $(liftQ [t|Animal|])
                2
                [ ConstrRepr 'Turtle 0b11 0b00 []
                , ConstrRepr 'Wombat 0b11 0b01 []
                , ConstrRepr 'Bear   0b11 0b10 []
                ]) #-}

topEntity :: Signal System Animal -> Signal System Bool
topEntity = fmap (== Bear)
{-# OPAQUE topEntity #-}

testBench :: Signal System Bool
testBench = done
 where
  testInputs = stimuliGenerator clk rst (Turtle :> Wombat :> Bear :> Nil)
  expectedOutputs =
    outputVerifier' clk rst (False :> False :> True :> Nil)
  done = expectedOutputs (topEntity testInputs)
  clk = tbSystemClockGen (not <$> done)
  rst = systemResetGen
{-# OPAQUE testBench #-}
