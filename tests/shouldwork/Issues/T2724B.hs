{-# LANGUAGE TemplateHaskell #-}

module T2724B where

import Clash.Annotations.BitRepresentation
import Clash.Explicit.Testbench
import Clash.Prelude

-- Non-monotone variant of T2724: the user-defined bit patterns do not match
-- the GHC tag order, so a naive bit-slice fix would silently produce wrong
-- results.
data Animal = Turtle | Wombat | Bear
    deriving (Generic, NFDataX, ShowX, Eq)

{-# ANN module (DataReprAnn
                $(liftQ [t|Animal|])
                2
                [ ConstrRepr 'Turtle 0b11 0b10 []
                , ConstrRepr 'Wombat 0b11 0b00 []
                , ConstrRepr 'Bear   0b11 0b01 []
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
