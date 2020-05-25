{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Indexed where

import Type
import Clash.Annotations.BitRepresentation.Deriving
import Clash.Prelude
import Clash.Explicit.Testbench

deriveDefaultAnnotation [t| WithVector |]

topEntity' :: WithVector -> Bool
topEntity' (MkTA xs _) = xs !! 1
topEntity' (MkTB b) = b

topEntity
  :: Signal System WithVector
  -> Signal System Bool
topEntity = fmap topEntity'
{-# NOINLINE topEntity #-}

testBench :: Signal System Bool
testBench = done
  where
    testInput = stimuliGenerator clk rst $ MkTB True
                                :> MkTB False
                                :> MkTA (True :> False :> Nil) 2
                                :> MkTA (True :> True :> Nil) 2
                                :> Nil

    expectedOutput = outputVerifier' clk rst $ True
                                   :> False
                                   :> False
                                   :> True
                                   :> Nil

    done = expectedOutput (topEntity testInput)
    clk  = tbSystemClockGen (not <$> done)
    rst  = systemResetGen
