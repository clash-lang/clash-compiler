{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}

module Indexed where

import Type
import Clash.Annotations.BitRepresentation.Deriving
import Clash.Prelude
import Clash.Prelude.Testbench

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
testBench = done'
  where
    testInput = stimuliGenerator $ MkTB True
                                :> MkTB False
                                :> MkTA (True :> False :> Nil) 2
                                :> MkTA (True :> True :> Nil) 2
                                :> Nil

    expectedOutput = outputVerifier' $ True
                                   :> False
                                   :> False
                                   :> True
                                   :> Nil

    done  = expectedOutput (topEntity testInput)
    done' =
      withClockResetEnable
        (tbSystemClockGen (not <$> done'))
        systemResetGen
        enableGen
        done
