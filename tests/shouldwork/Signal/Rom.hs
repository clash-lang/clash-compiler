module Rom where

import Clash.Prelude
import qualified Data.List as L

zeroAt0
  :: HasClockReset domain gated synchronous
  => Signal domain (Unsigned 8,Unsigned 8)
  -> Signal domain (Unsigned 8,Unsigned 8)
zeroAt0 a = mux en a (bundle (0,0))
  where
    en = register False (pure True)

topEntity
  :: SystemClockReset
  => Signal System (Unsigned 8)
  -> Signal System (Unsigned 8,Unsigned 8)
topEntity rd = zeroAt0 dout
  where
    dout = rom $(listToVecTH $ L.map (\x -> (x,x))  [0::Unsigned 8,1,2,3,4,5,6,7,8])  rd
{-# NOINLINE topEntity #-}

testBench :: Signal System Bool
testBench = done'
  where
    testInput      = register 0 (testInput + 1)
    expectedOutput = outputVerifier $(listToVecTH $ L.map (\x -> (x,x)) [0::Unsigned 8,0,1,2,3,4,5,6,7,8])
    done           = expectedOutput (topEntity testInput)
    done'          = withClockReset (tbSystemClockGen (not <$> done')) systemResetGen done
