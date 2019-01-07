module TRepeat where

import qualified Prelude as P
import Data.List (isInfixOf)
import System.Environment (getArgs)
import System.FilePath ((</>), takeDirectory)

import Clash.Prelude
import Clash.Explicit.Testbench

data A = A Int Int deriving (Eq, Generic, ShowX)
data B = B Int Int Int deriving (Eq, Generic, ShowX)

topEntity :: Signal System (RTree 2 Bool)
topEntity = pure (trepeat True)

-- Simulation test
testBench :: Signal System Bool
testBench = done
  where
    expectedOutput = outputVerifier clk rst ($([| v2t (replicate d4 True) |]) :> Nil)
    done           = expectedOutput topEntity
    clk            = tbSystemClockGen (not <$> done)
    rst            = systemResetGen
