-- This tests the special, verlog only, code-path for index_int,
-- which generates special verilog when the index is constant.
module IndexInt2 where

import qualified Prelude as P
import Control.Monad (when)
import Data.List (isInfixOf)
import System.Environment (getArgs)
import System.FilePath ((</>), takeDirectory)

import Clash.Prelude
import Clash.Explicit.Testbench

import Clash.Netlist.Types

topEntity :: Vec 4 (Unsigned 4) -> Signed 8 -> (Unsigned 4,Unsigned 4,Unsigned 4)
topEntity xs ix0 =
  ( xs !! ix0    -- non-constant index
  , xs !! ix1    -- constant index, vec is a Var
  , (tail xs :< 0xe) !! ix1 -- constant index, vec is an expression
  )
  where
    ix1 :: Signed 8
    ix1 = fold (+) (1 :> (-1) :> 1 :> Nil)
{-# NOINLINE topEntity #-}

testBench :: Signal System Bool
testBench = done
 where
  testVecs       = stimuliGenerator clk rst (map unpack $ 0x0010 :> 0x0100 :> 0x1000 :> Nil)
  testIxs        = stimuliGenerator clk rst (2 :> 1 :> 0 :> Nil)
  output         = map unpack $ 0x101 :> 0x110 :> 0x100 :> Nil
  expectedOutput = outputVerifier' clk rst output
  done           = expectedOutput (topEntity <$> testVecs <*> testIxs)
  clk            = tbSystemClockGen (not <$> done)
  rst            = systemResetGen


mainVerilog :: IO ()
mainVerilog = do
  [topDir] <- getArgs
  content <- readFile (topDir </> show 'topEntity </> "topEntity.v")
  let indexBegins = P.length $ filter (isInfixOf " index begin") $ lines content
  when (indexBegins /= 1) $
    error ("Expected 1 normal index blackbox, but found " <> show indexBegins)
