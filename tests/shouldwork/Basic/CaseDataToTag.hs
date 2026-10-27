{-# LANGUAGE MagicHash #-}

-- | Case-expressions on the constructor tag of a value, such as the ones GHC
-- derives '==' with, become case-expressions on the value itself. The HDL then
-- doesn't hold the tag in a signal as wide as an 'Int'.
module CaseDataToTag where

import qualified Prelude as P
import Data.List (isPrefixOf, tails)
import GHC.Exts (Int(I#), dataToTag#)
import System.Environment (getArgs)
import System.FilePath ((</>))

import Clash.Explicit.Testbench
import Clash.Prelude

data Mode = Idle | Counting | Done | Halted
  deriving (Generic, NFDataX, Eq, Show)

data Op = Nop | Add (Unsigned 8) | Halt
  deriving (Generic, NFDataX, Show)

-- | Derived '==' and '/=' compare constructor tags
modeChecks :: Mode -> (Bool, Bool, Bool)
modeChecks m = (m == Idle, m /= Done, m == Halted)

-- | Tags of constructors with fields
opTag :: Op -> Unsigned 8
opTag op = case I# (dataToTag# op) of
  1 -> 10
  2 -> 20
  _ -> 0

topEntity :: Mode -> Op -> ((Bool, Bool, Bool), Unsigned 8)
topEntity m op = (modeChecks m, opTag op)
{-# OPAQUE topEntity #-}

testBench :: Signal System Bool
testBench = done
 where
  testInput = stimuliGenerator clk rst
    ((Idle, Nop) :> (Counting, Add 3) :> (Done, Halt) :> (Halted, Nop) :> Nil)
  expectedOutput = outputVerifier' clk rst
    (    ((True,  True,  False), 0)
      :> ((False, True,  False), 10)
      :> ((False, False, False), 20)
      :> ((False, True,  True),  0)
      :> Nil )
  done = expectedOutput (uncurry topEntity <$> testInput)
  clk = tbSystemClockGen (not <$> done)
  rst = systemResetGen

countOccurrences :: String -> String -> Int
countOccurrences needle = P.length . P.filter (needle `isPrefixOf`) . tails

mainVerilog :: IO ()
mainVerilog = do
  [topDir] <- getArgs
  content <- readFile (topDir </> show 'topEntity </> "topEntity.v")
  -- All three comparisons of 'Mode' are case-expressions on 'm' itself, and
  -- 'opTag' is a case-expression on the constructor bits of 'op'
  let expect needle n
        | countOccurrences needle content == n = return ()
        | otherwise = P.error $ P.concat
            [ "Expected ", show n, " times ", show needle, " in:\n\n", content ]
  expect "case(m)" 3
  expect "case(op[9:8])" 1
