{-# LANGUAGE NamedFieldPuns #-}

-- | Signals Clash introduces get names derived from the source:
--
--   * Arguments of a function get the function name and parameter name
--   * Arguments of a record constructor get the field name
--   * Case alternatives get the constructor they match on
--   * Pattern bindings get the names of the variables in the pattern
--   * Variables GHC made up get the name of the parameter they are passed to
--
-- And struct fields in SystemVerilog are named after record fields. See
-- "SplitRegister" for splitting the state register per field.
module DerivedNames where

import qualified Prelude as P
import Data.List (isInfixOf)
import System.Environment (getArgs)
import System.FilePath ((</>))

import Clash.Prelude

data Mode = Idle | Counting | Done
  deriving (Generic, NFDataX, Eq, Show)

data State = State
  { mode :: Mode
  , counter :: Unsigned 8
  , total :: Unsigned 16
  } deriving (Generic, NFDataX, Show)

data Status = Status
  { busy :: Bool
  , progress :: Unsigned 8
  } deriving (Generic, NFDataX, Show)

controller :: State -> Maybe (Unsigned 8) -> (State, Status)
controller s@State{mode, counter, total} request = (s', status)
 where
  (nextMode, nextCounter) = case mode of
    Idle -> case request of
      Just n -> (Counting, n)
      Nothing -> (Idle, counter)
    Counting
      | counter == 0 -> (Done, 0)
      | otherwise -> (Counting, counter - 1)
    Done -> (Idle, counter)

  s' = s { mode = nextMode
         , counter = nextCounter
         , total = if mode == Done then total + 1 else total }

  status = Status
    { busy = mode /= Idle
    , progress = if mode == Counting then counter else maxBound }

topEntity ::
  Clock System -> Reset System -> Enable System ->
  Signal System (Maybe (Unsigned 8)) -> Signal System Status
topEntity = exposeClockResetEnable (mealy controller (State Idle 0 0))

assertIn :: String -> String -> IO ()
assertIn needle haystack
  | needle `isInfixOf` haystack = return ()
  | otherwise =
      P.error $ P.concat ["Expected:\n\n  ", needle, "\n\nIn:\n\n", haystack]

readHdl :: FilePath -> IO String
readHdl file = do
  [topDir] <- getArgs
  readFile (topDir </> show 'topEntity </> file)

mainVerilog :: IO ()
mainVerilog = do
  content <- readHdl "topEntity.v"
  -- Port named after the parameter of 'controller', rather than GHC's 'eta'
  assertIn "input wire [8:0] request" content
  -- The state register is the argument 's' of 'controller'
  assertIn "reg [25:0] c$controller_s " content
  -- Record fields
  assertIn "reg  c$busy;" content
  assertIn "reg [7:0] c$progress;" content
  assertIn "reg [15:0] c$total;" content
  -- Pattern binding
  assertIn "reg [9:0] nextMode_nextCounter;" content
  -- Case alternatives
  assertIn "wire [9:0] c$nextMode_nextCounter_case_Idle;" content
  assertIn "wire [9:0] c$nextMode_nextCounter_case_Counting;" content

mainSystemVerilog :: IO ()
mainSystemVerilog = do
  content <- readHdl "topEntity.sv"
  assertIn "c$controller_s.counter" content
  assertIn "nextMode_nextCounter" content

  types <- readHdl "DerivedNames_topEntity_types.sv"
  assertIn "logic [7:0] counter;" types
  assertIn "logic busy;" types
  assertIn "logic [7:0] progress;" types

mainVHDL :: IO ()
mainVHDL = do
  content <- readHdl "topEntity.vhdl"
  assertIn "request" content
  assertIn "\\c$controller_s\\" content
  assertIn "nextMode_nextCounter" content
  assertIn "\\c$nextMode_nextCounter_case_Idle\\" content
