-- | Fields of SystemVerilog structs are named after record fields. If a record
-- field isn't a valid SystemVerilog identifier, all fields of the struct are
-- named after the type, field index, and record field instead. Fields of tuples
-- are named after the type and field index.
module StructFieldNames where

import qualified Prelude as P
import Data.List (isInfixOf)
import System.Environment (getArgs)
import System.FilePath ((</>))

import Clash.Explicit.Prelude

data Plain = Plain { busy :: Bool, progress :: Unsigned 8 }
  deriving (Generic, NFDataX)

-- 'input' is a SystemVerilog keyword
data Keyword = Keyword { input :: Bool, ready :: Bool }
  deriving (Generic, NFDataX)

-- 'count'' isn't a SystemVerilog identifier
data Primed = Primed { count' :: Unsigned 4, valid :: Bool }
  deriving (Generic, NFDataX)

topEntity ::
  Clock System -> Reset System -> Enable System ->
  Signal System Plain ->
  Signal System Keyword ->
  Signal System Primed ->
  Signal System (Bool, Unsigned 4) ->
  Signal System (Plain, Keyword, Primed, (Bool, Unsigned 4))
topEntity clk rst en a b c d = bundle (reg a, reg b, reg c, reg d)
 where
  reg :: NFDataX x => Signal System x -> Signal System x
  reg = delay clk en (deepErrorX "no initial value")

assertIn :: String -> String -> IO ()
assertIn needle haystack
  | needle `isInfixOf` haystack = return ()
  | otherwise =
      P.error $ P.concat ["Expected:\n\n  ", needle, "\n\nIn:\n\n", haystack]

mainSystemVerilog :: IO ()
mainSystemVerilog = do
  [topDir] <- getArgs
  types <- readFile (topDir </> show 'topEntity </> "StructFieldNames_topEntity_types.sv")
  assertIn "logic busy;" types
  assertIn "logic [7:0] progress;" types
  assertIn "logic Keyword_sel0_input;" types
  assertIn "logic Keyword_sel1_ready;" types
  assertIn "logic [3:0] Primed_sel0_count_;" types
  assertIn "logic Primed_sel1_valid;" types
  assertIn "logic Tuple2_sel0;" types
  assertIn "logic [3:0] Tuple2_sel1;" types
