{-|
Top entities are compiled concurrently, but share the set of names they claim
in the HDL. Clash used to let them claim names in whichever order they finished
normalization, so the names in the HDL depended on timing.

'testBench' instantiates 'topEntity', so 'topEntity' comes first in the order in
which top entities are compiled. Both use component 'f', so 'topEntity' must get
@f@ and 'testBench' @f_0@. Before the fix 'testBench', which normalizes much
quicker, won @f@.
-}
module ConcurrentNames where

import Clash.Explicit.Prelude
import Clash.Explicit.Testbench

import Control.Monad (unless)
import System.Directory (doesFileExist)
import System.Environment (getArgs)
import System.FilePath ((</>), (<.>))
import qualified Prelude as P

f :: Unsigned 8 -> Unsigned 8
f x = x + 1
{-# OPAQUE f #-}

-- | Takes a while to normalize, because Clash unrolls the vector of functions
topEntity :: Vec 24 (Unsigned 8) -> Unsigned 8
topEntity xs = foldr (.) id (map (\x y -> f (x + y)) xs) 0
{-# OPAQUE topEntity #-}

testBench :: Signal System (Unsigned 8)
testBench = o
 where
  o = register clk systemResetGen enableGen 0 (fmap (\x -> topEntity (repeat x) + f x) o)
  clk = tbSystemClockGen (pure True)
{-# OPAQUE testBench #-}

assertExists :: FilePath -> IO ()
assertExists path = do
  exists <- doesFileExist path
  unless exists (P.error ("Expected file to exist: " <> path))

check :: String -> IO ()
check ext = do
  [topDir] <- getArgs
  assertExists (topDir </> show 'topEntity </> "f" <.> ext)
  assertExists (topDir </> show 'testBench </> "f_0" <.> ext)

mainVHDL :: IO ()
mainVHDL = check "vhdl"

mainVerilog :: IO ()
mainVerilog = check "v"

mainSystemVerilog :: IO ()
mainSystemVerilog = check "sv"
