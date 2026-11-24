-- | When Clash specializes a function, the binders of the specialized function
-- get the names of the lambdas in the body of the original function. These
-- names end up as port names. Lambdas following a let should be found as well,
-- rather than falling back to made-up names like @c$pTS0@.
module SpecializeLetPortNames where

import qualified Prelude as P
import Data.List (isInfixOf)
import System.Environment (getArgs)
import System.FilePath ((</>))

import Clash.Explicit.Prelude

acc ::
  Int -> Clock System -> Reset System -> Enable System ->
  Unsigned 8 -> Signal System (Unsigned 8) -> Signal System (Unsigned 8)
acc n = let start = fromIntegral (n + 1) in \clk rst en k x ->
  let s = register clk rst en start (s + x * pure k) in s
{-# OPAQUE acc #-}

topEntity ::
  Clock System -> Reset System -> Enable System ->
  Signal System (Unsigned 8) -> Signal System (Unsigned 8)
topEntity clk rst en x = acc 0 clk rst en 3 x + acc 0 clk rst en 5 x
{-# OPAQUE topEntity #-}

assertIn :: String -> String -> IO ()
assertIn needle haystack
  | needle `isInfixOf` haystack = return ()
  | otherwise =
      P.error $ P.concat ["Expected:\n\n  ", needle, "\n\nIn:\n\n", haystack]

assertNotIn :: String -> String -> IO ()
assertNotIn needle haystack
  | needle `isInfixOf` haystack =
      P.error $ P.concat ["Unexpected:\n\n  ", needle, "\n\nIn:\n\n", haystack]
  | otherwise = return ()

mainVerilog :: IO ()
mainVerilog = do
  [topDir] <- getArgs
  P.mapM_ (check topDir) ["acc.v", "acc_0.v"]
 where
  check topDir file = do
    content <- readFile (topDir </> show 'topEntity </> file)
    assertNotIn "pTS" content
    assertIn "clk // clock" content
    assertIn "rst // reset" content
    assertIn "en // enable" content
