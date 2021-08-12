module T1881 where

import qualified Prelude as P
import Data.List (isInfixOf)
import System.Environment (getArgs)
import System.FilePath ((</>), takeDirectory)

import Clash.Prelude

topEntity :: Bool -> Bool -> (Bool, Bool, Bool)
topEntity a b = (a && b, a || b, not a)

assertIn :: String -> String -> IO ()
assertIn needle haystack
  | needle `isInfixOf` haystack = return ()
  | otherwise = P.error $ mconcat [ "Expected:\n\n  ", needle
                                  , "\n\nIn:\n\n", haystack ]


mainVHDL :: IO ()
mainVHDL = do
  [topDir] <- getArgs
  content <- readFile (topDir </> show 'topEntity </> "topEntity.vhdl")

  assertIn "and" content
  assertIn "or" content
  assertIn "not" content

mainVerilog :: IO ()
mainVerilog = do
  [topDir] <- getArgs
  content <- readFile (topDir </> show 'topEntity </> "topEntity.v")

  -- Each of these operators should appear, instead of the options being
  -- expanded out into multiplexers.
  assertIn "&" content
  assertIn "|" content
  assertIn "~" content

mainSystemVerilog :: IO ()
mainSystemVerilog = do
  [topDir] <- getArgs
  content <- readFile (topDir </> show 'topEntity </> "topEntity.sv")

  -- Each of these operators should appear, instead of the options being
  -- expanded out into multiplexers.
  assertIn "&" content
  assertIn "|" content
  assertIn "~" content
