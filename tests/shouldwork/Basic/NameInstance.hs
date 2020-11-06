module NameInstance where

import qualified Prelude as P
import Data.List (isInfixOf)
import System.Environment (getArgs)
import System.FilePath ((</>), takeDirectory)

import Clash.Prelude

topEntity :: Bool -> Bool -> (Bool,Bool)
topEntity = suffixName @"after" $ setName @"foo" $ prefixName @"before" f

f :: Bool -> Bool -> (Bool,Bool)
f x y = (y,x)
{-# NOINLINE f #-}

-- File content test
assertIn :: String -> String -> IO ()
assertIn needle haystack
  | needle `isInfixOf` haystack = return ()
  | otherwise                   = P.error $ P.concat [ "Expected:\n\n  ", needle
                                                     , "\n\nIn:\n\n", haystack ]

mainSystemVerilog :: IO ()
mainSystemVerilog = do
  [topDir] <- getArgs
  content <- readFile (takeDirectory topDir </> "topEntity.sv")
  assertIn "before_foo_after" content

mainVerilog :: IO ()
mainVerilog = do
  [topDir] <- getArgs
  content <- readFile (takeDirectory topDir </> "topEntity.v")
  assertIn "before_foo_after" content

mainVHDL :: IO ()
mainVHDL = do
  [topDir] <- getArgs
  content <- readFile (takeDirectory topDir </> "topEntity.vhdl")
  assertIn "before_foo_after" content
