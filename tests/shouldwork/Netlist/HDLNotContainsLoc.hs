{-# LANGUAGE DataKinds #-}

module HDLNotContainsLoc where

import Clash.Prelude

import qualified Prelude as P
import Data.List (isInfixOf)
import System.Environment (getArgs)
import System.FilePath ((</>), takeDirectory)

topEntity :: Maybe Int -> Int
topEntity x = case x of Nothing -> 0; Just x -> x

assertNotIn :: String -> String -> IO ()
assertNotIn needle haystack
  | not (needle `isInfixOf` haystack)
  = return ()
  | otherwise
  = P.error $ P.concat [ "Did not expect:\n\n  ", needle
                       , "\n\nIn:\n\n", haystack ]

mainVHDL :: IO ()
mainVHDL = do
  [topDir] <- getArgs
  content <- readFile (topDir </> show 'topEntity </> "topEntity.vhdl")
  assertNotIn "HDLNotContainsLoc.hs:" content

mainVerilog :: IO ()
mainVerilog = do
  [topDir] <- getArgs
  content <- readFile (topDir </> show 'topEntity </> "topEntity.v")
  assertNotIn "HDLNotContainsLoc.hs:" content

mainSystemVerilog :: IO ()
mainSystemVerilog = do
  [topDir] <- getArgs
  content <- readFile (topDir </> show 'topEntity </> "topEntity.sv")
  assertNotIn "HDLNotContainsLoc.hs:" content
