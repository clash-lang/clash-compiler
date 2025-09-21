{-# LANGUAGE CPP #-}

module T3011 where

import Clash.Prelude hiding ((^))
import Prelude ((^))

import qualified Prelude as P
import Data.List (isInfixOf)
import System.Environment (getArgs)
import System.FilePath ((</>), takeDirectory)

opaquePow1 :: Integer -> Integer -> Integer
opaquePow1 a b = a ^ b
{-# CLASH_OPAQUE opaquePow1 #-}

opaquePow2 :: Integer -> Int -> Integer
opaquePow2 a b = a ^ b
{-# CLASH_OPAQUE opaquePow2 #-}

opaquePow3 :: Int -> Int -> Int
opaquePow3 a b = a ^ b
{-# CLASH_OPAQUE opaquePow3 #-}


topEntity :: (Integer, Integer, Int)
topEntity =
  ( opaquePow1 2 11
  , opaquePow2 2 12
  , opaquePow3 2 13
  )
{-# CLASH_OPAQUE topEntity #-}

-- File content test
assertIn :: String -> String -> IO ()
assertIn needle haystack
  | needle `isInfixOf` haystack = return ()
  | otherwise                   = P.error $ P.concat [ "Expected:\n\n  ", needle
                                                     , "\n\nIn:\n\n", haystack ]

check :: FilePath -> IO ()
check fileName = do
  [topDir] <- getArgs
  content <- readFile (topDir </> show 'topEntity </> fileName)
  assertIn "2048" content
  assertIn "4096" content
  assertIn "8192" content


mainVHDL :: IO ()
mainVHDL = check "topEntity.vhdl"

mainVerilog :: IO ()
mainVerilog = check "topEntity.v"

mainSystemVerilog :: IO ()
mainSystemVerilog = check "topEntity.sv"
