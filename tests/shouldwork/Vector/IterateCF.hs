module IterateCF where

import qualified Prelude as P

import Clash.Prelude
import Clash.Explicit.Testbench
import Data.List (isInfixOf)
import System.Environment (getArgs)
import System.FilePath ((</>), takeDirectory)

topEntity :: (Bool, Bool)
topEntity = (a1, a2)
 where
  test :: Vec 10 Int
  test = iterateI succ 255

  a1 = case test of
    (s :> _) -> s == 255
    _ -> False

  a2 = case test of
    (_ :> _ :> s :> _) -> s == 257
    _ -> False
{-# NOINLINE topEntity #-}

assertNotIn :: String -> String -> IO ()
assertNotIn needle haystack
  | needle `isInfixOf` haystack = P.error $ P.concat [ "Did not expect:\n\n  ", needle
                                                     , "\n\nIn:\n\n", haystack ]
  | otherwise = pure ()

mainVHDL :: IO ()
mainVHDL = do
  [topDir] <- getArgs
  content  <- readFile (takeDirectory topDir </> "topEntity.vhdl")

  assertNotIn "255" content
  assertNotIn "256" content
  assertNotIn "257" content
