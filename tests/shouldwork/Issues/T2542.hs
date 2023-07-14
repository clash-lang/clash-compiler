module T2542 where

import qualified Prelude as P
import           Data.List (isInfixOf)
import           System.Environment (getArgs)
import           System.FilePath ((</>), takeDirectory)

import Clash.Prelude

topEntity :: (Index 2, Index 2)
topEntity = case reverse (indicesI @2) of
  (a `Cons` b `Cons` Nil) -> (a,b)

assertIn :: String -> String -> IO ()
assertIn needle haystack
  | needle `isInfixOf` haystack = return ()
  | otherwise = P.error $ mconcat [ "Expected:\n\n  ", needle
                                  , "\n\nIn:\n\n", haystack ]

mainVHDL :: IO ()
mainVHDL = do
  [topDir] <- getArgs
  content <- readFile (topDir </> show 'topEntity </> "topEntity.vhdl")

  assertIn "to_unsigned(1,1)" content
  assertIn "to_unsigned(0,1)" content
