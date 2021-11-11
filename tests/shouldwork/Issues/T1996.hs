module T1996 where

import qualified Prelude as P
import           Data.List (isInfixOf)
import           System.Environment (getArgs)
import           System.FilePath ((</>), takeDirectory)

import           Clash.Prelude

topEntity :: (Int, Int) -> (Int, Int)
topEntity = id

assertNotIn :: String -> String -> IO ()
assertNotIn needle haystack
  | needle `isInfixOf` haystack =
      P.error $ P.concat [ "Did not expect:\n\n  ", needle
                         , "\n\nIn:\n\n", haystack ]
  | otherwise = return ()

mainVHDL :: IO ()
mainVHDL = do
  [topDir] <- getArgs
  content  <- readFile (topDir </> show 'topEntity </> "T1996_topEntity_types.vhdl")

  assertNotIn "T1996_topEntity_types." content
